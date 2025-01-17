## The backend for the VM. The core code-generation is done by `vmgen`; the
## linking bits and artifact creation are implemented here
##
## Executable generation happens in roughly the following steps:
## 1. Collect all modules and their top-level statements via the `passes`
##  interface. This is a pure collection step, no further processing is done
## 2. Generate all module init procedures (i.e. code for all top-level
##  statements)
## 3. Iteratively generate code for all alive routines (excluding `method`s)
## 4. Generate the main procedure
## 5. Pack up all required data into `PackedEnv` and write it to the output
##  file
##
## Similiar to the C and JS backend, dead-code-elimination (DCE) happens as a
## side-effect of how the routines are processed

import
  std/[
    tables
  ],
  compiler/ast/[
    ast,
    ast_types,
    lineinfos,
    astalgo, # for `getModule`
    reports
  ],
  compiler/front/[
    msgs,
    options
  ],
  compiler/sem/[
    passes,
    transf
  ],
  compiler/modules/[
    magicsys,
    modulegraphs
  ],
  compiler/vm/[
    packed_env,
    vmaux,
    vmdef,
    vmgen,
    vmobjects,
    vmops,
    vmtypegen
  ],
  experimental/[
    results
  ]

import std/options as stdoptions

type
  CodeFragment = object
    ## The state required for generating code in multiple steps.
    ## `CodeFragment` helps when generating code for multiple procedures in
    ## an interleaved manner.
    prc: PProc
    code: seq[TInstr]
    debug: seq[TLineInfo]

  Module = object
    stmts: seq[PNode] ## top level statements in the order they were parsed
    sym: PSym ## module symbol

    initGlobalsCode: CodeFragment ## the bytecode of `initGlobalsProc`. Each
      ## encountered `{.global.}`'s init statement gets code-gen'ed into the
      ## `initGlobalCode` of the module that owns it
    initGlobalsProc: CodeInfo ## the proc that initializes `{.global.}`
      ## variables
    initProc: CodeInfo ## the module init proc (top-level statements)

  ModuleListRef = ref ModuleList
  ModuleList = object of RootObj
    modules: seq[Module]
    modulesClosed: seq[int] ## indices into `modules` in the order the modules
                            ## were closed. The first closed module comes
                            ## first, then the next, etc.
    moduleMap: Table[int, int] ## module sym-id -> index into `modules`

  ModuleRef = ref object of TPassContext
    ## The pass context for the VM backend. Represents a reference to a
    ## module in the module list
    list: ModuleListRef
    index: int

func growBy[T](x: var seq[T], n: Natural) {.inline.} =
  x.setLen(x.len + n)

iterator cpairs[T](s: seq[T]): (int, lent T) =
  ## Continous pair iterator. Supports `s` growing during iteration
  var i = 0
  while i < s.len:
    yield (i, s[i])
    inc i


proc registerCallbacks(c: var TCtx) =
  ## Registers callbacks for various functions, so that no code is
  ## generated for them and that they can (must) be overridden by the runner

  # the callbacks registered here need to be manually kept in sync with the
  # callbacks registered in ``vmrunner.registerCallbacks``. The runner will
  # complain if there's a mismatch

  template cb(key: string) =
    c.callbackKeys.add(IdentPattern(key))

  registerBasicOps(c)
  registerDebugOps(c)
  registerIoReadOps(c)
  registerIoWriteOps(c)
  registerOsOps(c)
  registerOs2Ops(c)

  # Used by some tests
  cb "stdlib.system.getOccupiedMem"

func initFuncTblEntry(sym: PSym, sig: RoutineSigId, info: CodeInfo): FuncTableEntry =
  FuncTableEntry(sym: sym, sig: sig,
                 kind: ckDefault,
                 start: info.start, regCount: info.regCount.uint16)

proc appendCode(c: var TCtx, f: CodeFragment) =
  ## Copies the code from the fragment to the end of the global code buffer
  c.code.add(f.code)
  c.debug.add(f.debug)

func collectRoutineSyms(ast: PNode, syms: var seq[PSym]) =
  ## Traverses the `ast`, collects all symbols that are of routine kind and
  ## appends them to `syms`
  if ast.kind == nkSym:
    if ast.sym.kind in routineKinds:
      syms.add(ast.sym)

    return

  for i in 0..<ast.safeLen:
    collectRoutineSyms(ast[i], syms)

proc generateTopLevelStmts*(module: var Module, c: var TCtx,
                            config: ConfigRef) =
  ## Generates code for all collected top-level statements of `module` and
  ## compiles the fragments into a single function. The resulting code is
  ## stored in `module.initProc`
  let n = newNodeI(nkEmpty, module.sym.info) # for line information

  c.prc = PProc(sym: module.sym)
  c.prc.regInfo.newSeq(1) # the first register is always the (potentially
                          # non-existant) result

  # start of init proc
  let start = c.code.len

  for x in module.stmts.items:
    let tn = transformExpr(c.graph, c.idgen, c.module, x)
    let r = c.genStmt(tn)

    if unlikely(r.isErr):
      config.localReport(r.takeErr)

  c.gABC(n, opcRet)

  module.initProc = (start: start, regCount: c.prc.regInfo.len)

proc generateCodeForProc(c: var TCtx, s: PSym): VmGenResult =
  let body = transformBody(c.graph, c.idgen, s, cache = false)
  result = genProc(c, s, body)

proc generateGlobalInit(c: var TCtx, f: var CodeFragment, defs: openArray[PNode]) =
  ## Generates and emits code for the given `{.global.}` initialization
  ## statements (`nkIdentDefs` in this case) into `f`
  template swapState() =
    swap(c.code, f.code)
    swap(c.debug, f.debug)
    swap(c.prc, f.prc)

  # In order to generate code into the fragment, the fragment's state is
  # swapped with the `TCtx''s one
  swapState()

  for def in defs.items:
    assert def.kind == nkIdentDefs
    for i in 0..<def.len-2:
      # note: don't transform the expressions here; they already were, during
      # transformation of their owning procs
      let
        asgn = newTreeI(nkAsgn, def[i].info, def[i], def[^1])
        r = genStmt(c, asgn)

      if unlikely(r.isErr):
        c.config.localReport(r.takeErr)

  # Swap back once done
  swapState()

proc generateAliveProcs(c: var TCtx, mlist: var ModuleList) =
  ## Runs code generation for all routines (except methods) directly used
  ## by the routines in `c.codegenInOut.newProcs`, including the routines in
  ## the list itself.
  ##
  ## This function can be called multiple times, but `c.codegenInOut.newProcs`
  ## must only contain not-yet-code-gen'ed routines each time.
  ##
  ## An alive routine is a routine who's symbol is used in: a top-level
  ## statement; another alive routine's body; a ``const``s value

  let start = c.functions.len
  # adjust the function table size. Since `generateAllProcs` may be called
  # multiple times, `setLen` has to be used instead of `newSeq`
  c.functions.setLen(c.codegenInOut.nextProc)

  # `newProcs` can grow during iteration, so `citems` has to be used
  for ri, sym in c.codegenInOut.newProcs.cpairs:
    c.config.internalAssert(sym.kind notin {skMacro, skTemplate}):
      "unexpanded macro or template"
    c.config.internalAssert(sym.kind != skIterator):
      "closure iterator wasn't rejected by vmgen"

    let i = start + ri
    c.functions[i] = c.initProcEntry(sym)

    # don't generate code for a proc that is overridden with a callback:
    if c.functions[i].kind == ckCallback:
      continue

    assert c.codegenInOut.globalDefs.len == 0

    c.module = sym.getModule()
    # code-gen' the routine. This might add new entries to the `newProcs` list
    let r = generateCodeForProc(c, sym)
    if r.isOk:
      fillProcEntry(c.functions[i], r.unsafeGet)
    else:
      c.config.localReport(r.takeErr)

    # `{.global.}` initialization is done here, since the initializer
    # expression might contain references to other functions (which can
    # also contain further globals...)
    if c.codegenInOut.globalDefs.len > 0:
      let mI = mlist.moduleMap[c.module.id]
      generateGlobalInit(c, mlist.modules[mI].initGlobalsCode,
                         c.codegenInOut.globalDefs)

      c.codegenInOut.globalDefs.setLen(0)

    # code-gen might've found new functions, so adjust the function table:
    c.functions.setLen(c.codegenInOut.nextProc)

proc generateEntryProc(c: var TCtx, info: TLineInfo, initProcs: Slice[int],
                       initProcTyp: VmTypeId): CodeInfo =
  ## Generates the entry function and returns it's function table index.
  ## The entry function simply calls all given `initProcs` (ordered from low
  ## to high by their function table index) and then returns the value of the
  ## ``programResult`` global
  let
    n = newNodeI(nkEmpty, info)
    start = c.code.len

  # setup code-gen state. One register for the return value and one as a
  # temporary to hold the init procs
  c.prc = PProc(regInfo: @[RegInfo(), RegInfo()])

  # the entry procedure simply calls all module init procedures
  for idx in initProcs.items:
    c.gABx(n, opcLdNull, 1, initProcTyp.int)
    c.gABx(n, opcWrProc, 1, idx) # `idx` is the function's table index
    c.gABC(n, opcIndCall, 0, 1, 1)

  # load ``programResult`` into the result register and then return
  let prSym = magicsys.getCompilerProc(c.graph, "programResult")
  c.gABx(n, opcLdGlobal, 0, c.symToIndexTbl[prSym.id].int)
  c.gABC(n, opcNodeToReg, 0, 0)
  c.gABC(n, opcRet)

  result = (start: start, regCount: 2)

func addInitProcs(ft: var seq[FuncTableEntry], m: Module, sig: RoutineSigId) =
  ## Appends entries for module `m`'s initialization procs (if any) to the
  ## function table
  # XXX: initializing the globals _before_ top-level statements are
  #      executed violates the spec. Following the spec would cause
  #      behaviour than can be considered more broken and is thus
  #      decided against. The language spec needs to clarify what exactly
  #      is meant by 'initialization' in this context
  if m.initGlobalsProc.start != -1:
    ft.add initFuncTblEntry(m.sym, sig, m.initGlobalsProc)

  if m.initProc.start != -1:
    ft.add initFuncTblEntry(m.sym, sig, m.initProc)

proc generateMain(c: var TCtx, mainModule: PSym,
                  mlist: ModuleList): FunctionIndex =
  ## Generates and links in the main procedure (the entry point) along with
  ## setting up the required state.

  # first, create a `VmType` for the init proc:
  let
    # Use a fresh signature ID for the init functions because we'd need
    # access to a `proc()` `PType` otherwise:
    voidSig = c.typeInfoCache.nextSigId
    typId = c.types.len.VmTypeId
    typ = PVmType(kind: akCallable, routineSig: voidSig)

  # fill in size information
  (typ.sizeInBytes, typ.alignment) = c.typeInfoCache.staticInfo[akCallable]

  c.types.add(typ)

  var systemIdx, mainIdx: int
  # XXX: can't use `pairs` since it copies
  for i in 0..<mlist.modules.len:
    let sym = mlist.modules[i].sym
    if sfMainModule     in sym.flags: mainIdx = i
    elif sfSystemModule in sym.flags: systemIdx = i

  # then, append the module init procs to the function table:
  let firstInitProc = c.functions.len

  # `generateEntryProc` uses the function table order as the order the
  # init procedures are called in, so we need to add the table entries in the
  # right order. That is, module closed order with special handling for the
  # main and system module
  addInitProcs(c.functions, mlist.modules[systemIdx], voidSig)
  for mI in mlist.modulesClosed.items:
    let m = mlist.modules[mI]
    if {sfMainModule, sfSystemModule} * m.sym.flags == {}:
      addInitProcs(c.functions, m, voidSig)

  addInitProcs(c.functions, mlist.modules[mainIdx], voidSig)

  # lastly, generate the actual code:
  let
    initProcs = firstInitProc..c.functions.high
    info = generateEntryProc(c, mainModule.info, initProcs, typId)

  result = c.functions.len.FunctionIndex
  c.functions.add initFuncTblEntry(mainModule, voidSig, info)

func storeExtra(enc: var PackedEncoder, dst: var PackedEnv,
                routineSymLookup: sink Table[int, LinkIndex],
                consts: seq[(PVmType, PNode)], globals: seq[PVmType]) =
  ## Stores the previously gathered complex constants and globals into `dst`

  var denc: DataEncoder
  denc.startEncoding(dst)
  # XXX: `sink` is used for `routineSymLookup` in order to get around a
  #      deep copy
  denc.routineSymLookup = routineSymLookup

  # complex constants (i.e. non-literals):
  mapList(dst.cconsts, consts, it):
    let id = dst.nodes.len
    dst.nodes.growBy(1)
    denc.storeData(dst, it[1])
    (enc.typeMap[it[0]], id.uint32)

  # for globals, only their types are stored. All initialization is
  # done in VM bytecode
  mapList(dst.globals, globals, it):
    enc.typeMap[it]

proc generateCode*(g: ModuleGraph) =
  ## The backend's entry point. Orchestrates code generation and linking. If
  ## all went well, the resulting binary is written to the project's output
  ## file
  let
    mlist = g.backend.ModuleListRef
    conf = g.config

  var c = TCtx(config: g.config, cache: g.cache, graph: g, idgen: g.idgen,
               mode: emStandalone)

  c.typeInfoCache.init()
  c.codegenInOut.flags = {cgfCollectGlobals}

  # register the extra ops so that code generation isn't performed for the
  # corresponding procs:
  registerCallbacks(c)

  # generate all module init procs (i.e. code for the top-level statements):
  for m in mlist.modules.mitems:
    c.refresh(m.sym, g.idgen)
    generateTopLevelStmts(m, c, g.config)

    # combine module list iteration with initialiazing `initGlobalsCode`:
    m.initGlobalsCode.prc = PProc()

  var startConst = LinkIndex(0)
  # iteratively generate code for all alive procedures
  while c.codegenInOut.nextProc.int > c.functions.len:
    # generate code for the set of active alive routines
    # (`c.codegenInOut.newProcs`). This can uncover new ``const`` symbols
    generateAliveProcs(c, mlist[])
    c.codegenInOut.newProcs.setLen(0)

    # collect routine symbols from new `const`s, if any
    for i in startConst..<c.codegenInOut.nextConst:
      collectRoutineSyms(c.codegenInOut.newConsts[i].ast,
                         c.codegenInOut.newProcs)

    startConst = c.codegenInOut.nextConst

    # create link table entries for the collected routine symbols
    for sym in c.codegenInOut.newProcs.items:
      c.symToIndexTbl[sym.id] = c.codegenInOut.nextProc
      inc c.codegenInOut.nextProc

  # XXX: generation of method dispatchers would go here. Note that `method`
  #      support will require adjustments to DCE handling
  #generateMethods(c)

  # create procs from the global initializer code fragments
  for m in mlist.modules.mitems:
    template frag: untyped = m.initGlobalsCode
    if frag.code.len > 0:
      let
        start = c.code.len
        rc = frag.prc.regInfo.len

      c.appendCode(frag)
      c.gABC(g.emptyNode, opcRet)

      # The code fragment isn't used anymore beyond this point, so it can be
      # freed already
      reset(frag)

      m.initGlobalsProc = (start: start, regCount: rc)
    else:
      m.initGlobalsProc = (start: -1, regCount: 0)

  let entryPoint =
    generateMain(c, g.getModule(conf.projectMainIdx), mlist[])

  c.gABC(g.emptyNode, opcEof)

  # code generation is finished

  # collect globals and `const`s:
  # XXX: these two steps could be combined with storing into `PackedEnv`.
  #      Pros: no need for the `globals` and `consts` seqs
  #      Cons: (probably) higher I-cache pressure, slightly more complex logic

  var globals = newSeq[PVmType](c.codegenInOut.newGlobals.len)
  for i, sym in c.codegenInOut.newGlobals.pairs:
    let typ = c.typeInfoCache.lookup(conf, sym.typ)
    # the type was already created during vmgen
    globals[i] = typ.unsafeGet

  var consts = newSeq[(PVmType, PNode)](c.codegenInOut.newConsts.len)
  for i, sym in c.codegenInOut.newConsts.pairs:
    let typ = c.typeInfoCache.lookup(conf, sym.typ)
    consts[i] = (typ.unsafeGet, sym.ast)

  # pack the data and write it to the ouput file:
  var
    enc: PackedEncoder
    env: PackedEnv

  enc.init(c.types)
  storeEnv(enc, env, c)
  storeExtra(enc, env, c.symToIndexTbl, consts, globals)
  env.code = move c.code
  env.entryPoint = entryPoint

  let err = writeToFile(env, prepareToWriteOutput(conf))
  if err != RodFileError.ok:
    let rep = BackendReport(kind: rbackVmFileWriteFailed,
                            outFilename: conf.absOutFile.string,
                            failureMsg: $err)
    conf.globalReport(rep)

# Below is the `passes` interface implementation

proc myOpen(graph: ModuleGraph, module: PSym, idgen: IdGenerator): PPassContext =
  if graph.backend == nil:
    graph.backend = ModuleListRef()

  let
    mlist = ModuleListRef(graph.backend)
    next = mlist.modules.len

  # append an empty module to the list
  mlist.modules.growBy(1)
  mlist.modules[next] = Module(sym: module)
  mlist.moduleMap[module.id] = next

  result = ModuleRef(list: mlist, index: next)

proc myProcess(b: PPassContext, n: PNode): PNode =
  result = n
  let m = ModuleRef(b)

  const declarativeKinds = routineDefs + {nkTypeSection, nkPragma,
    nkExportStmt, nkExportExceptStmt, nkFromStmt, nkImportStmt,
    nkImportExceptStmt}

  if n.kind notin declarativeKinds:
    m.list.modules[m.index].stmts.add(n)

proc myClose(graph: ModuleGraph; b: PPassContext, n: PNode): PNode =
  result = myProcess(b, n)

  let m = ModuleRef(b)
  m.list.modulesClosed.add(m.index)

const vmgenPass* = makePass(myOpen, myProcess, myClose)