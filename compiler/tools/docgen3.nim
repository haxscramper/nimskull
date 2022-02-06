import
  sem/[
    semdata,
    sem,
    passes
  ],
  front/[
    options,
    msgs
  ],
  ast/[
    ast,
    renderer,
    lineinfos
  ],
  modules/[
    modulegraphs,
    modules
  ],
  utils/[
    astrepr,
    debugutils,
    pathutils
  ],
  ./docgen_types,
  ./docgen_file_tracking,
  ./docgen_use_registration,
  ./docgen_unparser,
  ./docgen_ast_aux,
  ./docgen_sqlite,
  std/[
    tables,
    hashes,
    strutils,
    strformat,
    sequtils
  ]

import std/options as std_options

static:
  assert(
    defined(useNodeIds),
    "Documentation generator requires node ids to be enabled")




type
  DocVisitor = object
    parent: Option[DocEntryId]
    docUser: DocEntryId ## Active toplevel user
    declContext: DocDeclarationContext ## Active documentation declaration
    ## context that will be passed to new documentable entry on construction.

iterator visitWhen(visitor: DocVisitor, node: PNode): (DocVisitor, PNode) =
  ## Iterate over all branches in 'when' statement, yielding new visitor
  ## object with updated 'when' conditions.
  var conditions: seq[PNode]
  for branch in node:
    var visitor = visitor
    if branch.kind == nkElifBranch:
      # defensive tree copy here in order to avoid sem modifying the data
      # later on
      let cp = branch[0].copyTree()
      visitor.declContext.whenConditions.add cp
      conditions.add cp
      yield (visitor, branch[1])

    else:
      # TODO construct inverted condition from all branches seen earlier,
      # add it as 'when condition' for the context.
      yield (visitor, branch[0])

proc initDocPart*(example: PNode): DocTextPart =
  discard

proc registerPreUses(
    conf: ConfigRef,
    db: var DocDb,
    visitor: DocVisitor,
    node: PNode
  ) =
  ## Register all usage information that can be meaningfully collected
  ## before semantic passes. For now this includes conditional import and
  ## include usages.
  let file = node.info.fileIndex

  proc addImport(node: PNode) =
    discard "TODO"

  proc addInclude(node: PNode) =
    discard "TODO"

  proc aux(node: PNode, vistor: DocVisitor) =
    case node.kind:
      of nkImportStmt:
        let file = node.info.fileIndex
        for sub in node:
          addImport(sub)

      of nkFromStmt, nkImportExceptStmt:
        addImport(node[0])

      of nkIncludeStmt:
        for sub in node:
          addInclude(sub)

      of nkStmtList, nkBlockStmt, nkStmtListExpr, nkBlockExpr:
        for sub in node:
          aux(sub, visitor)

      of nkWhenStmt:
        for (visitor, body) in visitWhen(visitor, node):
          aux(body, visitor)

      else:
        discard

  aux(node, visitor)


proc newDocEntry(
    db: var DocDb,
    visitor: DocVisitor,
    kind: DocEntryKind,
    name: string
  ): DocEntryId =

  if visitor.parent.isSome():
    db.newDocEntry(
      kind = kind,
      name = name,
      parent = visitor.parent.get(),
      context = visitor.declContext)

  else:
    db.newDocEntry(visitor.docUser, kind, name, visitor.declContext)

proc newDocEntry(
    db: var DocDb,
    visitor: DocVisitor,
    parent: DocEntryId,
    kind: DocEntryKind,
    name: string
  ): DocEntryId =

  db.newDocEntry(
    kind = kind,
    name = name,
    parent = parent,
    context = visitor.declContext)


proc getEntryName(node: PNode): DefName =
  case node.kind:
    of nkIdentDefs, nkTypeDef, nkConstDef, nkProcDeclKinds:
      result = getEntryName(node[0])

    else:
      result = unparseName(node)

proc classifyDeclKind(db: DocDb, def: DefTree): DocEntryKind =
  case def.kind:
    of deftProc:
      case def.node.kind:
        of nkProcDef:      result = ndkProc
        of nkTemplateDef:  result = ndkTemplate
        of nkMacroDef:     result = ndkMacro
        of nkMethodDef:    result = ndkMethod
        of nkIteratorDef:  result = ndkIterator
        of nkFuncDef:      result = ndkFunc
        of nkConverterDef: result = ndkConverter
        else:
          failNode def.node


    of deftObject:
      case def.getSName():
        of "CatchableError": result = ndkException
        of "Defect":         result = ndkDefect
        of "RootEffect":     result = ndkEffect
        else:
          result =
            if def.objBase.isNil():
              ndkObject

            else:
              db[db[def.objBase]].kind

    of deftAlias:
      if def.node.kind in {nkInfix}:
        result = ndkTypeclass

      else:
        result = ndkAlias

    else:
      assert false, "todo " & $def.kind

proc getDeprecated(name: DefName): Option[string] =
  let depr = name.pragmas.filterPragmas(@["deprecated"])
  if 0 < len(depr):
    let depr = depr[0]
    if depr.safeLen == 0:
      return some ""

    else:
      return some depr[1].getSName()

proc updateCommon(entry: var DocEntry, decl: DefTree) =
  ## Update common fields from the unparsed definition tree
  entry.deprecatedMsg = getDeprecated(decl.name)
  entry.location = some decl.nameNode().info
  if decl.kind == deftProc:
    for doc in decl.procDocs:
      entry.docText.parts.add:
        if doc.kind == nkCommentStmt:
          initDocPart(doc.comment)

        else:
          initDocPart(doc)

  else:
    entry.docText = initDocText(decl.comment)

  if decl.hasSym():
    entry.sym = decl.sym

  if decl.exported:
    entry.visibility = dvkPublic


proc registerProcDef(db: var DocDb, visitor: DocVisitor, def: DefTree): DocEntryId =
  ## Register new procedure definition in the documentation database,
  ## return constructed entry
  result = db.newDocEntry(
    visitor,
    db.classifyDeclKind(def),
    def.getSName()
  )

  let name = def.getSName()
  var pkind: DocProcKind
  case name:
    of "=destroy": pkind = dpkDestructor
    of "=sink":    pkind = dpkMoveOverride
    of "=copy":    pkind = dpkCopyOverride
    of "=":        pkind = dpkAsgnOverride
    else:
      if name[^1] == '=' and name[0] in IdentStartChars:
        pkind = dpkPropertySet

      elif name[0] in IdentStartChars:
        if name.startsWith("init") or name.startsWith("new"):
          pkind = dpkConstructor

        else:
          pkind = dpkRegular

      else:
        pkind = dpkOperator

  db[result].procKind = pkind

  for argument in def.arguments:
    let arg = db.newDocEntry(
      visitor = visitor,
      parent = result,
      kind = ndkArg,
      name = argument.getSName()
    )

    db[arg].updateCommon(argument)
    db[arg].argType = argument.typ
    db[arg].argDefault = argument.initExpr

proc registerTypeDef(db: var DocDb, visitor: DocVisitor, decl: DefTree): DocEntryId =
  case decl.kind:
    of deftObject:
      result = db.newDocEntry(
        visitor,
        db.classifyDeclKind(decl),
        decl.getSName())

      let objId = result
      var db = addr db
      # "'db' is of type <var DocDb> which cannot be captured as it would
      # violate memory safety". Yes, the `db` is a `ref` type, but I still
      # get this error, so I'm using pointer hack here.

      proc auxField(field: DefField): DocEntryId =
        let head = field.head
        result = db[].newDocEntry(
          visitor = visitor,
          parent = objId,
          kind = ndkField,
          name = head.getSName()
        )

        db[][result].updateCommon(head)
        db[][result].fieldType = head.typ

      proc auxField(field: DefField, visitor: DocVisitor): seq[DocEntryId] =
        case field.kind:
          of deffWrapCase:
            let resField = auxField(field)
            for branch in field.branches:
              db[][resField].switchesInto.add((branch.branchExprs, @[]))
              for subfield in branch.subfields:
                # `when` might have nore than one nested field returned
                # from aux, all other cases are covered by `subfields`
                # iteration above.
                for subId in auxField(subfield, visitor):
                  db[][resField].switchesInto[^1].subfields.add subId

            # `case` only yields a single toplevel field
            result.add resField

          of deffWrapWhen:
            # 'wrap when' does not have a direct field declared, instead
            # return all the nested fields directly
            for branch in field.branches:
              var visitor = visitor
              if branch.branchExprs.len > 0:
                visitor.declContext.whenConditions.add branch.branchExprs[0]

              for subfield in branch.subfields:
                result.add auxField(subfield, visitor)

          of deffPlain:
            result.add auxField(field)

      for field in decl.objFields:
        discard auxField(field, visitor)


    of deftEnum:
      result = db.newDocEntry(visitor, ndkEnum, decl.getSName())
      # Calling 'updateCommon' can be done multiple times, and here we need
      # it in order to get correct visibility annotations on the enum
      # fields.
      updateCommon(db[result], decl)

      for field in decl.enumFields:
        let fId = db.newDocEntry(
          visitor = visitor,
          parent = result,
          kind = ndkEnumField,
          name = field.getSName()
        )

        db[fId].updateCommon(field)
        db[fId].visibility = db[result].visibility

        if not field.strOverride.isNil():
          db[fId].enumStringOverride = some field.strOverride.getSName()

        if not field.valOverride.isNil():
          db[fId].enumValueOverride = some field.valOverride

    of deftAlias:
      result = db.newDocEntry(
        visitor,
        db.classifyDeclKind(decl),
        decl.getSName()
      )

      db[result].baseType = decl.baseType

    of deftMagic:
      result = db.newDocEntry(
        visitor, ndkBuiltin, decl.getSName())

    else:
      assert false, $decl.kind

proc registerDeclSection(
    db: var DocDb,
    visitor: DocVisitor,
    node: PNode,
    nodeKind: DocEntryKind = ndkGlobalVar
  ) =
  ## Register let, var or const declaration section

  case node.kind:
    of nkConstSection, nkVarSection, nkLetSection:
      let nodeKind =
        case node.kind:
          of nkConstSection: ndkGlobalConst
          of nkVarSection:  ndkGlobalVar
          else: ndkGlobalLet

      for subnode in node:
        db.registerDeclSection(visitor, subnode, nodeKind)

    of nkConstDef, nkIdentDefs:
      let defs = unparseDefs(node)
      for def in defs:
        let pragma = def.pragmas.filterPragmas(
          @["intdefine", "strdefine", "booldefine"])

        let nodeKind = tern(0 < len(pragma), ndkCompileDefine, nodeKind)

        var doc = db.newDocEntry(
          visitor, nodeKind, def.getSName())

        db.addSigmap(node[0], doc)
        db[doc].updateCommon(def)

    else:
      failNode node


proc registerTopLevel(db: var DocDb, visitor: DocVisitor, node: PNode) =
  if db.isFromMacro(node):
    return

  case node.kind:
    of nkProcDeclKinds:
      let def = unparseDefs(node)[0]
      var doc = db.registerProcDef(visitor, def)
      db[doc].updateCommon(def)
      db.addSigmap(node, doc)

    of nkTypeSection:
      for typeDecl in node:
        registerTopLevel(db, visitor, typeDecl)

    of nkTypeDef:
      let def = unparseDefs(node)[0]
      var doc = db.registerTypeDef(visitor, def)
      db[doc].updateCommon(def)
      db.addSigmap(node, doc)

    of nkStmtList:
      for subnode in node:
        db.registerTopLevel(visitor, subnode)

    of nkVarSection, nkLetSection, nkConstSection:
      db.registerDeclSection(visitor, node)

    of nkWhenStmt:
      for (visitor, body) in visitWhen(visitor, node):
        registerTopLevel(db, visitor, body)

    else:
      discard

proc registerTopLevelDoc(
    db: var DocDb, module: DocEntryId, node: PNode, inModuleBody: bool)  =
  ## Register toplevel documentation block, either adding it as a part of
  ## documentation comment for the toplevel module (if not `inModuleBody`),
  ## or as a standalone toplevel documentation.
  if inModuleBody:
    let doc = db.newDocEntry(
      parent = module,
      kind = ndkComment,
      name = ""
    )

    db[doc].location = some node.info

  else:
    db[module].docText.parts.add:
      if node.kind == nkCommentStmt:
        initDocPart(node.comment)

      else:
        initDocPart(node)

func expandCtx(ctx: DocContext): int =
  ctx.expansionStack.len

func inExpansion(ctx: DocContext): bool =
  result = 0 < expandCtx(ctx)

func isSkip(context: PContext): bool =
  not context.config.isCompilerDebug()

proc preExpand(context: PContext, expr: PNode, sym: PSym) =
  if context.isSkip(): return

  let ctx = DocContext(context)
  let active = ctx.db.expansions.add Expansion(
    expansionOf: sym,
    expandDepth: ctx.expansionStack.len,
    expandedFrom: expr.copyTree(),
  )

  ctx.activeExpansion = active
  if ctx.expansionStack.len == 0:
    # If there are no other active expansions, register current one as a
    # toplevel entry
    ctx.toplevelExpansions.add active
    discard ctx.db.occur(expr, ctx.db[sym], dokExpansion)

  else:
    ctx.db.expansions[ctx.expansionStack[^1]].nested.add active

  ctx.expansionStack.add active

proc postExpand(context: PContext, expr: PNode, sym: PSym) =
  if context.isSkip(): return
  let ctx = DocContext(context)
  let last = ctx.expansionStack.pop()
  ctx.db.expansions[last].resultNode = expr.copyTree()

  proc aux(n: PNode) =
    ctx.db.expandedNodes[n.id] = last
    if 0 < safeLen(n):
      for sub in items(n):
        aux(sub)

  if not ctx.inExpansion():
    aux(expr)

proc preResem(context: PContext, expr: PNode, sym: PSym) =
  if context.isSkip(): return

  let ctx = DocContext(context)
  ctx.db.expansions[ctx.activeExpansion].immediateResult = expr.copyTree()

proc callHead(n: PNode): PNode =
  case n.kind:
    of nkIdent, nkSym:
      result = n

    of nkCall:
      result = callHead(n[0])

    else:
      failNode n

proc resolve(context: PContext, expr, call: PNode) =
  if context.isSkip(): return

  let ctx = DocContext(context)
  if not ctx.inExpansion(): return

  ctx.db.expansions[ctx.activeExpansion].resolveMap[
    callHead(expr).id] = callHead(call).sym

type
  DocBackend = ref object of RootObj
    ## User to transfer persistent data (database) between open/close pairs
    ## for the semantic pass.
    db: DocDb

let preSemPass = makePass(
  TPassOpen(
    proc(
      graph: ModuleGraph, module: PSym, idgen: IdGenerator
    ): PPassContext {.nimcall.} =
      var db = DocBackend(graph.backend).db
      return DocPreContext(
        graph: graph,
        db: db,
        docModule: db.newDocEntry(ndkModule, module.name.s)
      )
  ),
  TPassProcess(
    proc(c: PPassContext, n: PNode): PNode {.nimcall.} =
      var ctx = DocPreContext(c)
      assert not ctx.db.isNil()
      var visitor = DocVisitor()
      visitor.docUser = ctx.docModule
      visitor.parent = some ctx.docModule
      visitor.declContext.preSem = true

      # Perform initial registration of all the entries in the code -
      # this one excludes all macro-generated ones, but includes
      # conditionally enabled elements.
      registerTopLevel(ctx.db, visitor, n)

      registerPreUses(ctx.graph.config, ctx.db, visitor, n)

      result = n
  ),
  TPassClose(
    proc(graph: ModuleGraph; p: PPassContext, n: PNode): PNode {.nimcall.} =
      # Pre-sem documenter does not have any special 'close' actions for now
      discard
  ),
  isFrontend = true)

proc setupDocPasses(graph: ModuleGraph): DocDb =
  ## Setup necessary documentation pass context for module graph.
  # Persistent data storage for all documentable entries. The data persists
  # across all open/close triggers.
  var back = DocBackend(db: DocDb())

  graph.backend = back

  if false:
    registerPass(graph, preSemPass)

  # Documentation pass /extends/ the semantic pass, so we are adding it directly
  # after 'pre-sem'
  registerPass(graph, makePass(
    TPassOpen(
      proc(
        graph: ModuleGraph, module: PSym, idgen: IdGenerator
      ): PPassContext {.nimcall.} =
        var back = DocBackend(graph.backend)

        var ctx = DocContext(
          db: back.db,
          docModule: back.db[module],
          resolveHook:  SemResolveHook(resolve),
          expandHooks: (
            preMacro:         SemExpandHook(preExpand),
            preMacroResem:    SemExpandHook(preResem),
            postMacro:        SemExpandHook(postExpand),
            preTemplate:      SemExpandHook(preExpand),
            preTemplateResem: SemExpandHook(preResem),
            postTemplate:     SemExpandHook(postExpand)))

        if ctx.docModule.isNil():
          ctx.docModule = newDocEntry(back.db, ndkModule, module.name.s)

        ctx = DocContext(newContext(graph, module, ctx))
        ctx.db[ctx.docModule].visibility = dvkPublic
        back.db.sigmap[module] = ctx.docModule

        return semPassSetupOpen(ctx, graph, module, idgen)
    ),
    TPassProcess(
      proc(c: PPassContext, n: PNode): PNode {.nimcall.} =
        result = semPassProcess(c, n)
        var ctx = DocContext(c)
        var visitor = DocVisitor()

        visitor.docUser = ctx.docModule
        visitor.parent = some ctx.docModule

        assert not ctx.db.isNil()

        if isDocumentationNode(n):
          # Registering toplevel documentation elements for module only in
          # the post-sem analysis, since documentation is not subject to
          # conditional compile-time hiding (supposedly. And if it is then
          # it most likely is the author's intention. Althoug this
          # assumption can certainly be revised later on).
          registerTopLevelDoc(
            ctx.db, ctx.docModule, n, ctx.inModuleBody)

        elif n.kind == nkDiscardStmt:
          # Ignoring toplevel discard statements - the can be used as an
          # old-style comments, or be a testament spec (test files can also
          # have toplevel documentation)
          discard

        else:
          # Register toplevel declaration entries generated in the code,
          # excluding ones that were generated as a result of macro
          # expansions.
          registerTopLevel(ctx.db, visitor, result)
          ctx.inModuleBody = false
          # Register immediate uses of the macro expansions
          var state = initRegisterState()
          state.moduleId = ctx.docModule
          ctx.registerUses(result, state)
    ),
    TPassClose(
      proc(graph: ModuleGraph; p: PPassContext, n: PNode): PNode {.nimcall.} =
        result = semPassClose(graph, p, n)
        var ctx = DocContext(p)
        # Add information about known macro expansions that were processed
        # during compilation
        registerExpansions(ctx)

    ),
    isFrontend = true))

  return back.db



proc writeFlatDump*(conf: ConfigRef, db: DocDb) =
  var res = open("/tmp/res_dump", fmWrite)
  res.writeLine("DOCUMENTABLE ENTRIES:")
  for id, entry in db.entries:
    res.writeLine(db $ id)

  res.writeLine("OCCURENCIES:")
  for id, entry in db.occurencies:
    res.writeLine(db $ id)

  res.close()

const ndkOnlyNested* = {ndkArg, ndkInject}

proc writeEtags*(conf: ConfigRef, db: DocDb, file: AbsoluteFile) =
  var tagfile = open(file.string, fmWrite)
  # https://git.savannah.gnu.org/cgit/emacs.git/tree/etc/ETAGS.EBNF

  const
    FF = '\x0c'  # tag section starter
    LF = '\x0a'  # line terminator
    DEL = '\x7f' # pattern terminator
    SOH = '\x01' # name terminator

  var perFile: OrderedTable[FileIndex, DocEntrySet]
  for id, entry in db.entries:
    if entry.kind notin ndkOnlyNested and entry.location.isSome():
      perFile.mgetOrPut(
        entry.location.get().fileIndex, DocEntrySet()).incl id

  for id, file in conf.m.fileInfos:
    # etags require actual parts of the source code to be present and
    # searchable, so calling `numLines` here to populate `.lines` data.
    discard conf.numLines(FileIndex(id))

  # `tagfile ::= { tagsection }` - etags file consists of multiple
  # (unlimited number of) repeating sections
  for file, ids in perFile:
    # Each tagsection is `tagsection ::= FF LF ( includesec | regularsec ) LF` -
    # in this case we are only writing `regularspec` for now.
    tagfile.write(FF, LF) # Common stat indicators
    # `regularsec ::= filename "," [ unsint ] [ LF fileprop ] { LF tag }`

    # `filename`
    tagfile.write(conf.toFilenameOption(file, foAbs), ",")
    for id in ids: # `{ LF tag }`
      tagfile.write(LF)
      # `tag ::= directtag | patterntag`
      # `patterntag ::= pattern DEL [ tagname SOH ] position`
      # `position ::= realposition | ","`
      # `realposition ::= "," unsint | unsint "," | unsint "," unsint`
      let loc = db[id].location.get()
      tagfile.write(
        # `pattern DEl`. In that canse pattern is a piece of code that
        # emacs will progressively try to search in the target file, so
        # instead of trying to reconstruct all possible prefixes on all
        # possible indentation levels just reusing original source code
        # here.
        conf.m.fileInfos[loc.fileIndex.int32].lines[loc.line - 1], DEL,
        # `tagname SOH` - tagname will displayed in the emacs, so rendering
        # actual signature here.
        db.fullName(id), SOH,
        # `position -> realposition -> unsint "," unsint`
        loc.line, ",", loc.col
      )

  close(tagfile)

import std/json

proc writeJsonLines*(conf: ConfigRef, db: DocDb) =
  var jfile = open("/tmp/jsontags.json", fmWrite)

  for id, entry in db.entries:
    var res = %*{
      "name": entry.name,
      "id": id.int,
      "kind": $entry.kind,
      "visibility": $entry.visibility
    }

    if entry.deprecatedMsg.isSome():
      res["deprecated"] = %entry.deprecatedMsg.get()

    if entry.location.isSome():
      let loc = entry.location.get()
      res["location"] = %*{
        "file": conf.toMsgFilename(loc.fileIndex),
        "line": $loc.line,
        "col": $loc.col
      }


    jfile.writeLine($res)

  close(jfile)

proc commandDoc3*(graph: ModuleGraph, ext: string) =
  ## Execute documentation generation command for module graph
  let db = setupDocPasses(graph)
  compileProject(graph)
  echo "Compiled documentation generator project"

  graph.config.writeFlatDump(db)
  echo "wrote list of tags to the /tmp/res_dump"

  if false:
    graph.config.writeJsonLines(db)
    echo "wrote json to the /tmp/jtags"

  graph.config.writeSqlite(db, AbsoluteFile"/tmp/docdb.sqlite")
  echo "wrote sqlite db to /tmp/docdb.sqlite"

  let outTags = graph.config.projectFull.changeFileExt("etags")
  graph.config.writeEtags(db, outTags)
  echo "wrote etags to the ", outTags.string

  graph.config.writeSourcetrail(db, AbsoluteFile"/tmp/docdb.srctrldb")
  echo "wrote sourcetral db to /tmp/docdb.srctrldb"
