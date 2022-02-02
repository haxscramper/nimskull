import
  sem/[
    semdata,
    sem,
    passes
  ],
  front/[
    options
  ],
  ast/[
    astalgo,
    ast,
    trees,
    types,
    wordrecg,
    renderer
  ],
  modules/[
    modulegraphs,
    modules
  ],
  utils/[
    astrepr,
    debugutils
  ],
  ./docgen_types,
  ./docgen_file_tracking,
  ./docgen_use_registration,
  ./docgen_unparser,
  ./docgen_ast_aux,
  std/[
    tables,
    hashes,
    strutils,
    intsets
  ]

import std/options as std_options

static:
  assert(
    defined(useNodeIds),
    "Documentation generator requires node ids to be enabled")




type
  DocVisitor = object
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
       visitor.declContext.whenConditions.add branch[0]
       conditions.add branch[0]
       yield (visitor, branch[1])
    else:
       # TODO construct inverted condition from all branches seen earlier,
       # add it as 'when condition' for the context.
       yield (visitor, branch[0])


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

  db.newDocEntry(visitor.docUser, kind, name, visitor.declContext)


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

proc registerTypeDef(db: var DocDb, visitor: DocVisitor, decl: DefTree): DocEntryId =
  case decl.kind:
    of deftObject:
      result = db.newDocEntry(
        visitor,
        db.classifyDeclKind(decl),
        decl.getSName())

      when false:
        if objectDecl.base.isSome():
          entry.superTypes.add ctx[objectDecl.base.get()]

        for param in objectDecl.name.genParams:
          var p = entry.newDocEntry(dekParam, param.head)

        for nimField in iterateFields(objectDecl):
          var field = entry.newDocEntry(dekField, nimField.name)
          field.docText.rawDoc.add nimField.docComment
          field.docText.docBody = ctx.convertComment(
            nimField.docComment, nimField.declNode.get())

          if nimField.declNode.get().isExported():
            field.visibility = dvkPublic

          with field:
            identType = some ctx.toDocType(nimField.fldType)
            identTypeStr = some $nimField.fldType

          ctx.setLocation(field, nimField.declNode.get())
          ctx.addSigmap(nimField.declNode.get(), field)

    of deftEnum:
      result = db.newDocEntry(visitor, ndkEnum, decl.getSName())

      when false:
        ctx.setLocation(entry, node)
        entry.docText.docBody = ctx.convertComment(enumDecl.docComment, node)

        for enField in enumDecl.values:
          var field = entry.newDocEntry(dekEnumField, enField.name)
          field.visibility = entry.visibility
          # info enField.docComment
          field.docText.docBody = ctx.convertComment(
            enField.docComment, enField.declNode.get())
          ctx.setLocation(field, enField.declNode.get())
          ctx.addSigmap(enField.declNode.get(), field)


    of deftAlias:
      result = db.newDocEntry(
        visitor,
        db.classifyDeclKind(decl),
        decl.getSName()
      )

      db[result].baseType = decl.baseType

      when false:
        for param in parseNType(node[0]).genParams:
          var p = entry.newDocEntry(dekParam, param.head)

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

        if def.exported:
          db[doc].visibility = dvkPublic

        db.addSigmap(node[0], doc)

    else:
      failNode node

proc updateCommon(entry: var DocEntry, decl: DefTree) =
  entry.deprecatedMsg = getDeprecated(decl.name)
  if decl.hasSym():
    entry.sym = decl.sym

  if decl.exported:
    entry.visibility = dvkPublic


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

proc setupDocPasses(graph: ModuleGraph): DocDb =
  ## Setup necessary documentation pass context for module graph.
  # Persistent data storage for all documentable entries. The data persists
  # across all open/close triggers.
  var back = DocBackend(db: DocDb())

  graph.backend = back

  registerPass(graph, makePass(
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
    isFrontend = true))

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

        assert not ctx.db.isNil()
        # Register toplevel declaration entries generated in the code,
        # excluding ones that were generated as a result of macro
        # expansions.
        registerTopLevel(ctx.db, visitor, result)

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

proc commandDoc3*(graph: ModuleGraph, ext: string) =
  ## Execute documentation generation command for module graph
  let db = setupDocPasses(graph)
  compileProject(graph)
  echo "Compiled documentation generator project"
