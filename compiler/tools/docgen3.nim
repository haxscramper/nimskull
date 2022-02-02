import
  sem/[
    semdata,
    sem,
    passes
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
    options,
    tables,
    hashes,
    strutils,
    intsets
  ]

static:
  assert(
    defined(useNodeIds),
    "Documentation generator requires node ids to be enabled")







proc getEntryName(node: PNode): DefName =
  case node.kind:
    of nkIdentDefs, nkTypeDef, nkConstDef, nkProcDeclKinds:
      result = getEntryName(node[0])

    else:
      result = unparseName(node)

proc classifyDeclKind(ctx: DocContext, def: DefTree): DocEntryKind =
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
              ctx.db[ctx[def.objBase]].kind

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

proc registerProcDef(ctx: DocContext, def: DefTree): DocEntryId =
  ctx.activeUser = ctx[def.name.sym]
  result = ctx.db.newDocEntry(
    ctx.docModule,
    ctx.classifyDeclKind(def),
    def.sym.getSName())

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

  ctx.db[result].procKind = pkind

proc registerTypeDef(ctx: DocContext, decl: DefTree): DocEntryId =
  ctx.activeUser = ctx[decl.name.sym]
  case decl.kind:
    of deftObject:
      result = ctx.db.newDocEntry(
        ctx.docModule,
        ctx.classifyDeclKind(decl),
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
      result = ctx.db.newDocEntry(ctx.docModule, ndkEnum, decl.getSName())

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
      result = ctx.db.newDocEntry(
        ctx.docModule,
        classifyDeclKind(ctx, decl),
        decl.getSName())

      ctx.db[result].baseType = decl.baseType

      when false:
        for param in parseNType(node[0]).genParams:
          var p = entry.newDocEntry(dekParam, param.head)

    of deftMagic:
      result = ctx.db.newDocEntry(
        ctx.docModule, ndkBuiltin, decl.getSName())

    else:
      assert false, $decl.kind





proc registerDeclSection(
    ctx: DocContext,
    node: PNode,
    nodeKind: DocEntryKind = ndkGlobalVar
  ) =

  case node.kind:
    of nkConstSection, nkVarSection, nkLetSection:
      let nodeKind =
        case node.kind:
          of nkConstSection: ndkGlobalConst
          of nkVarSection:  ndkGlobalVar
          else: ndkGlobalLet

      for subnode in node:
        ctx.registerDeclSection(subnode, nodeKind)

    of nkConstDef, nkIdentDefs:
      let defs = unparseDefs(node)
      for def in defs:
        ctx.activeUser = ctx[def.sym]
        let pragma = def.pragmas.filterPragmas(
          @["intdefine", "strdefine", "booldefine"])

        let nodeKind = tern(0 < len(pragma), ndkCompileDefine, nodeKind)

        var doc = ctx.db.newDocEntry(
          ctx.docModule, nodeKind, def.getSName())

        if def.exported:
          ctx.db[doc].visibility = dvkPublic

        ctx.addSigmap(node[0], doc)

    else:
      failNode node

proc updateCommon(entry: var DocEntry, decl: DefTree) =
  entry.deprecatedMsg = getDeprecated(decl.name)
  entry.sym = decl.sym
  if decl.exported:
    entry.visibility = dvkPublic


proc registerTopLevel(ctx: DocContext, node: PNode) =
  if ctx.db.isFromMacro(node):
    return

  case node.kind:
    of nkProcDeclKinds:
      let def = unparseDefs(node)[0]
      var doc = ctx.registerProcDef(def)
      ctx.db[doc].updateCommon(def)
      ctx.addSigmap(node, doc)

    of nkTypeSection:
      for typeDecl in node:
        registerTopLevel(ctx, typeDecl)

    of nkTypeDef:
      let def = unparseDefs(node)[0]
      var doc = ctx.registerTypeDef(def)
      ctx.db[doc].updateCommon(def)
      ctx.addSigmap(node, doc)


    of nkStmtList:
      for subnode in node:
        ctx.registerTopLevel(subnode)

    of nkVarSection, nkLetSection, nkConstSection:
      ctx.registerDeclSection(node)

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
    expansionUser: ctx.activeUser
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
  ## Setup necessary context (semantic and docgen passes) for module graph
  var back = DocBackend(db: DocDb())

  graph.backend = back

  registerPass(graph, makePass(
    TPassOpen(
      proc(
        graph: ModuleGraph, module: PSym, idgen: IdGenerator
      ): PPassContext {.nimcall.} =
        var back = DocBackend(graph.backend)
        var c = DocContext(newContext(graph, module, DocContext(
          db: back.db,
          docModule: newDocEntry(back.db, ndkModule, module.name.s),
          resolveHook:  SemResolveHook(resolve),
          expandHooks: (
            preMacro:         SemExpandHook(preExpand),
            preMacroResem:    SemExpandHook(preResem),
            postMacro:        SemExpandHook(postExpand),
            preTemplate:      SemExpandHook(preExpand),
            preTemplateResem: SemExpandHook(preResem),
            postTemplate:     SemExpandHook(postExpand)))))

        c.activeUser = c.docModule
        c.db[c.docModule].visibility = dvkPublic
        back.db.sigmap[module] = c.docModule

        return semPassSetupOpen(c, graph, module, idgen)
    ),
    TPassProcess(
      proc(c: PPassContext, n: PNode): PNode {.nimcall.} =
        result = semPassProcess(c, n)
        var ctx = DocContext(c)
        let user = ctx.activeUser

        # Register toplevel declaration entries generated in the code,
        # excluding ones that were generated as a result of macro
        # expansions.
        registerTopLevel(ctx, result)

        # Register immediate uses of the macro expansions
        var state = initRegisterState()
        state.moduleId = ctx.docModule
        ctx.activeUser = user
        ctx.registerUses(result, state)
    ),
    TPassClose(
      proc(graph: ModuleGraph; p: PPassContext, n: PNode): PNode {.nimcall.} =
        result = semPassClose(graph, p, n)
        var ctx = DocContext(p)
        registerExpansions(ctx)

        # for expand in ctx.db.expansions[ctx.firstExpansion .. ^1]:
        #   if expand.expandDepth == 0:
        #     ctx.occur(
        #       tern(
        #         expand.expandedFrom.kind in {nkIdent, nkSym},
        #         expand.expandedFrom,
        #         expand.expandedFrom[0],
        #       ),
        #       ctx[expand.expansionOf],
        #       dokCall,
        #       some expand.expansionUser)

        #   else:
        #     # Information about expansion-in-expansion for a macro
        #     discard


    ),
    isFrontend = true))

  return back.db

proc commandDoc3*(graph: ModuleGraph, ext: string) =
  ## Execute documentation generation command for module graph
  let db = setupDocPasses(graph)
  compileProject(graph)
