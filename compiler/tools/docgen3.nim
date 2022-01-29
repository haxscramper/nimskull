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
    astrepr
  ],
  ./docgen_types,
  ./docgen_file_tracking,
  ./docgen_unparser,
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

type
  RegisterStateKind = enum
    rskTopLevel
    rskInheritList
    rskPragma
    rskObjectFields ## Object field groups
    rskObjectBranch ## Object branch or switch expression
    rskEnumFields ## Enum field groups

    rskProcHeader
    rskProcArgs
    rskProcReturn
    rskBracketHead
    rskBracketArgs
    rskTypeHeader
    rskAliasHeader
    rskEnumHeader

    rskDefineCheck
    rskAsgn
    rskCallHead

    rskImport
    rskExport
    rskInclude


  RegisterState = object
    state: seq[RegisterStateKind]
    switchId: DocId
    moduleId: DocId
    user: Option[DocId]
    hasInit: bool


proc `+`(state: RegisterState, kind: RegisterStateKind): RegisterState =
  result = state
  result.state.add kind

proc `+`(state: RegisterState, user: DocId): RegisterState =
  result = state
  if not user.isValid():
    result.user = some user

proc `+`(state: RegisterState, user: Option[DocId]): RegisterState =
  result = state
  if result.user.isNone() or (result.user.isSome() and user.isSome()):
    if user.isSome() and not user.get().isValid():
      discard

    else:
      result.user = user

proc `+=`(state: var RegisterState, kind: RegisterStateKind) =
  state.state.add kind




proc top(state: RegisterState): RegisterStateKind =
  return state.state[^1]

proc initRegisterState(): RegisterState =
  RegisterState(state: @[rskTopLevel])

proc startPos(node: PNode): TLineInfo =
  case node.kind:
    of nkTokenKinds:
      result = node.info

    of nkAccQuoted:
      result = node[0].startPos()

    else:
      result = node[0].startPos()

proc finishPos(node: PNode): TLineInfo =
  case node.kind:
    of nkTokenKinds:
      result = node.info
      result.col += len($node).int16 - 1

    of nkAccQuoted:
      result = node.info
      result.col += len($node).int16

    else:
      if len(node) > 0:
        var idx = len(node) - 1
        while idx >= 0 and node[idx].kind in {nkEmpty}:
          dec idx

        if idx >= 0:
          result = node[idx].finishPos()

        else:
          result = node.info

      else:
        result = node.info

proc nodeExtent(node: PNode): DocExtent =
  result.start = startPos(node)
  result.finish = finishPos(node)




proc isEnum*(node: PNode): bool =
  case node.kind:
    of nkEnumTy:
      true

    of nkTypeDef:
      node[2].isEnum()

    else:
      false

proc isAliasDecl*(node: PNode): bool =
  case node.kind:
    of nkObjectTy, nkEnumTy:
      false

    of nkPtrTy, nkRefTy:
      isAliasDecl(node[0])

    of nkTypeDef:
      isAliasDecl(node[2])

    else:
      true

proc effectSpec(n: PNode, effectType: set[TSpecialWord]): PNode =
  assert n.kind in {nkPragma, nkEmpty}
  for it in n:
    case it.kind:
      of nkExprColonExpr, nkCall:
        if whichPragma(it) in effectType:
          result = it[1]
          if result.kind notin {nkCurly, nkBracket}:
            result = newNodeI(nkCurly, result.info)
            result.add(it[1])
          return

      of nkIdent, nkSym, nkEmpty:
        discard

      else:
        assert false, "Unexpected kind " & $it.kind


proc effectSpec(sym: PSym, word: TSpecialWord): PNode =
  if not isNil(sym) and not isNil(sym.ast) and sym.ast.safeLen >= pragmasPos:
    return sym.ast[pragmasPos].effectSpec(word)


proc getEffects(node: PNode, effectPos: int): PNode =
  if node.safeLen > 0 and
     node[0].len >= effectListLen and
     not isNil(node[0][effectPos]):
    result = newNode(nkBracket)
    for node in node[0][effectPos]:
      result.add node

  else:
    result = newNode(nkEmpty)

proc exprTypeSym(n: PNode): PSym =
  case n.kind:
    of nkCall:
      result = n.typ.sym

    else:
      result = headSym(n)

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

proc registerProcDef(ctx: DocContext, def: DefTree): DocEntry =
  ctx.activeUser = ctx[def.name.sym]
  result = ctx.docModule.newDocEntry(
    ctx.classifyDeclKind(def), def.sym.getSName())

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

  result.procKind = pkind


  # for argument in procDef[paramsPos][1 ..^ 1]:
  #   for ident in argument[0 ..^ 3]:
  #     var arg = entry.newDocEntry(ndkArg, ident.getSName())
  #     arg.sym = ident.sym

proc registerProcBody(
    ctx: DocContext, body: PNode, state: RegisterState, node: PNode) =
  let s = node[0].headSym()
  if isNil(s) or not isValid(ctx[s]):
    return

  let
    main = ctx.db[ctx[s]]
    decl = s.ast
    prag = decl[pragmasPos]
    mainRaise = s.typ.n.getEffects(exceptionEffects)
    mainEffect = s.typ.n.getEffects(tagEffects)

  if not mainRaise.isNil:
    for r in mainRaise:
      let sym = exprTypeSym(r)
      if not sym.isNil():
        main.raises.incl ctx[sym]
        let callSym = headSym(r)
        if not isNil(callSym) and callSym.kind in skProcDeclKinds:
          # For `openarray[T]` `headSym` returns type symbol, but we are only
          # concerned with calls to other procedures here.
          main.raisesVia[ctx[callSym]] = DocIdSet()

  if not mainEffect.isNil:
    for e in mainEffect:
      main.effects.incl ctx[exprTypeSym(e)]

      let callSym = headSym(e)
      if not isNil(callSym) and callSym.kind in skProcDeclKinds:
        main.effectsVia[ctx[callSym]] = DocIdSet()

  let icpp = effectSpec(prag, {wImportc, wImportCpp, wImportJs, wImportObjC})
  if not icpp.isNil():
    main.wrapOf = some icpp[0].getSName()

  let dyn = effectSpec(prag, wDynlib)
  if not dyn.isNil():
    main.dynlibOf = some dyn[0].getSName()

  proc aux(node: PNode) =
    case node.kind:
      of nkTokenKinds - {nkSym}:
        discard

      of nkCall, nkCommand:
        let head = node[0].headSym()
        if not isNil(head):
          main.calls.incl ctx[head]
          let raises = head.effectSpec(wRaises)
          if not raises.isEmptyTree():
            for r in raises:
              let id = ctx[r]
              if isValid(id) and id in main.raisesVia:
                main.raisesVia[id].incl ctx[head]

          let effects = head.effectSpec(wTags)
          if not effects.isNil():
            for e in effects:
              let id = ctx[e]
              if isValid(id):
                main.effectsVia.mgetOrPut(id, DocIdSet()).incl id

        for sub in node:
          aux(sub)

      of nkRaiseStmt:
        if node[0].kind != nkEmpty and not isNil(node[0].typ):
          let et = node[0].typ.skipTypes(skipPtrs)
          if not isNil(et.sym):
            main.raisesDirect.incl ctx[et.sym]

      of nkSym:
        case node.sym.kind:
          of skVar, skLet:
            main.globalIO.incl ctx[node.sym]

          else:
            discard

      else:
        for sub in node:
          aux(sub)

  aux(body)


proc impl(
    ctx: DocContext,
    node: PNode,
    state: RegisterState,
    parent: PNode
  ): Option[DocId] {.discardable.} =

  if node.id in ctx.expanded[]:
    return

  case node.kind:
    of nkSym:
      case node.sym.kind:
        of skType:
          if state.top() == rskExport:
            ctx.db[state.moduleId].exports.incl ctx[node]

          else:
            let useKind =
              case state.top():
                of rskTopLevel, rskPragma, rskAsgn:  dokTypeDirectUse
                of rskObjectFields, rskObjectBranch: dokTypeAsFieldUse
                of rskProcArgs, rskProcHeader:       dokTypeAsArgUse

                of rskInheritList: dokInheritFrom
                of rskProcReturn:  dokTypeAsReturnUse
                of rskBracketHead: dokTypeSpecializationUse
                of rskBracketArgs: dokTypeAsParameterUse
                of rskTypeHeader:  dokObjectDeclare
                of rskEnumHeader:  dokEnumDeclare
                of rskAliasHeader: dokAliasDeclare
                of rskCallHead:    dokTypeConversionUse
                of rskImport:      dokImported
                of rskExport:      dokExported
                of rskInclude:     dokIncluded
                of rskDefineCheck: dokDefineCheck
                of rskEnumFields:  dokNone

            ctx.occur(node, useKind, state.user)

            if useKind in {dokEnumDeclare, dokObjectDeclare, dokAliasDeclare}:
              result = some ctx[node]

        of skEnumField:
          if state.top() == rskEnumFields:
            ctx.occur(node, dokEnumFieldDeclare, state.user)

          else:
            ctx.occur(node, dokEnumFieldUse, state.user)

        of skField:
          if not node.sym.owner.isNil:
            let id = ctx[node.sym.owner]
            if id.isValid():
              ctx.occur(node, parent, id, dokFieldUse, state.user)

        of skProcDeclKinds:
          if state.top() == rskProcHeader:
            ctx.occur(node, dokCallDeclare, state.user)
            result = some ctx[node]

          elif state.top() == rskExport:
            ctx.db[state.moduleId].exports.incl ctx[node]

          else:
            ctx.occur(node, dokCall, state.user)

        of skParam, skVar, skConst, skLet, skForVar:
          let sym = node.headSym()
          if sym in ctx:
            if state.top() == rskAsgn:
              ctx.occur(node, dokGlobalWrite, state.user)

            elif state.top() == rskTopLevel:
              ctx.occur(node, dokGlobalDeclare, state.user)
              result = some ctx[sym]

            else:
              ctx.occur(node, dokGlobalRead, state.user)

          else:
            let (kind, ok) =
              case state.top():
                of rskAsgn:                    (dokLocalWrite, true)
                of rskProcHeader, rskProcArgs: (dokLocalArgDecl, true)
                of rskObjectFields, rskProcReturn:
                  # Local callback parameters or fields in return `tuple`
                  # values.
                  (dokLocalWrite, false)

                else:
                  (dokLocalUse, true)

            if ok:
              ctx.occur(node, kind, $node.headSym(), state.hasInit)

        of skResult:
          # IDEA can be used to collect information about `result` vs
          # `return` uses
          discard

        of skLabel, skTemp, skUnknown, skConditional, skDynLib,
           skGenericParam, skStub, skPackage, skAlias:
          discard # ???

        of skModule:
          ctx.occur(node, dokImport, some(state.moduleId))
          let module = ctx.db[state.moduleId]
          case state.top():
            of rskImport:
              module.imports.incl ctx[node]

            of rskExport:
              module.exports.incl ctx[node]

            of rskCallHead:
              # `module.proc`
              discard

            else:
              assert false, $state.top()


    of nkIdent:
      if state.switchId.isValid():
        let sub = ctx.db[state.switchId].getSub($node)
        if sub.isValid():
          ctx.occur(node, sub, dokEnumFieldUse, state.user)

      elif state.top() == rskDefineCheck:
        let def = ctx.db.getOrNew(ndkCompileDefine, $node)
        ctx.occur(node, def.id, dokDefineCheck, state.user)

      elif state.top() == rskPragma:
        let def = ctx.db.getOrNew(ndkPragma, $node)
        ctx.occur(node, def.id, dokAnnotationUsage, state.user)

      elif state.top() == rskObjectFields:
        let def = ctx.db[state.user.get].getSub($node)
        ctx.occur(node, def, dokFieldDeclare, state.user)

    of nkCommentStmt,
       nkEmpty,
       nkStrKinds,
       nkFloatKinds,
       nkNilLit:
      discard
      # TODO store list of all the hardcoded magics in the code -
      # nonstandard float and integer literals, formatting strings etc.

    of nkIntKinds:
      if not isNil(node.typ) and
         not isNil(node.typ.sym) and
         not isNil(node.typ.sym.ast):

        let parent = ctx[node.typ.sym]
        if node.typ.sym.ast.isEnum():
          if parent.isValid():
            let sub = ctx.db[parent].getSub($node)
            ctx.occur(node, sub, dokEnumFieldUse, state.user)


    of nkPragmaExpr:
      result = ctx.impl(node[0], state, node)
      ctx.impl(node[1], state + rskPragma + result, node)

    of nkIdentDefs, nkConstDef:
      var state = state
      state.hasInit = isEmptyTree(node[2])
      result = ctx.impl(node[0], state, node) # Variable declaration
      ctx.impl(node[1], state + result, node)
      ctx.impl(node[2], state + result, node)

    of nkImportStmt:
      for subnode in node:
        ctx.impl(subnode, state + rskImport, node)

    of nkIncludeStmt:
      for subnode in node:
        ctx.impl(subnode, state + rskInclude, node)

    of nkExportStmt:
      # QUESTION not really sure what `export` should be mapped to, so
      # discarding for now.
      for subnode in node:
        ctx.impl(subnode, state + rskExport, node)

    of nkGenericParams:
      # TODO create context with generic parameters declared in
      # procedure/type header and use them to create list of local symbols.
      discard

    of nkPragma:
      # TODO implement for pragma uses
      for subnode in node:
        result = ctx.impl(subnode, state + rskPragma, node)

    of nkAsmStmt:
      # IDEA possible analysis of passthrough code?
      discard

    of nkCall, nkConv:
      if node.kind == nkCall and "defined" in $node:
        ctx.impl(node[1], state + rskDefineCheck, node)

      else:
        for idx, subnode in node:
          if idx == 0:
            ctx.impl(subnode, state + rskCallHead, node)

          else:
            ctx.impl(subnode, state, node)

    of nkProcDeclKinds:
      result = ctx.impl(node[0], state + rskProcHeader, node)
      # IDEA process TRM macros/pattern susing different state constraints.
      ctx.impl(node[1], state + rskProcHeader + result, node)
      ctx.impl(node[2], state + rskProcHeader + result, node)
      ctx.impl(node[3][0], state + rskProcReturn + result, node)
      for n in node[3][1 ..^ 1]:
        ctx.impl(n, state + rskProcArgs + result, node)

      ctx.impl(node[4], state + rskProcHeader + result, node)
      ctx.impl(node[5], state + rskProcHeader + result, node)

      ctx.impl(node[6], state + result, node)

      ctx.registerProcBody(node[6], state, node)

    of nkBracketExpr:
      ctx.impl(node[0], state + rskBracketHead, node)
      for subnode in node[1..^1]:
        ctx.impl(subnode, state + rskBracketArgs, node)

    of nkRecCase:
      var state = state
      state.switchId = ctx[node[0][1]]
      ctx.impl(node[0], state + rskObjectBranch, node)
      for branch in node[1 .. ^1]:
        for expr in branch[0 .. ^2]:
          ctx.impl(expr, state + rskObjectBranch, node)

        ctx.impl(branch[^1], state + rskObjectFields, node)

    of nkRaiseStmt:
      # TODO get type of the raised expression and if it is a concrete type
      # record `dokRaised` usage.
      for subnode in node:
        ctx.impl(subnode, state, node)

    of nkOfInherit:
      ctx.impl(node[0], state + rskInheritList, node)

    of nkOpenSymChoice:
      # QUESTION I have no idea what multiple symbol choices mean *after*
      # semcheck happened, so discarding for now.
      discard

    of nkTypeDef:
      let decl =
        if node.isAliasDecl(): rskAliasHeader
        elif node.isEnum():    rskEnumHeader
        else:                  rskTypeHeader

      var state = state
      result = ctx.impl(node[0], state + decl, node)
      if state.user.isNone():
        state.user = result

      ctx.impl(node[1], state + decl + result, node)
      ctx.impl(node[2], state + result, node)

    of nkDistinctTy:
      if node.safeLen > 0:
        ctx.impl(node[0], state, node)

    of nkAsgn:
      result = ctx.impl(node[0], state + rskAsgn, node)
      if result.isSome():
        ctx.impl(node[1], state + result.get(), node)

      else:
        ctx.impl(node[1], state, node)

    of nkObjConstr:
      if node[0].kind != nkEmpty and not isNil(node[0].typ):
        let headType = node[0].typ.skipTypes(skipPtrs)
        if not isNil(headType.sym):
          let head = ctx.db[ctx[headType.sym]]
          for fieldPair in node[1 ..^ 1]:
            let field = fieldPair[0]
            let fieldId = head.getSub(field.getSName())
            if fieldId.isValid():
              ctx.occur(field, fieldPair, fieldId, dokFieldSet, state.user)

      ctx.impl(node[0], state, node)
      for subnode in node[1 ..^ 1]:
        ctx.impl(subnode[1], state, node)

    else:
      var state = state
      case node.kind:
        of nkEnumTy:
          state += rskEnumFields

        of nkObjectTy, nkRecList:
          state += rskObjectFields

        else:
          discard

      for subnode in node:
        ctx.impl(subnode, state, node)


proc registerUses(ctx: DocContext, node: PNode, state: RegisterState) =
  discard impl(ctx, node, state, nil)

proc registerTypeDef(ctx: DocContext, decl: DefTree): DocEntry =
  ctx.activeUser = ctx[decl.name.sym]
  case decl.kind:
    of deftObject:
      result = ctx.docModule.newDocEntry(
        ctx.classifyDeclKind(decl), decl.getSName())

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
      result = ctx.docModule.newDocEntry(ndkEnum, decl.getSName())

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
      result = ctx.docModule.newDocEntry(
        classifyDeclKind(ctx, decl),
        decl.getSName())

      result.baseType = decl.baseType

      when false:
        for param in parseNType(node[0]).genParams:
          var p = entry.newDocEntry(dekParam, param.head)

    of deftMagic:
      result = ctx.docModule.newDocEntry(ndkBuiltin, decl.getSName())

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

        var doc = ctx.docModule.newDocEntry(nodeKind, def.getSName())
        if def.exported:
          doc.visibility = dvkPublic

        ctx.addSigmap(node[0], doc)

    else:
      failNode node

proc updateCommon(entry: var DocEntry, decl: DefTree) = 
  entry.deprecatedMsg = getDeprecated(decl.name)
  entry.sym = decl.sym
  if decl.exported:
    entry.visibility = dvkPublic


proc registerTopLevel(ctx: DocContext, node: PNode) =
  let user = ctx.activeUser

  case node.kind:
    of nkProcDeclKinds:
      let def = unparseDefs(node)[0]
      var doc = ctx.registerProcDef(def)
      doc.updateCommon(def)
      ctx.addSigmap(node, doc)

    of nkTypeSection:
      for typeDecl in node:
        registerTopLevel(ctx, typeDecl)

    of nkTypeDef:
      let def = unparseDefs(node)[0]
      var doc = ctx.registerTypeDef(def)
      doc.updateCommon(def)
      ctx.addSigmap(node, doc)


    of nkStmtList:
      for subnode in node:
        ctx.registerTopLevel(subnode)

    of nkVarSection, nkLetSection, nkConstSection:
      ctx.registerDeclSection(node)

    else:
      discard

  var state = initRegisterState()
  state.moduleId = ctx.docModule.id
  ctx.activeUser = user
  ctx.registerUses(node, state)

func expandCtx(ctx: DocContext): int =
  ctx.config.m.msgContext.len

proc preExpand(context: PContext, expr: PNode, sym: PSym) =
  let ctx = DocContext(context)
  ctx.db.expansions.add ExpansionData(
    expansionOf: sym,
    expandDepth: ctx.expansionStack.len,
    expandedFrom: expr,
    expansionUser: ctx.activeUser
  )

  ctx.activeExpansion = ExpansionId(ctx.db.expansions.high)
  ctx.expansionStack.add ctx.db.expansions.high

proc postExpand(context: PContext, expr: PNode, sym: PSym) =
  let ctx = DocContext(context)
  ctx.expanded[].incl expr.id
  let last = ctx.expansionStack.pop()
  ctx.db.expansions[last].resultNode = expr

type
  DocBackend = ref object of RootObj
    db: DocDb
    sigmap: TableRef[PSym, DocId]
    expanded: ref IntSet

proc setupDocPasses(graph: ModuleGraph): DocDb =
  ## Setup necessary context (semantic and docgen passes) for module graph
  var back = DocBackend(
    sigmap: newTable[PSym, DocId](),
    db: DocDb(),
    expanded: (ref IntSet)()
  )

  graph.backend = back

  registerPass(graph, makePass(
    TPassOpen(
      proc(
        graph: ModuleGraph, module: PSym, idgen: IdGenerator
      ): PPassContext {.nimcall.} =
        var back = DocBackend(graph.backend)

        var c = DocContext(newContext(graph, module, DocContext(
          db: back.db,
          expanded: back.expanded,
          sigmap: back.sigmap,
          docModule: newDocEntry(back.db, ndkModule, module.name.s),
          firstExpansion: back.db.expansions.len,
          expandHooks: (
            preMacro:     SemExpandHook(preExpand),
            postMacro:    SemExpandHook(postExpand),
            preTemplate:  SemExpandHook(preExpand),
            postTemplate: SemExpandHook(postExpand)))))

        c.activeUser = c.docModule.id
        c.docModule.visibility = dvkPublic
        back.sigmap[module] = c.docModule.id

        return semPassSetupOpen(c, graph, module, idgen)
    ),
    TPassProcess(
      proc(c: PPassContext, n: PNode): PNode {.nimcall.} =
        result = semPassProcess(c, n)
        var ctx = DocContext(c)
        registerTopLevel(ctx, result)
    ),
    TPassClose(
      proc(graph: ModuleGraph; p: PPassContext, n: PNode): PNode {.nimcall.} =
        result = semPassClose(graph, p, n)
        var ctx = DocContext(p)

        for expand in ctx.db.expansions[ctx.firstExpansion .. ^1]:
          if expand.expandDepth == 0:
            ctx.occur(
              tern(
                expand.expandedFrom.kind in {nkIdent, nkSym},
                expand.expandedFrom,
                expand.expandedFrom[0],
              ),
              ctx[expand.expansionOf],
              dokCall,
              some expand.expansionUser)

          else:
            # Information about expansion-in-expansion for a macro
            discard


    ),
    isFrontend = true))

  return back.db

proc commandDoc3*(graph: ModuleGraph, ext: string) =
  ## Execute documentation generation command for module graph
  let db = setupDocPasses(graph)
  compileProject(graph)
