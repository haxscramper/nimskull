import "."/[
  semdata,
  sem,
  sighashes,
  modulegraphs,
  ast,
  passes,
  wordrecg,
  modules,
  docgen_types,
  docgen_file_tracking,
  renderer
]

import std/[options, tables, hashes, strutils]


type
  DocSemContext* = ref object of PContext
    ## Extended semantic pass context used for documentation generation.

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
        raise newUnexpectedKindError(it, $treeRepr(it))


proc effectSpec(sym: PSym, word: TSpecialWord): PNode =
  if notNil(sym) and notNil(sym.ast) and sym.ast.safeLen >= pragmasPos:
    return sym.ast.asProcDecl().pragmas().effectSpec(word)


proc getEffects(node: PNode, effectPos: int): PNode =
  if node.safeLen > 0 and
     node[0].len >= effectListLen and
     not isNil(node[0][effectPos]):
    result = nnkBracket.newPTree()
    for node in node[0, {nkArgList}][effectPos]:
      result.add node

  else:
    result = newEmptyPNode()

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
      main.raises.incl ctx[exprTypeSym(r)]
      let callSym = headSym(r)
      if notNil(callSym) and callSym.kind in skProcDeclKinds:
        # For `openarray[T]` `headSym` returns type symbol, but we are only
        # concerned with calls to other procedures here.
        main.raisesVia[ctx[callSym]] = DocIdSet()

  if not mainEffect.isNil:
    for e in mainEffect:
      main.effects.incl ctx[exprTypeSym(e)]

      let callSym = headSym(e)
      if notNil(callSym) and callSym.kind in skProcDeclKinds:
        main.effectsVia[ctx[callSym]] = DocIdSet()

  let icpp = effectSpec(prag, {wImportc, wImportcpp, wImportJs, wImportObjC})
  if not icpp.isNil():
    main.wrapOf = some icpp[0].getStrVal()

  let dyn = effectSpec(prag, wDynlib)
  if dyn.isNil():
    main.dynlibOf = some dyn[0].getStrVal()

  proc aux(node) =
    case node.kind:
      of nkTokenKinds - {nkSym}:
        discard

      of nkCall, nkCommand:
        let head = node[0].headSym()
        if not isNil(head):
          main.calls.incl ctx[head]
          let raises = head.effectSpec(wRaises)
          if ?raises:
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
        if node[0].kind != nkEmpty and notNil(node[0].typ):
          let et = node[0].typ.skipTypes(skipPtrs)
          if notNil(et.sym):
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
       docgen_file_tracking.nkStrKinds,
       nkFloatKinds,
       nkNilLit:
      discard

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

      result = ctx.impl(node[0], state + decl, node)
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
          let headId = ctx[headType.sym]
          if headId in ctx.db:
            let head = ctx.db[headId]
            for fieldPair in node[1 ..^ 1]:
              let field = fieldPair[0]
              let fieldId = head.getSub(field.getStrVal())
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

proc registerToplevel(ctx: DocContext, node: PNode) =
  try:
    case node.kind:
      of nkProcDeclKinds:
        ctx.registerProcDef(node)

      of nkTypeSection:
        for typeDecl in node:
          registerToplevel(ctx, typeDecl)

      of nkTypeDef:
        ctx.registerTypeDef(node)

      of nkStmtList:
        for subnode in node:
          ctx.registerTopLevel(subnode)

      of nkVarSection, nkLetSection, nkConstSection:
        ctx.registerDeclSection(node)

      of nkEmpty, nkCommentStmt, nkIncludeStmt, nkImportStmt,
         nkPragma, nkExportStmt:
        discard

      else:
        discard

    var state = initRegisterState()
    state.moduleId = ctx.module.id()
    ctx.registerUses(node, state)

  except:
    ctx.err ctx.nodePosDisplay(node)
    raise

proc registerDocs*(ctx: DocContext, node: PNode) =
  discard

proc semDocPassOpen*(
    graph: ModuleGraph; module: PSym; idgen: IdGenerator
  ): PPassContext =
  ## Open extended semantic pass context for given module
  var c = newContext(graph, module, DocSemContext()).DocSemContext()
  return semPassSetupOpen(c, graph, module, idgen)

proc setupDocPasses(graph: ModuleGraph): DocDb =
  ## Setup necessary context (semantic and docgen passes) for module graph
  var db {.global.}: DocDb
  db = DocDb()
  registerPass(graph, makePass(
    # Largely identical to the regular `sem.semPass`, with only exception
    # of the extended sem doc pass context usage.
    semDocPassOpen, semPassProcess, semPassClose, isFrontend = true))

  # Register main documentation pass
  registerPass(graph, makePass(
    TPassOpen(
      proc(graph: ModuleGraph, module: PSym, idgen: IdGenerator): PPassContext {.nimcall.} =
        var context = DocContext(db: db)

        return context
    ),
    TPassProcess(
      proc(c: PPassContext, n: PNode): PNode {.nimcall.} =
        # No node modifications is performed here, only analyzing nodes.
        result = n
        var ctx = DocContext(c)
        registerDocs(ctx, n)
    ),
    TPassClose(
      proc(graph: ModuleGraph; p: PPassContext, n: PNode): PNode {.nimcall.} =
        discard)
  ))

  return db

proc commandDoc3*(graph: ModuleGraph, ext: string) =
  ## Execute documentation generation command for module graph
  let db = setupDocPasses(graph)
  compileProject(graph)
