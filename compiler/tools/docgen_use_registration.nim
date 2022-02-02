import
  ./docgen_types,
  ./docgen_file_tracking,
  ./docgen_unparser,
  ./docgen_code_renderer

import
  ast/[
    ast,
    types,
    renderer,
    trees,
    wordrecg
  ],
  utils/[
    debugutils,
    astrepr
  ],
  std/[
    options,
    intsets,
    strutils,
    tables
  ]


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
    switchId: DocEntryId
    moduleId*: DocEntryId
    user: Option[DocEntryId]
    hasInit: bool
    allowMacroNodes: bool ## Allow to record information about usages in
    ## code generated form macro expansions.

proc `+`(state: RegisterState, kind: RegisterStateKind): RegisterState =
  result = state
  result.state.add kind

proc `+`(state: RegisterState, user: DocEntryId): RegisterState =
  result = state
  if not user.isNil():
    result.user = some user

proc `+`(state: RegisterState, user: Option[DocEntryId]): RegisterState =
  result = state
  if result.user.isNone() or (result.user.isSome() and user.isSome()):
    if user.isSome() and user.get().isNil():
      discard

    else:
      result.user = user

proc `+=`(state: var RegisterState, kind: RegisterStateKind) =
  state.state.add kind




proc top(state: RegisterState): RegisterStateKind =
  return state.state[^1]

proc initRegisterState*(): RegisterState =
  RegisterState(state: @[rskTopLevel])

proc isEnum*(node: PNode): bool =
  case node.kind:
    of nkEnumTy:  true
    of nkTypeDef: node[2].isEnum()
    else:         false

proc isAliasDecl(node: PNode): bool =
  case node.kind:
    of nkObjectTy, nkEnumTy: false
    of nkPtrTy, nkRefTy:     isAliasDecl(node[0])
    of nkTypeDef:            isAliasDecl(node[2])
    else:                    true


proc getEffects(node: PNode, effectPos: int): PNode =
  if node.safeLen > 0 and
     node[0].len >= effectListLen and
     not isNil(node[0][effectPos]):
    result = newNode(nkBracket)
    for node in node[0][effectPos]:
      result.add node

  else:
    result = newNode(nkEmpty)

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


proc exprTypeSym(n: PNode): PSym =
  case n.kind:
    of nkCall:
      result = n.typ.sym

    else:
      result = headSym(n)


proc registerProcBody(
    ctx: DocContext, body: PNode, state: RegisterState, node: PNode) =
  let s = node[0].headSym()
  if isNil(s) or isNil(ctx[s]):
    return

  var main = ctx.db[ctx[s]]
  let
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
          main.raisesVia[ctx[callSym]] = DocEntrySet()

  if not mainEffect.isNil:
    for e in mainEffect:
      main.effects.incl ctx[exprTypeSym(e)]

      let callSym = headSym(e)
      if not isNil(callSym) and callSym.kind in skProcDeclKinds:
        main.effectsVia[ctx[callSym]] = DocEntrySet()

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
              if not isNil(id) and id in main.raisesVia:
                main.raisesVia[id].incl ctx[head]

          let effects = head.effectSpec(wTags)
          if not effects.isNil():
            for e in effects:
              let id = ctx[e]
              if not isNil(id):
                main.effectsVia.mgetOrPut(id, DocEntrySet()).incl id

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


proc registerSymbolUse(
    ctx: DocContext,
    node: PNode,
    state: RegisterState,
    parent: PNode
  ): Option[DocEntryId] =

  assert node.kind == nkSym

  var db = ctx.db
  case node.sym.kind:
    of skType:
      if state.top() == rskExport:
        db[state.moduleId].exports.incl ctx[node]

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

        discard db.occur(node, useKind, state.user)
        if useKind in {dokEnumDeclare, dokObjectDeclare, dokAliasDeclare}:
          result = some ctx[node]

    of skEnumField:
      if state.top() == rskEnumFields:
        discard db.occur(node, dokEnumFieldDeclare, state.user)

      else:
        discard db.occur(node, dokEnumFieldUse, state.user)

    of skField:
      if not node.sym.owner.isNil:
        let id = ctx[node.sym.owner]
        if not id.isNil():
          discard db.occur(
            node, parent, id, dokFieldUse, state.user)

    of skProcDeclKinds:
      if state.top() == rskProcHeader:
        discard db.occur(node, dokCallDeclare, state.user)
        result = some ctx[node]

      elif state.top() == rskExport:
        db[state.moduleId].exports.incl ctx[node]

      else:
        discard db.occur(node, dokCall, state.user)

    of skParam, skVar, skConst, skLet, skForVar:
      let sym = node.headSym()
      if sym in ctx:
        if state.top() == rskAsgn:
          discard db.occur(node, dokGlobalWrite, state.user)

        elif state.top() == rskTopLevel:
          discard db.occur(node, dokGlobalDeclare, state.user)
          result = some ctx[sym]

        else:
          discard db.occur(
            node, dokGlobalRead, state.user)

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
          discard db.occur(
            node, kind, $node.headSym(), state.hasInit)

    of skResult:
      # IDEA can be used to collect information about `result` vs
      # `return` uses
      discard

    of skLabel, skTemp, skUnknown, skConditional, skDynLib,
       skGenericParam, skStub, skPackage, skAlias:
      discard # ???

    of skModule:
      discard db.occur(node, dokImport, some(state.moduleId))
      case state.top():
        of rskImport:
          db[state.moduleId].imports.incl ctx[node]

        of rskExport:
          db[state.moduleId].exports.incl ctx[node]

        of rskCallHead:
          # `module.proc`
          discard

        else:
          assert false, $state.top()




proc impl(
    ctx: DocContext,
    node: PNode,
    state: RegisterState,
    parent: PNode
  ): Option[DocEntryId] {.discardable.} =

  if ctx.db.isFromMacro(node) and not state.allowMacroNodes:
    return

  var db = ctx.db

  case node.kind:
    of nkSym:
      result = registerSymbolUse(ctx, node, state, parent)

    of nkIdent:
      if not state.switchId.isNil():
        let sub = db.getSub(state.switchId, $node)
        if not sub.isNil():
          discard db.occur(node, sub, dokEnumFieldUse, state.user)

      elif state.top() == rskDefineCheck:
        let def = db.getOrNew(ndkCompileDefine, $node)
        discard db.occur(node, def, dokDefineCheck, state.user)

      elif state.top() == rskPragma:
        let def = db.getOrNew(ndkPragma, $node)
        discard db.occur(node, def, dokAnnotationUsage, state.user)

      elif state.top() == rskObjectFields:
        let def = db.getSub(state.user.get, $node)
        discard db.occur(node, def, dokFieldDeclare, state.user)

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
          if not parent.isNil():
            let sub = db.getSub(parent, $node)
            discard db.occur(node, sub, dokEnumFieldUse, state.user)


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
          let head = db[headType.sym]
          for fieldPair in node[1 ..^ 1]:
            let field = fieldPair[0]
            let fieldId = db.getSub(head, field.getSName())
            if not fieldId.isNil():
              discard db.occur(
                field, fieldPair, fieldId, dokFieldSet, state.user)

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


proc registerUses*(ctx: DocContext, node: PNode, state: RegisterState) =
  discard impl(ctx, node, state, nil)

type
  CodeWriter = object
    code: DocCode ## Final chunk of the code to write out
    line: int ## Current active line *index*
    column: int ## Current column
    indent: int ## Relative indentation - used for arranging multiple
    ## chunks of macro expansion code, not for indentation-based
    ## formatting.


proc last(writer: var CodeWriter): var DocCodeLine =
  if writer.code.codeLines.len == 0:
    writer.code.codeLines.add newCodeLine(writer.line, "")

  writer.code.codeLines[^1]

proc lastPart(line: var DocCodeLine): var DocCodePart = line.parts[^1]

proc add*(line: var DocCodeLine, text: string) =
  line.text.add text
  line.lineHigh += text.len

proc expandLastPart(line: var DocCodeLine) =
  ## Expand the last part of the line to it's maximum height
  line.lastPart().slice.column.b = line.lineHigh

proc add(writer: var CodeWriter, text: string) =
  ## Add piece of unformatted text to the last line, expanding last
  ## occurence if it does not have any occurence information
  writer.last.add text
  writer.column += text.len
  if writer.last().lastPart().occur.isNone():
    writer.last().expandLastPart()

proc addLine(writer: var CodeWriter) =
  ## Add new empty line to the code writer, accounting for active
  ## indentation
  writer.code.codeLines.add newCodeLine(writer.line, "")
  inc writer.line
  writer.column = 0
  writer.add(repeat(" ", writer.indent))

proc add(writer: var CodeWriter, text: string, occur: DocOccurId) =
  let line = writer.line
  let start = writer.column
  add(writer, text)
  let finish = writer.column
  writer.last.parts.add newCodePart(
    initDocSlice(line, start, finish), occur)


proc writeCode(
    db: var DocDb,
    node: PNode,
    writer: var CodeWriter,
    expand: ExpansionId
  ) =
  ## Write out expanded node into the final chunks of text, adding
  ## occurence information
  var known: Table[int, PNode]
  proc aux(n: PNode) =
    known[n.id] = n
    if 0 < safeLen(n):
      for sub in n:
        aux(sub)

  aux(node)

  debug node

  var
    store = newStrStore()
    opts = initLytOptions()
    conf = defaultNimFormatConf

  let
    blc = toLytBlock(node, conf, store)
    lyt = toLayout(blc, opts)

  for ev in nimFormatEvents(lyt, store):
    case ev.kind:
      # Indentation/separator spaces
      of nimEvFormatSpaces:
        writer.add repeat(" ", ev.spaces)

      # Layout newline - tokens themselves can't contain newlines
      of nimEvFormatNewline:
        writer.addLine()

      # Format regular token: `ev.token` is a `lexer.TokType`
      of nimEvFormatToken:
        writer.add $ev.token

      of nimEvFormatStr:
        writer.add $ev.str

      # Format token node - ident, symbol, integer or any other literal
      of nimEvFormatNode:
        let node = known[ev.node]
        # TODO First check if node is tracked by the sigmatch hook data

        case node.kind:
          of nkIdent:
            writer.add node.ident.s

          of nkSym:
            let id = db[node.sym]
            # TODO Occurence information can be added here,
            if not id.isNil():
              writer.add(node.sym.name.s, db.add DocOccur(
                inExpansionOf: some expand,
                kind: dokInMacroExpansion,
                refid: id
              ))

            else:
              writer.add(node.sym.name.s)

          of nkFloatLiterals:
            writer.add $node.floatVal

          of nkIntKinds:
            writer.add $node.intVal

          else:
            writer.add $node


proc writeCode(
    db: var DocDb,
    expand: ExpansionId,
    writer: var CodeWriter
  ) =
  ## Recursively write information about macro expansion to the code
  template nl() =
    writer.addLine()

  template writeCode(node: PNode) =
    writer.indent += 2
    nl()
    db.writeCode(node, writer, expand)
    writer.indent -= 2
    nl()

  writer.add "# Expansion of the "
  writer.add $db[expand].expansionOf
  nl()
  writeCode(db[expand].expandedFrom)
  nl()
  writer.add "# Evaluated into "
  nl()
  writeCode(db[expand].immediateResult)
  nl()
  writer.add "# Final expansions was"
  nl()
  writeCode(db[expand].resultNode)
  nl()


proc registerExpansions*(ctx: DocContext) =
  var db = ctx.db
  var writer = CodeWriter()
  for expand in ctx.toplevelExpansions:
    db.writeCode(expand, writer)

  for line in writer.code.codeLines:
    echo line.text
    for part in line.parts:
      if part.occur.isSome():
        echo "> ", part
