import
  ./docgen_types,
  ./docgen_file_tracking,
  ./docgen_unparser,
  ./docgen_code_renderer,
  ./docgen_ast_aux

import
  ast/[
    ast,
    types,
    renderer,
    trees,
    wordrecg
  ],
  utils/[
    astrepr
  ],
  front/[
    options
  ],
  std/[
    strutils,
    tables
  ]

import std/options as std_options

type
  DocVisitor* = object
    parent*: DocEntryId
    docUser*: DocEntryId ## Active toplevel user
    declContext*: DocDeclarationContext ## Active documentation declaration
    ## context that will be passed to new documentable entry on construction.
    activeModule*: DocEntryId

proc newDocEntry*(
    db: var DocDb,
    visitor: DocVisitor,
    kind: DocEntryKind,
    name: PNode
  ): DocEntryId =
  assert not visitor.parent.isNil()
  db.newDocEntry(
    kind = kind,
    name = name,
    parent = visitor.parent,
    context = visitor.declContext)

proc newDocEntry*(
    db: var DocDb,
    visitor: DocVisitor,
    parent: DocEntryId,
    kind: DocEntryKind,
    name: PNode
  ): DocEntryId =

  db.newDocEntry(
    kind = kind,
    name = name,
    parent = parent,
    context = visitor.declContext)



iterator visitWhen*(visitor: DocVisitor, node: PNode): (DocVisitor, PNode) =
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
    rskProcBody
    rskBracketHead
    rskBracketArgs
    rskTypeHeader
    rskAliasHeader
    rskEnumHeader
    rskTypeName

    rskDefineCheck
    rskAsgnTo
    rskAsgnFrom
    rskCallHead

    rskImport
    rskExport
    rskInclude


  RegisterState* = object
    state: seq[RegisterStateKind]
    switchId: DocEntryId
    localUser: Option[DocEntryId]
    moduleId*: DocEntryId
    visitor: DocVisitor
    user*: DocEntryId
    hasInit: bool
    allowMacroNodes: bool ## Allow to record information about usages in
    ## code generated form macro expansions.

proc `+`(state: RegisterState, kind: RegisterStateKind): RegisterState =
  result = state
  result.state.add kind

proc `+`(state: RegisterState, user: DocEntryId): RegisterState =
  assert not user.isNil()
  result = state
  result.user = user

proc `+=`(state: var RegisterState, kind: RegisterStateKind) =
  state.state.add kind




proc top(state: RegisterState): RegisterStateKind =
  return state.state[^1]

proc hasAll(state: RegisterState, kinds: set[RegisterStateKind]): bool =
  for kind in kinds:
    var has = false
    if kind in state.state:
      has = true

    if not has:
      return false

  return true

proc hasAny(state: RegisterState, kinds: set[RegisterStateKind]): bool =
  for part in state.state:
    if part in kinds:
      return true


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
    db: var DocDb, body: PNode, state: RegisterState, node: PNode) =
  let s = node[0].headSym()
  if isNil(s) or
     isNil(db[s]) or
     s.kind notin skProcKinds:
    return

  var main = db[db[s]]
  let
    decl = s.ast
    prag = decl[pragmasPos]
    mainRaise = s.typ.n.getEffects(exceptionEffects)
    mainEffect = s.typ.n.getEffects(tagEffects)

  if not mainRaise.isNil:
    for r in mainRaise:
      let sym = exprTypeSym(r)
      if not sym.isNil():
        main.raises.incl db[sym]
        let callSym = headSym(r)
        if not isNil(callSym) and callSym.kind in skProcDeclKinds:
          # For `openarray[T]` `headSym` returns type symbol, but we are only
          # concerned with calls to other procedures here.
          main.raisesVia[db[callSym]] = DocEntrySet()

  if not mainEffect.isNil:
    for e in mainEffect:
      main.effects.incl db[exprTypeSym(e)]

      let callSym = headSym(e)
      if not isNil(callSym) and callSym.kind in skProcDeclKinds:
        main.effectsVia[db[callSym]] = DocEntrySet()

  let icpp = effectSpec(prag, {wImportc, wImportCpp, wImportJs, wImportObjC})
  if not icpp.isNil():
    main.wrapOf = some icpp[0].getSName()

  let dyn = effectSpec(prag, wDynlib)
  if not dyn.isNil():
    main.dynlibOf = some dyn[0].getSName()

  proc aux(node: PNode, db: var DocDb) =
    case node.kind:
      of nkTokenKinds - {nkSym}:
        discard

      of nkCall, nkCommand:
        let head = node[0].headSym()
        if isNil(head) or (
          # `=destroy` or other auto-generated proc
          head notin db and sfGeneratedOp in head.flags
        ):
          discard

        else:
          main.calls.incl db[head]
          let raises = head.effectSpec(wRaises)
          if not raises.isEmptyTree():
            for r in raises:
              let id = db[r]
              if not isNil(id) and id in main.raisesVia:
                main.raisesVia[id].incl db[head]

          let effects = head.effectSpec(wTags)
          if not effects.isNil():
            for e in effects:
              let id = db[e]
              if not isNil(id):
                main.effectsVia.mgetOrPut(id, DocEntrySet()).incl id

        for sub in node:
          aux(sub, db)

      of nkRaiseStmt:
        if node[0].kind != nkEmpty and not isNil(node[0].typ):
          let et = node[0].typ.skipTypes(skipPtrs)
          if not isNil(et.sym):
            main.raisesDirect.incl db[et.sym]

      of nkSym:
        case node.sym.kind:
          of skVar, skLet:
            main.globalIO.incl db[node.sym]

          else:
            discard

      else:
        for sub in node:
          aux(sub, db)

  aux(body, db)

proc occur*(
    db: var DocDb,
    node: PNode,
    kind: DocOccurKind,
    state: RegisterState,
    idOverride: DocEntryId = EmptyDocEntryId
  ): DocOccurId =
  assert not state.user.isNil()
  var occur = DocOccur(
    user: state.user,
    refid: tern(idOverride.isNil(), db[node], idOverride),
    kind: kind,
    loc: db.add(nodeLocation(node)),
    localUser: state.localUser
  )
  assert not occur.refid.isNil(), $node.kind
  return db.occurencies.add occur

proc occur*(
    db: var DocDb,
    node: PNode,
    parent: PNode,
    kind: DocOccurKind,
    state: RegisterState,
    idOverride: DocEntryId = EmptyDocEntryId
  ): DocOccurId =
  ## Construct new docmentable entry occurence and return new ID
  assert not state.user.isNil()
  var occur = DocOccur(
    kind: kind, user: state.user, localUser: state.localUser,
    loc: db.add(parent.subslice(node)),
    refid: tern(idOverride.isNil(), db[node], idOverride))

  assert not occur.refid.isNil(), $node.kind
  return db.add occur

proc registerSymbolUse(
    db: var DocDb,
    node: PNode,
    state: RegisterState,
    parent: PNode
  ) =

  assert node.kind == nkSym
  let sym = node.sym
  case sym.kind:
    of skType:
      if state.top() == rskExport:
        db[state.moduleId].exports.incl db[node]

      else:
        let useKind =
          case state.top():
            of rskTopLevel, rskPragma, rskAsgnTo,
               rskAsgnFrom, rskTypeName, rskProcBody:
              dokTypeDirectUse

            of rskObjectFields, rskObjectBranch: dokTypeAsFieldUse
            of rskProcArgs, rskProcHeader:       dokTypeAsArgUse

            of rskInheritList: dokInheritFrom
            of rskProcReturn:  dokTypeAsReturnUse
            of rskBracketHead: dokTypeSpecializationUse
            of rskBracketArgs: dokTypeAsParameterUse
            # Registered type declaration header again, returning
            # immediately - this occurence of a symbols should not be
            # registered since this information (declaration location) is
            # already contained in the documentable entry definition.
            of rskTypeHeader:  return
            of rskEnumHeader:  return
            of rskAliasHeader: return
            of rskCallHead:    dokTypeConversionUse
            of rskImport:      dokImported
            of rskExport:      dokExported
            of rskInclude:     dokIncluded
            of rskDefineCheck: dokDefineCheck
            of rskEnumFields:  dokNone

        if node notin db and state.hasAny({rskProcReturn, rskProcArgs}):
          discard
          # `proc createU*(T: typedesc, size = 1.Positive): ptr T` - `T` is
          # a `skParam, module:2 item:1788` at the start and
          # `skType, itemId: module:2 item:1791` later on. Completely different
          # symbols, I don't think this is even possible to properly trace down.
        else:
          discard db.occur(node, useKind, state)


    of skEnumField:
      let sym = sym
      if sym notin db:
        # For some unknown reason when enum declaration is seen by the
        # entry registration it still has `nkIdent` for field defintions,
        # and those are not registered in sigmap. This code is a HACK, but
        # I don't want to dive into debugging sem right now, so ...
        let sub = db.getSub(db[sym.owner], $node) # Get owner entry ID from
        # symbol and get it's nested entry by name.
        db.addSigmap(sym, sub) # Add symbol to sigmap

      if state.top() != rskEnumFields:
        # In enum field declaration, ignoring symbol occurence
        discard db.occur(node, dokEnumFieldUse, state)

    of skField:
      if not sym.owner.isNil():
        let id = db[sym.owner]
        if db[id].kind in {ndkObject}:
          # Tuple typedefs can also have fields, but we are not tracking
          # them here since `tuple[]` /type/ is not a documentable entry.
          discard db.occur(
            node, parent, dokFieldUse, state,
            idOverride = db.getSub(id, $node))

    of skProcDeclKinds:
      if state.top() == rskProcHeader:
        # Ignoring procedure symbol in the procedure declaration header
        discard

      elif state.top() == rskExport:
        db[state.moduleId].exports.incl db[node]

      elif sfGeneratedOp notin sym.flags:
        discard db.occur(node, dokCall, state)

    of skParam, skVar, skConst, skLet, skForVar:
      if state.hasAll({rskTypeName}):
        # We are in the type name in one of the arguments or field
        # elements. The only place where it can happen is a callback
        # (field, variable or argument)
        return

      var kind = dokNone
      case state.top():
        of rskAsgnTo:
          kind = dokVarWrite

        of rskAsgnFrom, # `<??> = varSymbol`
           rskBracketHead, # `varSymbol[<??>]`
           rskPragma, # `{.???: varSymbol.}`
           rskProcBody,
           rskBracketArgs: # `??[varSymbol]`
          kind = dokVarRead

        of rskTopLevel:
          # Global variable declaration, location is registered in
          # documentable entry registration, returning immediately.
          return

        of rskProcArgs, rskProcReturn, rskProcHeader:
          if state.hasAll({rskTypeName}):
            # Use template/argument as a return type in a different context.
            #
            # template dotdotImpl(t) {.dirty.} =
            #   iterator `..`*(a, b: t): t {.inline.} =
            #
            # TemplateDef 0
            # 0 Sym 1 dotdotImpl sk:Template
            #     db entry: [1820]: private template 'dotdotImpl(t: )' parent [1] in 11(100, 9 .. 18)
            # 3 FormalParams 4
            #   1 IdentDefs 6
            #     0 Sym 7 t sk:Param
            #         db entry: [1821]: private arg 't' parent [1820] in 11(100, 20 .. 20)
            # 6 StmtList 13
            #   0 IteratorDef 14
            #     3 FormalParams 21
            #       0 Sym 22 t sk:Param
            #           db entry: [1821]: private arg 't' parent [1820] in 11(100, 20 .. 20)
            kind = dokParametrizationWithArg

          else:
            # Local argument declaration, returning.
            return

        else:
          return

      if node in db:
        discard db.occur(node, kind, state)

      else:
        # Technically /all/ symbols should have a mapped documentable
        # entries, but for whacky nonsense like this, I decided to pretend
        # this is not the case.
        #
        # `cast[proc (s: string)](showerrormessage2)(s)`
        discard

    of skResult:
      # IDEA can be used to collect information about `result` vs
      # `return` uses
      discard

    of skLabel, skTemp, skUnknown, skConditional, skDynLib,
       skGenericParam, skStub, skPackage, skAlias:
      discard # ???

    of skModule:
      if false:
        discard db.occur(node, dokImported, state)
        case state.top():
          of rskImport:
            db[state.moduleId].imports.incl db[node]

          of rskExport:
            db[state.moduleId].exports.incl db[node]

          of rskCallHead:
            # `module.proc`
            discard

          else:
            assert false, $state.top()




proc reg(
    db: var DocDb,
    node: PNode,
    state: RegisterState,
    parent: PNode
  ) =

  if db.isFromMacro(node) and not state.allowMacroNodes:
    db.reg(getExpansionOriginal(db, node), state, parent)
    return

  assert not state.user.isNil()

  case node.kind:
    of nkSym:
      registerSymbolUse(db, node, state, parent)

    of nkIdent:
      if not state.switchId.isNil():
        discard db.occur(
          node, dokEnumFieldUse, state, idOverride = db.getSub(state.switchId, $node))

      elif state.top() == rskDefineCheck:
        discard db.occur(
          node, dokDefineCheck, state,
          idOverride = db.getOrNewNamed(ndkCompileDefine, $node))

      elif state.top() == rskPragma:
        discard db.occur(
          node, dokAnnotationUsage, state,
          idOverride = db.getOrNewNamed(ndkPragma, $node))

    of nkCommentStmt,
       nkEmpty,
       nkFloatKinds,
       nkNilLit:
      discard
      # TODO store list of all the hardcoded magics in the code -
      # nonstandard float and integer literals, formatting strings etc.

    of nkStrKinds:
      # TODO register list of all string literals in code.
      discard

    of nkIntKinds:
      if not isNil(node.typ) and
         not isNil(node.typ.sym) and
         not isNil(node.typ.sym.ast) and
         not isNil(node.typ.n) and
         node.typ.n.kind == nkEnumTy:
        for enField in node.typ.n:
          # HACK Enum litearls are replaced by integers in the sem layer
          # for now, so I have to do this weird name translation
          assert enField.kind in {nkSym}
          if enField.getSName() == $node:
            discard db.occur(
              node, dokEnumFieldUse, state,
              idOverride = db[enField])

      else:
        # TODO register usage of magic integer constants in different
        # contexts
        discard

    of nkPragmaExpr:
      db.reg(node[0], state, node)
      db.reg(node[1], state + rskPragma, node)

    of nkIdentDefs, nkConstDef:
      # HACK using last name in the identifier list here, but in general it
      # is not possible to cleanly attach the 'user' part here, unless I
      # run analysis on the `^2` and `^1` for each newly declared variable.
      var state =
        if db.approxContains(node[PosLastIdent]):
          # Identifier is documented in the DB either directly or via
          # 'approximate' location definition.
          state + db[node[PosLastIdent]]

        else:
          # Identifier declaration is not registered in the DB - it is a
          # name in the `tuple[field: type]`.
          assert node.headSym().isNil(), $treeRepr(nil, node)
          state

      state.hasInit = not isEmptyTree(node[PosIdentInit])

      for ident in node[SliceAllIdents]:
        # Variable declaration
        db.reg(ident, state + rskAsgnTo, node)

      # Adding `result` to the state so we can register use of type /by/ a
      # variable
      db.reg(node[PosIdentType], state + rskTypeName, node)
      # Use if expression by a variable
      db.reg(node[PosIdentInit], state + rskAsgnFrom, node)

    of nkImportStmt:
      for subnode in node:
        db.reg(subnode, state + rskImport, node)

    of nkIncludeStmt:
      for subnode in node:
        db.reg(subnode, state + rskInclude, node)

    of nkExportStmt:
      # QUESTION not really sure what `export` should be mapped to, so
      # discarding for now.
      for subnode in node:
        db.reg(subnode, state + rskExport, node)

    of nkGenericParams:
      # TODO create context with generic parameters declared in
      # procedure/type header and use them to create list of local symbols.
      discard

    of nkPragma:
      # TODO implement for pragma uses
      for subnode in node:
        db.reg(subnode, state + rskPragma, node)

    of nkAsmStmt:
      # IDEA possible analysis of passthrough code?
      discard

    of nkCall, nkConv, nkCommand:
      if isRunnableExamples(node):
        return

      if node.kind == nkCall and "defined" in $node:
        db.reg(node[1], state + rskDefineCheck, node)

      else:
        for idx, subnode in node:
          if idx == 0:
            db.reg(subnode, state + rskCallHead, node)

          else:
            db.reg(subnode, state, node)

    of nkProcDeclKinds:
      db.reg(node[PosName], state + rskProcHeader, node)
      let state = state + db[node[PosName]]
      # IDEA process TRM macros/pattern using different state constraints.
      db.reg(node[1], state + rskProcHeader, node)
      db.reg(node[2], state + rskProcHeader, node)
      db.reg(node[PosProcArgs][PosProcReturn], state + rskProcReturn, node)
      for n in node[PosProcArgs][SliceAllArguments]:
        db.reg(n, state + rskProcArgs, node)

      db.reg(node[4], state + rskProcHeader, node)
      db.reg(node[5], state + rskProcHeader, node)
      db.reg(node[PosProcBody], state + rskProcBody, node)

      db.registerProcBody(node[PosProcBody], state, node)

    of nkBracketExpr:
      db.reg(node[0], state + rskBracketHead, node)
      for subnode in node[SliceAllArguments]:
        db.reg(subnode, state + rskBracketArgs, node)

    of nkRecCase:
      var state = state
      state.switchId = db[node[0][1]]
      db.reg(node[0], state + rskObjectBranch, node)
      for branch in node[1 .. ^1]:
        for expr in branch[0 .. ^2]:
          db.reg(expr, state + rskObjectBranch, node)

        db.reg(branch[^1], state + rskObjectFields, node)

    of nkRaiseStmt:
      # TODO get type of the raised expression and if it is a concrete type
      # record `dokRaised` usage.
      for subnode in node:
        db.reg(subnode, state, node)

    of nkOfInherit:
      db.reg(node[0], state + rskInheritList, node)

    of nkOpenSymChoice:
      # QUESTION I have no idea what multiple symbol choices mean *after*
      # semcheck happened, so discarding for now.
      discard

    of nkTypeDef:
      let decl =
        if node.isAliasDecl(): rskAliasHeader
        elif node.isEnum():    rskEnumHeader
        else:                  rskTypeHeader

      let state = state + db[node[PosName]]

      if state.top() != rskTopLevel and node[PosName] notin db:
        # Inner type declaration, they are not tracked into any
        # documentable entries for now.
        discard
      else:
        db.reg(node[PosName], state + decl, node)

      db.reg(node[1], state + decl, node)
      if decl == rskAliasHeader:
        # Not an `object` or `enum` declaration - that leaves only typedef,
        # and typedef RHS is a type name (or several type names)
        db.reg(node[PosTypeBody], state + rskTypeName, node)

      else:
        db.reg(node[PosTypeBody], state, node)

    of nkDistinctTy:
      if node.safeLen > 0:
        db.reg(node[0], state, node)

    of nkAsgn:
      db.reg(node[0], state + rskAsgnTo, node)
      db.reg(node[1], state + rskAsgnFrom, node)

    of nkObjConstr:
      if node[0].kind != nkEmpty and not isNil(node[0].typ):
        let headType = node[0].typ.skipTypes(skipPtrs)
        if not isNil(headType.sym):
          let head = db[headType.sym]
          for fieldPair in node[SliceAllArguments]:
            let key = fieldPair[0]
            if db.approxContains(key):
              db.reg(key, state, fieldPair)

            else:
              # Sometimes it just /happens, at random/, and sem layer
              # does not register fields as symbols, so have to get them
              # by name here.
              discard db.occur(
                key, dokFieldSet, state,
                idOverride = db.getSub(head, key.getSName()))


            db.reg(fieldPair[1], state, fieldPair)

      db.reg(node[0], state, node)
      for subnode in node[SliceAllArguments]:
        db.reg(subnode[1], state, node)

    of nkRecWhen:
      for (visitor, body) in visitWhen(state.visitor, node):
        var state = state
        state.visitor = visitor
        db.reg(body, state, node)

    of nkPostfix:
      db.reg(node[1], state, node)

    of nkAccQuoted:
      for sub in node:
        db.reg(sub, state, node)

    else:
      var state = state
      case node.kind:
        of nkEnumTy: state += rskEnumFields
        of nkObjectTy, nkRecList: state += rskObjectFields
        else: discard

      for subnode in node:
        db.reg(subnode, state, node)

proc registerUses*(db: var DocDb, node: PNode, state: RegisterState) =
  reg(db, node, state, nil)

type
  CodeWriter = object
    code: string ## Final chunk of the code to write out
    line: int ## Current active line *index*
    column: int ## Current column
    indent: int ## Relative indentation - used for arranging multiple
    ## chunks of macro expansion code, not for indentation-based
    ## formatting.
    file: FileIndex


proc add(writer: var CodeWriter, text: string) =
  ## Add piece of unformatted text to the last line, expanding last
  ## occurence if it does not have any occurence information
  writer.code.add text
  writer.column += text.len

proc addLine(writer: var CodeWriter) =
  ## Add new empty line to the code writer, accounting for active
  ## indentation
  if 0 < len(writer.code):
    writer.code.add "\n"

  inc writer.line
  writer.column = 0
  writer.add(repeat(" ", writer.indent))

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
              let ocId = db.occur(node, dokInMacroExpansion, RegisterState())

              db[ocId].inExpansionOf = some expand

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


proc registerExpansions*(db: var DocDb) =
  var writer = CodeWriter()
  for (module, expansions) in db.toplevelExpansions:
    for expand in expansions:
      db.writeCode(expand, writer)
