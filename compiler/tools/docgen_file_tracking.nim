import
  ./docgen_types,
  ast/[
    ast,
    renderer,
    lineinfos,
    types
  ],
  sem/[
    sighashes,
  ],
  modules/[
    modulegraphs
  ],
  std/[
    strutils,
    options,
    hashes,
    tables,
  ]

const
  nkStringKinds*  = nkStrKinds
  nkFloatKinds*   = { nkFloatLit .. nkFloat128Lit }
  nkLiteralKinds* = nkStrKinds + nkIntKinds + nkFloatKinds + {nkNilLit}
  nkTokenKinds*   = nkLiteralKinds + {nkIdent, nkSym}

  nkProcDeclKinds* = {
    nkProcDef,
    nkFuncDef,
    nkIteratorDef,
    nkTemplateDef,
    nkMacroDef,
    nkMethodDef,
    nkConverterDef
  }

  nkStmtBlockKinds* = {
    nkIfExpr,
    nkIfStmt,
    nkWhenStmt,
    nkWhenExpr,
    nkForStmt,
    nkBlockStmt
  }

  nkIdentDeclKinds* = {
    nkLetSection,
    nkVarSection,
    nkConstSection,
    nkIdentDefs
  }


  nkAllDeclKinds* = nkProcDeclKinds + nkIdentDeclKinds

  skProcDeclKinds* = {
    skProc,
    skTemplate,
    skMethod,
    skMacro,
    skIterator,
    skConverter,
    skFunc
  }


proc trySigHash*(sym: PSym): SigHash =
  if not isNil(sym):
    try:
      result = sym.sigHash()

    except IndexDefect as e:
      discard


proc hash*(s: PSym): Hash =
  hash(s.itemId.item) !& hash(s.itemId.module)

proc headSym(s: PSym): PSym = s
proc headSym(t: PType): PSym = t.sym.headSym()

proc headSym*(node: PNode): PSym =
  case node.kind:
    of nkProcDeclKinds, nkDistinctTy, nkVarTy, nkAccQuoted,
       nkBracketExpr, nkTypeDef, nkPragmaExpr, nkPar, nkEnumFieldDef,
       nkIdentDefs, nkRecCase, nkCallStrLit:
      result = headSym(node[0])

    of nkCommand, nkCall, nkPrefix, nkPostfix,
       nkHiddenStdConv, nkInfix:
      if node.len == 0:
        result = nil

      elif node.kind == nkCall:
        if node.len > 1 and node[1].kind == nkSym:
          result = node[1].sym

        else:
          result = headSym(node[0])

      else:
        result = headSym(node[0])

    of nkDotExpr:
      result = headSym(node[1])

    of nkSym:
      result = node.sym

    of nkRefTy, nkPtrTy:
      if node.len == 0:
        result = nil

      else:
        result = headSym(node[0])

    of nkIdent, nkEnumTy, nkProcTy, nkObjectTy, nkTupleTy,
       nkTupleClassTy, nkIteratorTy, nkOpenSymChoice,
       nkClosedSymChoice, nkCast, nkLambda, nkCurly,
       nkReturnStmt, nkRaiseStmt, nkBracket, nkEmpty,
       nkIfExpr
         :
      result = nil

    of nkCheckedFieldExpr:
      # First encountered during processing of `locks` file. Most likely
      # this is a `object.field` check
      result = nil

    of nkStmtListExpr:
      if isNil(node.typ):
        if node.len > 0:
          result = headSym(node[1])

        else:
          result = nil

      else:
        result = node.typ.skipTypes({tyRef}).sym

    of nkType, nkObjConstr:
      result = node.typ.skipTypes({tyRef}).sym


    else:
      assert false, "TODO " & $node.kind


proc addSigmap*(db: var DocDb, node: PNode, entry: DocEntryId) =
  ## Add mapping between specific symbol and documentable entry. Symbol
  ## node is retrived from the ast.
  try:
    let sym = node.headSym()
    if not isNil(sym):
      db.sigmap[sym] = entry

  except IndexDefect as e:
    discard


proc sigHash(t: PNode): SigHash =
  result = t.headSym().trySigHash()

proc sigHash(t: PSym): SigHash =
  result = t.trySigHash()


proc contains*(db: DocDb, ntype: PType | PNode | PSym): bool =
  ntype.headSym() in db.sigmap

proc `[]`*(db: DocDb, ntype: PType | PNode | PSym): DocEntryId =
  let sym = headSym(ntype)
  if not sym.isNil() and sym in db.sigmap:
    return db.sigmap[sym]


proc contains(s1, s2: DocLocation): bool =
  s1.line == s2.line and
  s1.column.a <= s2.column.a and s2.column.b <= s1.column.b

func `[]`*[R1, R2](slice: DocLocation, split: HSlice[R1, R2]): DocLocation =
  result = slice
  when R1 is BackwardsIndex:
    result.column.a = result.column.b - split.a.int

  else:
    result.column.a = result.column.a + split.a

  when R2 is BackwardsIndex:
    result.column.b = result.column.b - split.b.int

  else:
    result.column.a = result.column.a + split.b

func `-=`*(slice: var DocLocation, shift: int) =
  slice.column.a -= shift
  slice.column.b -= shift

proc splitOn*(base, sep: DocLocation):
  tuple[before, after: Option[DocLocation]] =

  if base.column.a == sep.column.a and
     base.column.b == sep.column.b:
    discard

  else:
    if base.column.a < sep.column.a:
      # [base text  ... (anything)]
      #          < [separator text]
      result.before = some initDocLocation(
        base.line, base.column.a, sep.column.a - 1, base.file)

    elif base.column.a == sep.column.a:
      discard

    else:
      assert false, "TODO"

    if sep.column.b < base.column.b:
      # [... (anything)  base text]
      # [separator text] <
      result.after = some initDocLocation(
        base.line, sep.column.b + 1, base.column.b, base.file)

    elif sep.column.b == base.column.b:
      discard

    else:
      assert false, "TODO"




proc nodeLocation*(node: PNode): DocLocation =
  let l = len($node)
  initDocLocation(
    node.info.line.int,
    node.info.col.int,
    node.info.col.int + l - 1,
    node.info.fileIndex
  )

proc nodeExprSlice(node: PNode): DocLocation =
  ## Return source code slice for `node`.
  result = nodeLocation(node)
  case node.kind:
    of nkDotExpr:
      result -= len($node[0]) - 1

    else:
      discard


proc subslice(parent, node: PNode): DocLocation =
  let main = parent.nodeExprSlice()
  case parent.kind:
    of nkDotExpr: result = main[^(len($node)) .. ^1]
    of nkExprColonExpr:
      result.line = main.line
      result.column.b = main.column.a
      result.column.a = main.column.a - len($node)

    else:
      result = main

proc startPos*(node: PNode): TLineInfo =
  case node.kind:
    of nkTokenKinds:
      result = node.info

    of nkAccQuoted:
      result = node[0].startPos()

    else:
      result = node[0].startPos()

proc finishPos*(node: PNode): TLineInfo =
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

proc nodeExtent*(node: PNode): DocExtent =
  let (start, finish) = (startPos(node), finishPos(node))
  result.file = start.fileIndex
  result.start = (start.line.int, start.col.int)
  result.finish = (finish.line.int, finish.col.int)

proc occur*(
    db: var DocDb,
    node: PNode,
    kind: DocOccurKind,
    user: Option[DocEntryId]
  ): DocOccurId =

  var occur = DocOccur(
    user: user, kind: kind, loc: db.add(nodeLocation(node)))

  if kind in dokLocalKinds:
    assert false, "Unexpected kind for occur " & $kind

  else:
    occur.refid = db[node]

  return db.occurencies.add occur

proc occur*(
    db: var DocDb,
    node: PNode,
    parent: PNode,
    id: DocEntryId,
    kind: DocOccurKind,
    user: Option[DocEntryId] = none DocEntryId
  ): DocOccurId =
  ## Construct new docmentable entry occurence and return new ID
  var occur = DocOccur(
    kind: kind, user: user, loc: db.add(parent.subslice(node)))

  occur.refid = id
  return db.add occur

proc occur*(
    db: var DocDb,
    node: PNode,
    id: DocEntryId,
    kind: DocOccurKind,
    user: Option[DocEntryId] = none DocEntryId
  ): DocOccurId =
  ## Construct new docmentable entry occurence and return new ID
  var occur = DocOccur(
    kind: kind, user: user, loc: db.add(nodeLocation(node)))

  occur.refid = id
  return db.add occur

proc occur*(
    db: var DocDb,
    node: PNode,
    kind: DocOccurKind,
    localid: string,
    withInit: bool = false
  ): DocOccurId =
  ## Construct new occurence of the local documentable entry and return the
  ## resulting ID
  var occur = DocOccur(
    kind: kind, loc: db.add(nodeLocation(node)))

  occur.localId = localid
  occur.withInit = withInit

  if kind in dokLocalDeclKinds:
    if not isNil(node.typ):
      let et = node.typ.skipTypes(skipPtrs)
      if not isNil(et.sym):
        let id = db[et.sym]
        if not id.isNil():
          occur.user = some id

  return db.add occur
