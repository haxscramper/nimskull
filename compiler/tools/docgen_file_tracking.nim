import
  ./docgen_types,
  ./docgen_ast_aux,
  ast/[
    ast,
    renderer,
    lineinfos,
    types,
  ],
  sem/[
    sighashes,
  ],
  modules/[
    modulegraphs
  ],
  utils/[
    astrepr
  ],
  std/[
    strutils,
    options,
    hashes,
    tables,
  ]


proc infoLocation*(info: TLineInfo): DocLocation =
  DocLocation(
    file: info.fileIndex,
    line: info.line.int,
    column: info.col.int..info.col.int
  )

proc nodeLocation*(node: PNode): DocLocation =
  result = infoLocation(node.info)
  result.column.b += len($node) - 1

proc nodeLocation*(node: PSym): DocLocation =
  result = infoLocation(node.info)
  result.column.b += len(node.name.s) - 1

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


func getHashdata(s: PSym): auto = (
    s.kind,
    s.name.id,
    s.info.fileIndex.int,
    s.info.line,
    s.info.col,
  )


proc hash*(s: PSym): Hash = hash(getHashdata(s))
proc `==`*(s1, s2: PSym): bool = getHashdata(s1) == getHashdata(s2)

proc hashdata*(s: PSym): string =
  let d = getHashdata(s)
  "[$#.$#.$#.$#.$#] = $#" % [$d[0], $d[1], $d[2], $d[3], $d[4], $hash(s)]



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



proc addSigmap*(db: var DocDb, sym: PSym, entry: DocEntryId) =
  try:
    if not isNil(sym):
      db.sigmap[sym] = entry

  except IndexDefect as e:
    discard

proc addSigmap*(db: var DocDb, node: PNode, entry: DocEntryId) =
  ## Add mapping between specific symbol and documentable entry. Symbol
  ## node is retrived from the ast.
  db.addSigmap(node.headSym(), entry)

proc sigHash(t: PNode): SigHash =
  result = t.headSym().trySigHash()

proc sigHash(t: PSym): SigHash =
  result = t.trySigHash()

proc newDocEntry*(
    db: var DocDb, kind: DocEntryKind, name: PNode | PSym,
    context: DocDeclarationContext = DocDeclarationContext()
  ): DocEntryId =
  ## Construct new documentable entry using symbol or identifier node to
  ## get name, location. If node is a symbol also updates sigmap with new
  ## documentable entry.
  when name is PNode:
    assert name.kind in {nkSym, nkIdent, nkAccQuoted}, $name.kind

  result = db.newDocEntry(kind, name.getSName(), context)
  db[result].location = some db.add(name.nodeLocation())
  when name is PSym:
    db.addSigmap(name, result)

  else:
    if name.kind == nkSym:
      db.addSigmap(name, result)


proc newDocEntry*(
    db: var DocDb,
    parent: DocEntryId, kind: DocEntryKind, name: PNode | PSym,
    context: DocDeclarationContext = DocDeclarationContext()
  ): DocEntryId =
  result = db.newDocEntry(kind, name, context)
  db[result].parent = some parent
  db[parent].nested.add result

proc contains*(db: DocDb, ntype: PType | PNode | PSym): bool =
  let sym = ntype.headSym()
  return not sym.isNil() and sym in db.sigmap


proc `[]`*(db: DocDb, ntype: PType | PNode | PSym): DocEntryId =
  let sym = headSym(ntype)
  if not sym.isNil() and sym in db.sigmap:
    return db.sigmap[sym]

  else:
    assert false, "no doc entry for node $# symdata was $# (in table $#)" % [
      $treeRepr(nil, ntype),
      hashdata(sym),
      $(sym in db.sigmap)
    ]



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





proc nodeExprSlice(node: PNode): DocLocation =
  ## Return source code slice for `node`.
  result = nodeLocation(node)
  case node.kind:
    of nkDotExpr:
      result -= len($node[0]) - 1

    else:
      discard


proc subslice*(parent, node: PNode): DocLocation =
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

