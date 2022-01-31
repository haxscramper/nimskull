import
  ast/[
    ast,
    lexer,
    renderer
  ],
  utils/[
    astrepr
  ],
  std/[
    sequtils
  ],
  experimental/text_layouter

initBlockFormatDSL()

proc max(args: varargs[int]): int =
  for arg in args:
    if result < arg:
      result = arg

proc assertKind(node: PNode, kind: set[TNodeKind]) =
  assert node.kind in kind, $treeRepr(nil, node)

func `[]`(n: ast.PNode, sl: HSlice[int, BackwardsIndex]): seq[PNode] =
  var idx = sl.a
  let maxl = n.len - sl.b.int
  while sl.a <= idx and idx <= maxl:
    result.add n[idx]
    inc idx

func high(n: PNode): int = n.len - 1

proc `of`*[A: object or ref object or distinct; K: enum](item: A, kind: K | set[K]): bool =
  ## Check if @arg{item} has @arg{kind}
  when kind is set:
    item.kind in kind

  else:
    item.kind == kind

type
  NimFormatFlag* = enum
    nffAllowMultilineProcHead

    nffHorizontalPackedOf
    nffVerticalPackedOf

    nffAddIInfo

  NimFormatConf* = object
    flags*: set[NimFormatFlag]
    baseIndent*: int

const
  defaultNimFormatConf* = NimFormatConf(
    flags: {
      nffAllowMultilineProcHead,
      nffAddIInfo,
      nffHorizontalPackedOf,
      nffVerticalPackedOf
    }
  )

func contains*(conf: NimFormatConf, flag: NimFormatFlag): bool =
  flag in conf.flags

func contains*(conf: NimFormatConf, flags: set[NimFormatFlag]): bool =
  len(flags * conf.flags) == len(flags)


proc toLytBlock(n: PNode, conf: NimFormatConf): LytBlock
template `~`(node: PNode): untyped = toLytBlock(node, conf)

const
  SpaceStr = LytStrIdMask(0)
  TokenStr = LytStrIdMask(1)
  IdentStr = LytStrIdMask(2)


proc toLytStr(count: int): LytStr =
  result.len = count
  result.id.setMask(SpaceStr)

proc toLytStr(tok: TokType): LytStr =
  result.id = LytStrId(tok.int)
  result.len = len($tok) # todo optimize
  result.id.setMask(TokenStr)

proc toLytStr(ident: PIdent): LytStr =
  result.id = LytStrId(ident.id)
  result.len = len(ident.s)
  result.id.setMask(IdentStr)

proc toLytStr(node: PNode): LytStr =
  case node.kind:
    of nkSym: result = toLytStr(node.sym.name)
    of nkIdent: result = toLytStr(node.ident)
    else: assert false, $node.kind

proc lT(args: varargs[LytStr, toLytStr]): LytBlock =
  discard

proc lytInfix(
    n: PNode, conf: NimFormatConf, spacing: bool = true): LytBlock =

  if n.kind == nkInfix:
    if spacing:
      result = lH(
        lytInfix(n[1], conf),
        lT(tkSpaces, n[0], tkSpaces),
        lytInfix(n[2], conf)
      )

    else:
      result = lH(lytInfix(n[1], conf), lT(n[0]), lytInfix(n[2], conf))

  else:
    result = toLytBlock(n, conf)

proc lytFormalParams(
    n: PNode,
    vertical: bool,
    conf: NimFormatConf
  ): LytBlock =

  assertKind(n, {nkFormalParams})
  result = tern(vertical, lV(), lH())
  let argPad =
    if vertical:
      n[1..^1].mapIt(len($it[0])).max() + 2

    else:
      0


  for idx, arg in n[1..^1]:
    var hor = lH(lT(alignLeft($arg[0] & ": ", argPad)), lT(arg[1]))
    if not(arg[2] of nkEmpty):
      hor.add lT(1, tkEquals, 1)
      hor.add toLytBlock(arg[2], conf)

    if idx < n.high - 1:
      hor &= tern(vertical, lT(tkComma), lT(tkComma, 1))

    result.add hor

proc lytFormalReturnClose(n: PNode, conf: NimFormatConf): LytBlock =
  assertKind(n, {nkFormalParams})
  if n[0] of nkEmpty:
    lT(")")

  else:
    lH(lT("): "), toLytBlock(n[0], conf))


proc lytDocComment(n: PNode, prefix: string = ""): LytBlock =
  if n.comment.len > 0:
    result = lV()
    for line in n.comment.split('\n'):
      result.add lT(prefix & "## " & line)

  else:
    result = lE()

proc lytCsv(n: PNode | seq[PNode], vertical: bool, conf: NimFormatConf): LytBlock =
  result = tern(vertical, lV(), lH())
  if vertical:
    for idx, item in n:
      result.add tern(
        idx < len(n) - 1,
        lH(toLytBlock(item, conf), lT(", ")),
        toLytBlock(item, conf)
      )

  else:
    for idx, item in n:
      result.add tern(
        idx > 0,
        lH(lT(", "), toLytBlock(item, conf)),
        toLytBlock(item, conf)
      )

proc lytTypedefHead(n: PNode, conf: NimFormatConf): LytBlock =
  if n[0] of nkPragmaExpr:
    lH(~n[0][0], ~n[1], lT(" "), ~n[0][1])

  else:
    lH(~n[0], ~n[1])

const
  nkTypeAliasKinds = {
    nkIdent, nkPtrTy, nkRefTy, nkBracketExpr}


proc lytTypedef(n: PNode, conf: NimFormatConf): LytBlock =
  var head = lytTypedefHead(n, conf)
  head.add lT(" = ")

  case n[2].kind:
    of nkObjectTy:
      head.add lT("object")
      var body: seq[seq[LytBlock]]
      var resBody = lV()

      template flush(): untyped =
        if ?body:
          resBody.add makeAlignedGrid(
            body, [sadLeft, sadLeft, sadLeft])

          body.clear()

      for field in n[2][2]:
        case field.kind:
          of nkEmpty:
            discard

          of nkRecCase:
            flush()
            resBody.add lS()
            resBody.add ~field

          of nkIdentDefs:
            body.add @[
              lH(~field[0], lT(": ")),
              ~field[1],
              lytDocComment(field, prefix = " ")]

          of nkCommentStmt:
            flush()
            resBody.add lytDocComment(field, prefix = "")

          else:
            raise newImplementKindError(field)

      flush()
      result = lV(head, lI(2, resBody))

    of nkEnumTy:
      head.add lT("enum")
      var body: seq[seq[LytBlock]]
      for field in n[2]:
        case field.kind:
          of nkIdent:
            let f = ~field
            body.add @[f, lT(""), lT("")]

          of nkEnumFieldDef:
            body.add @[~field[0], lT(" = "), ~field[1]]

          of nkEmpty:
            discard

          else:
            raise newUnexpectedKindError(field)

        if field.kind != nkEmpty:
          body.last().add lytDocComment(field, prefix = " ")

      result = lV(head, lI(2, makeAlignedGrid(
        body, [sadLeft, sadCenter, sadLeft, sadLeft])))

    of nkTypeAliasKinds:
      head.add ~n[2]
      result = head

    of nkProcTy:
      result = lV(head, lI(2, ~n[2]))

    of nkDistinctTy:
      result = lH(head, lT("distinct "), ~n[2][0])

    else:
      raise newImplementKindError(n[2], $n.treeRepr())


proc toLytBlock(n: PNode, conf: NimFormatConf): LytBlock =
  assertRef n

  case n.kind:
    of nkProcTy:
      result = lV(lH(lT("proc("),
          lytFormalParams(n[0], true, conf),
          lytFormalReturnClose(n[0], conf),
          lT(" "),
          ~n[1]
))

    of nkAccQuoted:
      result = lH(lT("`"))
      for arg in n:
        result.add ~arg

      result.add lT("`")

    of nkProcDef, nkLambda, nkConverterDef, nkFuncDef:
      result = makeChoiceBlock([])

      let kindName =
        case n.kind:
          of nkProcDef: "proc"
          of nkConverterDef: "converter"
          of nkLambda: ""
          of nkFuncDef: "func"
          else: raise newImplementKindError(n)

      # proc q*(a: B): C {.d.} =
      #   e
      result.add lV(lH(lH(lT(kindName), lS(), ~n[0], ~n[1], ~n[2], lT("(")),
          lytFormalParams(n[3], false, conf),
          lytFormalReturnClose(n[3], conf),
          if n[4] of nkEmpty: lT("") else: lH(lT(" "), ~n[4]),
          if n[6] of nkEmpty: lT("") else: lT(" = ")),
        lV(lytDocComment(n), lI(2, ~n[6])),
        lS())


      if n[3].len > 1 and nffAllowMultilineProcHead in conf:
        # proc q*(
        #     a: B
        #   ): C {.d.} =
        #     e
        result.add lV(lH(lT(kindName), lS(), ~n[0], ~n[1], ~n[2], lT("(")),
          lI(4, lytFormalParams(n[3], true, conf)),
          lH(lI(2, lytFormalReturnClose(n[3], conf)),
            if n[4] of nkEmpty: lT("") else: lH(lT(" "), ~n[4]),
            if n[6] of nkEmpty: lT("") else: lT(" = ")),
          lytDocComment(n),
          lI(2, ~n[6]),
          lS())

      when false:
        # proc q*(a: B):
        #     C {.d.} =
        #   e
        result.add lV(lH(headLyt, horizArgsLyt, postArgs),
          lI(2, lH(retLyt, pragmaLyt, eq)), implLyt)

        # proc q*(a: B):
        #     C
        #     {.d.} =
        #   e
        result.add lV(lH(headLyt, horizArgsLyt, postArgs),
          lI(2, retLyt),
          lI(2, lH(pragmaLyt, eq)),
          implLyt)

        if vertArgsLyt.len > 0:
          # proc q*(
          #     a: B
          #   ): C {.d.} =
          #     e
          result.add lV(headLyt,
            lI(4, vertArgsLyt),
            lI(2, lH(postArgs, retLyt, pragmaLyt, eq)),
            lS(),
            lI(2, implLyt))

          # proc q*(
          #       a: B
          #   ):
          #     C
          #     {.d.} =
          #   e
          result.add lV(headLyt,
            tern(vertArgsLyt.len == 0, lE(), vertArgsLyt),
            postArgs,
            lI(2, retLyt),
            lI(2, lH(pragmaLyt, eq)),
            implLyt
          )

    of nkStmtList:
      result = lV()
      var hadReal = false
      for sub in items(n):
        if sub of nkEmpty and not hadReal:
          discard

        else:
          hadReal = true
          result.add ~sub

    of nkForStmt:
      result = lV(lH(lT("for "), ~n[0], # IMPLEMENT multiple identifiers
          lT(" in "), ~n[^2],
          lT(":")),
        lI(2, ~n[^1]))

    of nkWhileStmt:
      result = lV(lH(lT("while "), ~n[0], lT(":")),
        lI(2, ~n[1]))

    of nkCaseStmt, nkRecCase:
      var regular = lV()
      if nffVerticalPackedOf in conf:
        var arms = lV()
        for arm in n[1..^1]:
          arms.add ~arm

        if n of nkRecCase:
          regular = lV(lH(lT("case "), ~n[0]), lI(2, arms))

        else:
          regular = lV(lH(lT("case "), ~n[0], lT(":")), lI(2, arms))

      var grid = lV()

      if nffHorizontalPackedOf in conf:
        var ofarms: seq[seq[LytBlock]]
        var elsearms: seq[LytBlock]

        for arm in n[1 .. ^1]:
          if arm of nkOfBranch:
            ofarms.add @[
              lH(lT("of "), lytCsv(arm[0 .. ^2], false, conf), lT(": ")),
              ~arm[^1]
            ]

          else:
            elsearms.add ~arm

        grid = lV(lH(lT("case "), ~n[0], lT(":")),
          lI(2,
            lV(makeAlignedGrid(ofarms, @[sadLeft, sadLeft]),
              lV(elsearms))))

      if {nffHorizontalPackedOf, nffVerticalPackedOf} in conf:
        result = lC(regular, grid)

      elif nffHorizontalPackedOf in conf:
        result = grid

      else:
        result = regular
      # result = grid

    of nkElse:
      result = lV(lT("else:"), lI(2, ~n[0]))

    of nkOfBranch:
      let expr = joinItLine(n[0..^2], ~it, lT(", "))
      let b = ~n[^1]

      case n[^1].kind:
        of nkRecList:
          result = lV(lH(lT("of "), expr, lT(":")), b)

        else:
          result = lV(lH(lT("of "), expr, lT(":")), lI(2, b))

    of nkIfStmt, nkWhenStmt:
      result = lV()
      for isFirst, branch in itemsIsFirst(n):
        if branch.kind == nkElse:
          result.add lV(lT("else:"),
            lI(2, ~branch[0]),
            lT(""))

        else:
          result.add lV(lH(lT(tern(
              isFirst,
              tern(n of nkIfStmt, "if ", "when "),
              "elif ")), ~branch[0], lT(":")),

            lI(2, ~branch[1]),
            lT(""))

    of nkLetSection, nkConstSection, nkVarSection:
      var decls: seq[LytBlock]

      for le in n:
        decls.add ~le


      let name = mapEnum(n.kind, {
        nkLetSection: "let",
        nkVarSection: "var",
        nkConstSection: "const"
      })

      if decls.len == 1:
        result = lH(lT(name & " "), decls[0])

      else:
        result = lH(lT(name), lI(2, lV(decls)))


    of nkIdentDefs, nkConstDef:
      if n[2].kind == nkLambda:
        result = lV(tern(
            n[1].kind == nkEmpty,
            lH(lT(n[0].str), lT(" = ")),
            lH(lT(n[0].str), lT(" : "), ~n[1], lT(" = "))
          ),
          ~n[2])
      else:
        result = lH(lT(n[0].str),
           tern(n[1] of nkEmpty, lT(""), lH(lT(": "), ~n[1])),
           tern(n[2] of nkEmpty, lT(""), lH(lT(" = "), ~n[2])),
           lytDocComment(n, " "))

    of nkEnumFieldDef:
      result = lH(~n[0], lT(" = "), ~n[1])

    of nkTypeSection:
      if len(n) == 0:
        result = lE()

      else:
        result = lV()
        var buffer: seq[seq[LytBlock]]
        for idx, def in n:
          if def[2] of nkTypeAliasKinds:
            buffer.add @[lytTypedefHead(def, conf), lT(" = "), ~def[2]]
            buffer.add @[lS()]

          if not(def[2] of nkTypeALiasKinds) or idx == len(n) - 1:
            if ?buffer:
              result.add makeAlignedGrid(buffer, [sadLeft, sadLeft, sadLeft])
              result.add lS()

              buffer.clear()

          if not(def[2] of nkTypeAliasKinds):
            result.add ~def
            result.add lS()

        result = lV(lT("type"), lI(2, result))

    of nkTypeDef:
      result = lytTypedef(n, conf)

    of nkPragmaExpr:
      result = lH(~n[0], lT(" "), ~n[1])

    of nkRecList:
      result = lV(lytDocComment(n))
      for fld in n:
        result.add ~fld

      result = lI(2, result)

    of nkPragma:
      result = lH()
      result.add lT("{.")
      var idx = 0
      while idx < len(n):
        var res: LytBlock
        let item = n[idx]
        if item of nkIdent and item.getStrVal() == "push":
          let next = n[idx + 1]
          res = lH(~item, lT(" "), tern(
            next of nkExprColonExpr,
            lH(~next[0], lT(":"), ~next[1]), ~next))

          inc idx

        else:
          res = ~item

        result.add tern(idx < n.len - 1, lH(res, lT(", ")), res)
        inc idx

      result.add lT(".}")

    of nkImportStmt:
      var imports = lV()
      for idx, path in n:
        if path of nkInfix:
          imports.add lH(lytInfix(path, conf, spacing = false),
            tern(idx < n.len - 1, lT(","), lT("")))

        else:
          imports.add ~path

      result = lV(lT("import"), lI(2, imports))

    of nkExportStmt:
      result = lH(lT("export "))
      for idx, exp in n:
        if idx > 0:
          result.add lH(lT(", "), ~exp)

        else:
          result.add ~exp

    of nkCommentStmt:
      result = lV()
      for line in split(n.comment, '\n'):
        result.add lH(lT("## " & line))

    of nkExprEqExpr: result = lH(~n[0], lT(" = "), ~n[1])

    of nkExprColonExpr: result = lH(~n[0], lT(": "), ~n[1])
    of nkPrefix: result        = lH(~n[0], ~n[1])
    of nkPostfix: result       = lH(~n[1], ~n[0])
    of nkInfix: result         = lytInfix(n, conf)
    of nkIdent: result         = lT(n.getStrVal())
    of nkDotExpr: result       = lH(~n[0], lT("."), ~n[1])
    of nkEmpty: result         = lT("")
    of nkIntLit: result        = lT($n.intVal)
    of nkPtrTy: result         = lH(lT("ptr "), ~n[0])
    of nkStrLit: result        = lT(n.getStrVal().escape())
    of nkNilLit: result        = lT("nil")
    of nkReturnStmt: result    = lH(lT("return "), ~n[0])
    of nkBreakStmt: result     = lH(lT("break "), ~n[0])
    of nkDiscardStmt: result   = lH(lT("discard "), ~n[0])
    of nkAsgn: result = lH(~n[0], lT(" = "), ~n[1])

    of nkBracket:
      result = lC(lH(lT("["), lytCsv(n, false, conf), lT("]")),
        lV(lT("["), lI(2, lytCsv(n, true, conf)), lT("]")))

    of nkGenericParams:
      result = lH(lT("["), lytCsv(n, false, conf), lT("]"))

    of nkCast:
      if n[0] of nkEmpty:
        result = lH(lT("cast("), ~n[1], lT(")"))

      else:
        result = lH(lT("cast["), ~n[0], lT("]("), ~n[1], lT(")"))


    of nkCurly:
      if len(n) > 6:
        result = lV(lT("{"), lI(2, lytCsv(n, true, conf)), lT("}"))

      else:
        result = lH(lT("{"), lytCsv(n, false, conf), lT("}"))


    of nkBlockStmt:
      result = lV(lH(lT("block"), tern(n[0].isEmptyNode(), lE(), lH(lT(" "), ~n[0])), lT(":")),
        lI(2, ~n[1]))

    of nkBracketExpr:
      result = lH(~n[0], lT("["), lytCsv(n[1..^1], false, conf), lT("]"))

    of nkPar:
      result = lH(lT("("), lytCsv(n[0..^1], false, conf), lT(")"))

    of nkCommand:
      result = lH(~n[0], lT(" "), lytCsv(n[1..^1], false, conf))

    of nkCall:
      result = lH(~n[0], lT("("), lytCsv(n[1..^1], false, conf), lT(")"))

    of nkPragmaBlock:
      result = lV(lH(~n[0], lT(":")), lI(2, ~n[1]))

    else:
      raise newImplementKindError(
        n, treeRepr(n, maxdepth = 3))

  assertRef result, $n.kind
