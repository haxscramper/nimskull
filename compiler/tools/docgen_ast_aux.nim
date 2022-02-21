import
  ast/[
    ast,
    renderer
  ],
  utils/[
    astrepr
  ]

proc failNode*(node: PNode) {.
    # deprecated: "Temporary hack to speed up development",
    noreturn
  .} =

  var conf = implicitTReprConf
  conf.maxPath = 3
  echo treeRepr(nil, node, conf)
  assert false

func getSName*(p: PNode): string =
  ## Get string value from `PNode`
  case p.kind:
    of nkIdent:         result = p.ident.s
    of nkSym:           result = p.sym.name.s
    of nkStrKinds:      result = p.strVal
    of nkOpenSymChoice: result = p[0].sym.name.s
    of nkAccQuoted:
      for sub in p:
        result.add getSName(sub)

    else:
      assert false, "Unexpected kind for 'getSName' - " & $p.kind

proc getSName*(p: PSym): string = p.name.s


type
  NodePos* = enum
    PosLastIdent
    PosType
    PosInit
    PosProcBody
    PosProcReturn
    PosProcArgs
    PosTypeBody
    PosName
    PosBody

  NodeSlice* = enum
    SliceAllIdents
    SliceAllArguments
    SliceAllBranches
    SliceBranchExpressions

func `[]`*(node: PNode, pos: NodePos): PNode =
  case pos:
    of PosLastIdent: node[^3]
    of PosType: node[^2]
    of PosInit: node[^1]
    of PosProcBody: node[6]
    of PosTypeBody: node[2]
    of PosProcArgs: node[3]
    of PosBody: node[^1]
    of PosProcReturn:
      assert node.kind == nkFormalParams
      node[0]
    of PosName: node[0]

template `[]`*(node: PNode, slice: static[NodeSlice]): untyped =
  when slice == SliceAllIdents:
    node[0..^3]

  elif slice in {SliceAllArguments, SliceAllBranches}:
    node[1..^1]

  elif slice == SliceBranchExpressions:
    node[0 .. ^2]

  else:
    {.error: $slice.}
