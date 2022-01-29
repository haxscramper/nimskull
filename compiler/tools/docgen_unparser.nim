import
  ast/[
    ast,
    types
  ],
  utils/[
    astrepr
  ]

type
  DefTreeKind* = enum
    deftProc
    deftObject
    deftEnum
    deftArg
    deftLet
    deftVar
    deftConst
    deftField
    deftEnumField

  DefFieldKind* = enum
    deffPlain
    deffWrapOf
    deffWrapElse
    deffWrapWhen

const deftIdentKinds* = { deftArg, deftLet, deftVar, deftConst, deftField }

type
  DefName* = object
    ident*: PNode
    exported*: bool
    pragmas*: seq[PNode]

  DefField* = object
    head*: DefTree
    case kind*: DefFieldKind
      of deffWrapOf, deffWrapWhen, deffWrapElse:
        branchExprs*: seq[PNode]
        subfields*: seq[DefField]

      else:
        discard

  DefTree = object
    defName*: DefName
    comment*: string
    defNode*: PNode

    case kind*: DefTreeKind
      of deftProc:
        arguments*: seq[DefTree]

      of deftIdentKinds:
        typ*: PNode
        initExpr*: PNode

      of deftEnum:
        enumFields*: seq[DefTree]

      of deftEnumField:
        strOverride*: PNode
        intOverride*: PNode

      of deftObject:
        objFields*: seq[DefField]
        objBase*: PNode

func newDef*(kind: DefTreeKind): DefTree =
  DefTree(kind: kind)

proc failNode(node: PNode) =
  debug node
  assert false

proc unparseName*(node: PNode): DefName =
  case node.kind:
    of nkPragmaExpr:
      case node[0].kind:
        of nkPostfix:
          result.ident = node[0][1]
          result.exported = true

        of nkIdent, nkSym:
          result.ident = node[0]

        else:
          failNode node

    of nkPostfix:
      result.ident = node[1]
      result.exported = true

    of nkIdentDefs:
      result = unparseName(node[0])

    else:
      failNode node

proc skipNodes*(t: PNode, kinds: set[TNodeKind]): PNode =
  result = t
  while result.kind in kinds:
    result = lastSon(result)

proc unparseFields*(node: PNode): seq[DefField] =
  assert false

proc unparseDefs*(node: PNode): seq[DefTree] =
  case node.kind:
    of nkStmtList, nkTypeSection:
      for sub in node:
        result.add unparseDefs(node)

    of nkTypeDef:
      if node[2].skipNodes({nkPtrTy, nkRefTy}).kind in {nkObjectTy}:
        var res = newDef(deftObject)

        res.objFields = unparseFields(node[2][2])

        failNode node

        result.add res

      else:
        failNode node

    else:
      failNode node


