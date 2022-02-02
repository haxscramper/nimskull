## This module provides a list of type definitions and convenience
## procedures for dealing with /entities/ declared in the code - objects,
## enums, procedures. It provides additional layer of abstraction on top of
## the AST, allowing user to work in terms `"if obj.exported"` as opposed
## to `if obj[0].kind == nnkPrefix`. Recursively iterate fields,
## definitions.

import
  ast/[
    ast,
    types,
    renderer
  ],
  utils/[
    astrepr
  ],
  ./docgen_file_tracking,
  ./docgen_ast_aux

type
  DefTreeKind* = enum
    ## Definition tree kind
    deftProc ## Procedure-like element
    deftObject ## Object definitio
    deftEnum
    deftArg ## Procedure argument
    deftLet ## 'let' variable declaration
    deftVar ## 'var' declaration
    deftMagic ## Declaration of the magic type
    deftConst ## 'const' declaration
    deftField ## Object field declaration
    deftEnumField ## Enum field declaration
    deftAlias ## Type alias declaration

  DefFieldKind* = enum
    ## Kind of the object declaration
    deffPlain
    deffWrapCase
    deffWrapWhen

const deftIdentKinds* = { deftArg, deftLet, deftVar, deftConst, deftField }

type
  DefName* = object
    ## Unparsed definition name, split into semantically meaningfuly chunks
    ## that can be queried directly.
    ident*: PNode ## Name identifier - symbol/ident/accquoted node
    exported*: bool ## Whether or not original node was exported
    pragmas*: seq[PNode] ## list of name pragmas

  DefFieldBranch* = object
    branchExprs*: seq[PNode]
    subfields*: seq[DefField]


  DefField* = ref object
    head*: DefTree
    case kind*: DefFieldKind
      of deffWrapCase, deffWrapWhen:
        branches*: seq[DefFieldBranch]

      else:
        discard

  DefTree* = ref object
    defName*: DefName
    comment*: string
    defNode: PNode

    case kind*: DefTreeKind
      of deftProc:
        arguments*: seq[DefTree]
        returnType*: PNode

      of deftIdentKinds:
        typ*: PNode
        initExpr*: PNode

      of deftAlias:
        baseType*: PNode
        isDistinct*: bool

      of deftEnum:
        enumFields*: seq[DefTree]

      of deftEnumField:
        strOverride*: PNode
        valOverride*: PNode

      of deftObject:
        objFields*: seq[DefField]
        objBase*: PNode

      of deftMagic:
        discard

func exported*(def: DefTree): bool = def.defName.exported
func exported*(def: DefField): bool = def.head.exported
func name*(def: DefTree): DefName = def.defName

func nameNode*(def: DefName): PNode = def.ident
func nameNode*(def: DefTree): PNode = def.name.nameNode()

func sym*(def: DefName | DefTree): PSym = def.nameNode().sym

func hasSym*(def: DefTree | DefName): bool =
  ## Check if unparsed definition tree or name has symbol in it's name
  ## (`nkSym`)
  def.nameNode.kind == nkSym

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

func filterPragmas*(pragmas: seq[PNode], names: seq[string]): seq[PNode] =
  ## Filter out list of pragma nodes, returning only ones whose names
  ## were in the `names` list.
  for pragma in pragmas:
    if pragma.safeLen > 0:
      case pragma[0].kind:
        of nkSym, nkIdent:
          if pragma[0].getSName() in names:
            result.add pragma[0]

        of nkCall, nkCommand, nkExprColonExpr:
          if pragma[0][0].getSName() in names:
            result.add pragma[0]

        else:
          assert false, $pragma[0].kind


proc getSName*(p: PSym): string = p.name.s
func getSName*(def: DefName | DefTree): string =
  ## Get string name of definition tree or definition name
  def.nameNode().getSName()

func pragmas*(def: DefTree): seq[PNode] = def.name.pragmas
func node*(def: DefTree): PNode = def.defNode


func allFields*(def: DefTree): seq[DefField] =
  func aux(def: DefField, res: var seq[DefField]) =
    if def.kind in {deffWrapCase, deffWrapWhen}:
      for branch in def.branches:
        for field in branch.subfields:
          aux(field, res)

  for field in def.objFields:
    result.add field
    aux(field, result)

func newDef*(kind: DefTreeKind, name: DefName, node: PNode): DefTree =
  DefTree(kind: kind, defName: name, defNode: node)

proc unparseName*(node: PNode): DefName =
  ## Unparse name definition ast into `DefName`. Input node can be be a
  ## `PragmaExpr`, `Postfix` or `Ident/Sym/AccQuoted` node.
  case node.kind:
    of nkPragmaExpr:
      case node[0].kind:
        of nkPostfix:
          result.ident = node[0][1]
          result.exported = true

        of nkIdent, nkSym, nkAccQuoted:
          # NOTE not entirely sure about `AccQuoted` here, but technically
          # it /is/ name - no need to reassemble the identifier from pices
          # here.
          result.ident = node[0]

        else:
          failNode node

      for pragma in node[1]:
        result.pragmas.add pragma

    of nkPostfix:
      result.ident = node[1]
      result.exported = true

    of nkIdent, nkSym, nkAccQuoted:
      result.ident = node

    else:
      failNode node

proc skipNodes*(t: PNode, kinds: set[TNodeKind]): PNode =
  result = t
  while result.kind in kinds:
    result = lastSon(result)

proc unparseIdentDefs(node: PNode): seq[DefTree] =
  let expr = node[^1]
  let typ = node[^2]
  for idx, name in node[0..^3]:
    var field = newDef(deftField, unparseName(name), name)
    field.typ = typ
    field.initExpr = expr
    if idx == node.len - 2:
      # QUESTION putting documentation comment only into the last
      # field, assuming documentation for 'group of fields' would
      # not be used this way, because it means that all target
      # fields must have the same type, which is a major block.
      field.comment = node.comment

    result.add field


proc unparseFields*(node: PNode): seq[DefField] =
  proc auxBranches(node: PNode): seq[DefFieldBranch]
  proc auxFields(node: PNode): seq[DefField] =
    case node.kind:
      of nkIdentDefs:
        for field in unparseIdentDefs(node):
          result.add DefField(head: field)

      of nkRecWhen:
        result.add DefField(
          kind: deffWrapWhen,
          branches: auxBranches(node)
        )

      of nkRecList:
        for sub in node:
          result.add auxFields(sub)

      of nkEmpty:
        discard

      else:
        failNode node

  proc auxBranches(node: PNode): seq[DefFieldBranch] =
    for branch in node:
      case branch.kind:
        of nkElifBranch:
          result.add DefFieldBranch(
            branchExprs: @[branch[0]],
            subfields: auxFields(branch[1])
          )

        of nkElse:
          result.add DefFieldBranch(subfields: auxFields(branch[0]))

        else:
          failNode branch

  return auxFields(node)


proc unparseDefs*(node: PNode): seq[DefTree]

proc getBaseType*(node: PNode): PNode =
  let body = node.skipNodes({nkRefTy, nkPtrTy})
  if body[1].kind == nkOfInherit:
    return body[1][0]

proc unparseType*(node: PNode): DefTree =
  let body = node[2].skipNodes({nkPtrTy, nkRefTy})
  case body.kind:
    of nkObjectTy:
      result = newDef(deftObject, unparseName(node[0]), node)
      result.comment = node.comment
      result.objFields = unparseFields(body[2])

      if body[1].kind == nkOfInherit:
        result.objBase = getBaseType(node)

    of nkEnumTy:
      result = newDef(deftEnum, unparseName(node[0]), node)
      result.comment = node.comment
      for field in body[1..^1]:
        if field.kind in {nkIdent, nkPrefix}:
          result.enumFields.add newDef(
            deftEnumField, unparseName(field), field)

        else:
          result.enumFields.add unparseDefs(field)

    of nkDistinctTy:
      result = newDef(deftAlias, unparseName(node[0]), node)
      result.isDistinct = true
      result.baseType = body[0]

    of nkInfix, nkProcTy, nkSym, nkIdent, nkBracketExpr, nkTupleTy:
      result = newDef(deftAlias, unparseName(node[0]), node)
      result.baseType = body

    of nkEmpty:
      assert node[0].kind in {nkPragmaExpr}, $treeRepr(nil, node)
      result = newDef(deftMagic, unparseName(node[0]), node)
      result.comment = node.comment

    else:
      echo ">>>> node"
      failNode node


proc unparseProcDef*(node: PNode): DefTree =
  result = newDef(deftProc, unparseName(node[0]), node)
  if node[pragmasPos].kind == nkPragma:
    for arg in node[pragmasPos]:
      result.defName.pragmas.add arg

  result.returnType = node[paramsPos][0]

  for arg in node[paramsPos][1..^1]:
    result.arguments.add unparseIdentDefs(arg)

proc unparseDefs*(node: PNode): seq[DefTree] =
  case node.kind:
    of nkStmtList, nkTypeSection:
      for sub in node:
        result.add unparseDefs(node)

    of nkEnumFieldDef:
      var res = newDef(deftEnumField, unparseName(node[0]), node)
      case node[1].kind:
        of nkIntLit:
          res.valOverride = node[1]

        else:
          failNode node[1]

      result.add res

    of nkTypeDef:
      result.add unparseType(node)

    of nkConstDef, nkIdentDefs:
      result = unparseIdentDefs(node):

    of nkProcDeclKinds:
      result.add unparseProcDef(node)

    else:
      failNode node
