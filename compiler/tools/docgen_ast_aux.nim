import
  ast/[
    ast,
    renderer
  ],
  utils/[
    astrepr
  ]

proc failNode*(node: PNode) {.
    deprecated: "Temporary hack to speed up development",
    noreturn
  .} =

  echo treeRepr(nil, node, maxPath = 3)
  assert false
