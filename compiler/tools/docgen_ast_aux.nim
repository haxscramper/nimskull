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
