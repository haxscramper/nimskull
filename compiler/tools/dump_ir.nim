import
  sem/[
    passes,
    sem
  ],
  front/[
    msgs,
    options,
    scriptconfig
  ],
  modules/[
    modulegraphs,
    modules
  ],
  utils/[
    pathutils,
    astrepr
  ],
  ast/[
    ast,
    idents,
    llstream
  ]

proc compileString*(graph: ModuleGraph, text: string): PNode =
  let moduleName: string = "/tmp/compileStringModuleName.nim"
  var
    idx {.global.}: FileIndex
    res {.global.}: PNode

  idx = graph.config.fileInfoIdx(AbsoluteFile moduleName)
  res = nkStmtList.newTree()
  registerPass(graph, semPass)
  registerPass(
    graph, makePass(
      TPassOpen(
        proc(graph: ModuleGraph, module: PSym, idgen: IdGenerator): PPassContext {.nimcall.} =
          return PPassContext()
      ),
      TPassProcess(
        proc(c: PPassContext, n: PNode): PNode {.nimcall.} =
          if n.info.fileIndex.uint32 == idx.uint32:
            res.add n
          result = n
      ),
      TPassClose(
        proc(graph: ModuleGraph; p: PPassContext, n: PNode): PNode {.nimcall.} =
          discard
      )
    )
  )

  var m = graph.makeModule(moduleName)
  graph.vm = setupVM(m, graph.cache, moduleName, graph, graph.idgen)
  graph.compileSystemModule()
  discard graph.processModule(m, graph.idgen, llStreamOpen(text))
  return res
