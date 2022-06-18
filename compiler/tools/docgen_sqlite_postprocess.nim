import
  compiler/tools/[
    docgen3,
    docgen_types,
    docgen_sqlite
  ],
  compiler/front/[
    options
  ],
  compiler/utils/[
    pathutils
  ],
  std/[
    os
  ]

let outSql = AbsoluteFile(commandLineParams()[0])

var newConf = ConfigRef()
var newDb = DocDb()
readSqlite(newConf, newDb, outSql)
echo "read sqlite from ", outSql.string

let outSql2 = outSql.changeFileExt("sqlite2")
newConf.writeSqlite(newDb, outSql2)
echo "wrote sqlite database back ", outSql2.string
