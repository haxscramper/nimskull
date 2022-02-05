## Writing documentation database in sqlite format, either for further code
## analysis (via scripts), or sourcetrail integraion (via direct database
## read)

import
  std/[
    db_sqlite,
    sequtils,
    with,
    strutils,
    strformat,
    os,
    times,
    algorithm
  ],
  front/[
    options,
    msgs
  ],
  ast/[
    ast,
    renderer
  ],
  utils/[
    pathutils
  ],
  ./docgen_types

import std/sqlite3 except close
import std/options as std_options

proc newInsert(table: string, columns: openArray[(string, int)]): string =
  var r = "insert into " & table & " (\n  "
  r.add columns.mapIt(it[0]).join(", ")
  r.add "\n ) values (\n"
  for idx, (name, val) in columns:
   r.add "  ?" & $val
   if idx < columns.high:
     r.add ","

   else:
     r.add " "

   r.add " -- " & name & "\n"

  r.add ");"
  return r

proc newTable(
    table: string,
    columns: openArray[(string, string)],
    extra: string = ""
  ): SqlQuery =

  var r: string

  r.add "create table "
  r.add table
  r.add "(\n"
  for idx, (name, format) in columns:
    if idx > 0: r.add ",\n"
    r.add "  "
    r.add name
    r.add " "
    r.add format

  if 0 < len(extra):
    r.add ",\n"
    r.add extra

  r.add "\n);"

  return sql(r)

proc newTableWithInsert(
    db: DbConn,
    table: string,
    columns: openArray[((string, int), string)],
    extra: string = ""
  ): string =

  var cols: seq[(string, string)]
  var insert: seq[(string, int)]
  for (key, val) in columns:
    cols.add((key[0], val))
    insert.add(key)

  db.exec sql(&"drop table if exists {table}")

  db.exec newTable(table, cols, extra)
  result = newInsert(table, insert)


proc reset(p: SqlPrepared) =
  discard reset(p.PStmt)

proc doExec(sq: DbConn, prep: SqlPrepared) =
  sq.exec(prep)
  reset(prep)

proc toSqlite(t: typedesc[int]): string = "INTEGER"
proc toSqlite(t: typedesc[bool]): string = "INTEGER"
proc toSqlite(t: typedesc[enum]): string = "INTEGER"
proc toSqlite(t: typedesc[string]): string = "TEXT"

proc toSqlite[T](t: typedesc[Option[T]]): string =
  toSqlite(typeof Option[T]().get())

template sq(expr: untyped): untyped =
  toSqlite(typeof expr)

proc bindParam[E: enum](ps: SqlPrepared, idx: int, opt: E) =
  bindParam(ps, idx, opt.int)

proc bindParam(
    ps: SqlPrepared, idx: int,
    it: FileIndex | DocEntryId | DocOccurId | uint16 | int | bool) =

  bindParam(ps, idx, it.int)

const
  sqPrimary = " PRIMARY KEY UNIQUE NOT NULL"
  sqNNil = " NOT NULL"


template withPrepared(conn: DbConn, prepCode: SqlPrepared, body: untyped): untyped =
  block:
    var prep {.inject.} = prepCode
    body
    finalize(prep)

template withPrepared(
    conn: DbConn, prepCode: string,
    prepName: untyped,
    body: untyped
  ): untyped =

  block:
    var prepName {.inject.} = conn.prepare(prepCode)
    body
    finalize(prepName)

template withPrepared(conn: DbConn, prepCode: string, body: untyped): untyped =
  withPrepared(conn, prepCode, prep, body)


proc writeSqlite*(conf: ConfigRef, db: DocDb, file: AbsoluteFile)  =
  var conn = open(file.string, "", "", "")

  let tab = (
    files: "files",
    entr: "entries",
    occur: "occurencies"
  )

  func toSqlite(t: typedesc[DocEntryId]): string = sq("int")
  func toSqlite(t: typedesc[DocOccurId]): string = sq("int")
  func toSqlite(t: typedesc[FileIndex]): string = sq("int")

  withPrepared(conn, conn.newTableWithInsert(tab.files, {
      ("id", 1): sq(int) & sqPrimary,
      ("abs", 2): sq(string)
  })):
    for idx, file in conf.m.fileInfos:
      prep.bindParam(1, idx.int)
      prep.bindParam(2, file.fullPath.string)
      conn.doExec(prep)

  withPrepared(conn, conn.newTableWithInsert(tab.entr, {
    ("id", 1): sq(int) & sqPrimary,
    ("name", 2): sq(string),
    ("kind", 3): sq(DocEntryKind),
    ("decl_file", 4): sq(FileIndex),
    ("decl_line", 5): sq(int),
    ("decl_col", 6): sq(int)
  })):
    for id, entry in db.entries:
      let loc = entry.location
      with prep:
        bindParam(1, id)
        bindParam(2, entry.name)
        bindParam(3, entry.kind)

      if loc.isSome():
        prep.bindParam(4, loc.get().fileIndex)
        prep.bindParam(5, loc.get().line)
        prep.bindParam(6, loc.get().col)

      else:
        prep.bindNull(4)
        prep.bindNull(5)
        prep.bindNull(6)

      conn.doExec(prep)

  withPrepared(conn, conn.newTableWithInsert(tab.occur, {
    ("id", 1): sq(int) & sqPrimary,
    ("kind", 2): sq(DocOccurKind),
    ("occur_of", 3): sq(DocEntryId),
    ("loc_file", 4): sq(FileIndex),
    ("loc_line", 5): sq(int),
    ("loc_col_start", 6): sq(int),
    ("loc_col_end", 7): sq(int)
  })):
    for id, occur in db.occurencies:
      if occur.kind notin dokLocalKinds:
        let s = occur.slice
        with prep:
          bindParam(1, id)
          bindParam(2, occur.kind)
          bindParam(3, occur.refid)
          bindParam(4, s.file)
          bindParam(5, s.line)
          bindParam(6, s.column.a)
          bindParam(7, s.column.b)

  proc kindTable[E: enum](e: typedesc[E]) =
    withPrepared(conn, conn.newTableWithInsert($E, {
      ("id", 1): sq(int),
      ("name", 2): sq(string)
    })):
      for item in low(E) .. high(E):
        prep.bindParam(1, item.int)
        prep.bindParam(2, $item)
        conn.doExec(prep)

  kindTable(DocEntryKind)
  kindTable(DocOccurKind)

  conn.close()

type
  TrailSymbol = enum
    strailSymAnnotation = 1
    strailSymBuiltinType = 2
    strailSymClass = 3
    strailSymEnum = 4
    strailSymEnumConstant = 5
    strailSymField = 6
    strailSymFunction = 7
    strailSymGlobalVariable = 8
    strailSymInterface = 9
    strailSymMacro = 10
    strailSymMethod = 11
    strailSymModule = 12
    strailSymNamespace = 13
    strailSymPackage = 14
    strailSymStruct = 15
    strailSymTypedef = 16
    strailSymTypeParameter = 17
    strailSymUnion = 18

  TrailRelation = enum
    strailRelUndefined = 0
    strailRelTypeUsage = 1
    strailRelUsage = 2
    strailRelCall = 3
    strailRelInheritance = 4
    strailRelOverride = 5
    strailRelTypeArgument = 6
    strailRelTemplateSpecialization = 7
    strailRelInclude = 8
    strailRelImport = 9
    strailRelMacroUsage = 10
    strailRelAnnotationUsage = 11

  TrailDefinition = enum
    trailDefNone
    trailDefImplicit
    trailDefExplicit

  TrailNode = enum
    nodeSymbol = 1 shl 0
    nodeType = 1 shl 1
    nodeBuiltinType = 1 shl 2

    nodeModule = 1 shl 3
    nodeNamespace = 1 shl 4
    nodePackage = 1 shl 5
    nodeStruct = 1 shl 6
    nodeClass = 1 shl 7
    nodeInterface = 1 shl 8
    nodeAnnotation = 1 shl 9
    nodeGlobalVariable = 1 shl 10
    nodeField = 1 shl 11
    nodeFunction = 1 shl 12
    nodeMethod = 1 shl 13
    nodeEnum = 1 shl 14
    nodeEnumConstant = 1 shl 15
    nodeTypedef = 1 shl 16
    nodeTypeParameter = 1 shl 17

    nodeFile = 1 shl 18
    nodeMacro = 1 shl 19
    nodeUnion = 1 shl 20


func toStrail(kind: DocEntryKind): TrailSymbol =
  case kind:
    of ndkNewtypeKinds - { ndkAlias, ndkDistinctAlias, ndkEnum }:
      strailSymStruct

    of ndkProc, ndkFunc, ndkConverter, ndkIterator:
      strailSymFunction

    of ndkMacro, ndkTemplate:
      strailSymMacro

    of ndkAlias, ndkDistinctAlias:
      strailSymTypedef

    of ndkGlobalConst, ndkGlobalVar, ndkGlobalLet:
      strailSymGlobalVariable

    of ndkParam, ndkArg, ndkInject, ndkComment, ndkFile:
      raise (ref AssertionDefect)(msg: $kind)

    of ndkCompileDefine:
      # compile-time defines might be treated as macros or as global
      # varibles. I'm not exactly sure how to classify them, but for
      # now I think global variable describes sematics a little better.
      strailSymGlobalVariable

    of ndkEnum:      strailSymEnum
    of ndkField:     strailSymField
    of ndkEnumField: strailSymEnumConstant
    of ndkBuiltin:   strailSymBuiltinType
    of ndkPragma:    strailSymAnnotation
    of ndkModule:    strailSymModule
    of ndkPackage:   strailSymPackage
    of ndkMethod:    strailSymMethod

func toStrailNode(kind: DocEntryKind): TrailNode =
  case toStrail(kind):
    of strailSymMethod: nodeMethod
    of strailSymPackage: nodePackage
    of strailSymModule: nodeModule
    of strailSymField: nodeField
    of strailSymAnnotation: nodeAnnotation
    of strailSymBuiltinType: nodeBuiltinType
    of strailSymClass: nodeClass
    of strailSymEnum: nodeEnum
    of strailSymEnumConstant: nodeEnumConstant
    of strailSymFunction: nodeFunction
    of strailSymGlobalVariable: nodeGlobalVariable
    of strailSymInterface: nodeInterface
    of strailSymMacro: nodeMacro
    of strailSymNamespace: nodeNamespace
    of strailSymStruct: nodeStruct
    of strailSymTypedef: nodeTypedef
    of strailSymTypeParameter: nodeTypeParameter
    of strailSymUnion: nodeUnion


func toStrail(kind: DocOccurKind): TrailRelation =
  case kind:
     of dokTypeAsArgUse, dokTypeAsReturnUse, dokTypeAsFieldUse:
       strailRelTypeUsage

     of dokExpansion, dokDefineCheck:
       strailRelMacroUsage

     of dokAnnotationUsage:
       strailRelAnnotationUsage

     of dokImported:
       strailRelImport

     of dokInheritFrom:
       strailRelInheritance

     of dokCall:
       strailRelCall

     of dokIncluded:
       strailRelInclude

     of dokEnumFieldUse, dokGlobalRead, dokGlobalWrite, dokFieldUse, dokFieldSet:
       strailRelUsage

     else:
       strailRelUndefined

const settings = """
<?xml version="1.0" encoding="utf-8" ?>
<config>
    <version>8</version>
</config>
"""


proc registerSourcetrail*(conf: ConfigRef, conn: DbConn, db: DocDb) =
  withPrepared(conn, conn.newTableWithInsert("meta", {
    ("id", 1): sq(int),
    ("key", 2): sq(string),
    ("value", 3): sq(string)
  }, extra = "PRIMARY KEY(id)")):
    for idx, (key, value) in {
      "project_settings": settings,
      "storage_version": "25",
      "timestamp": now().format("YYYY-MM-dd hh:mm:ss")
    }:
      prep.bindParam(1, idx)
      prep.bindParam(2, key)
      prep.bindParam(3, value)
      conn.doExec(prep)

  withPrepared(conn, conn.newTableWithInsert("component_access", {
    ("node_id", 1): sq(int) & sqNNil,
    ("type", 2): sq(int) & sqNNil
  }, extra = """
PRIMARY KEY(node_id),
FOREIGN KEY(node_id) REFERENCES node(id) ON DELETE CASCADE
""")):
    for id, entry in db.entries:
      prep.bindParam(1, id)
      prep.bindParam(2, 0)
      conn.doExec(prep)

  withPrepared(conn, conn.newTableWithInsert("filecontent", {
    ("id", 1): sq(int),
    ("content", 2): sq(string)
  })):
    for id, file in conf.m.fileInfos:
      prep.bindParam(1, id)
      prep.bindParam(2, file.fullPath.string.readFile())
      conn.doExec(prep)

  withPrepared(conn, conn.newTableWithInsert("file", {
    ("id", 1): sq(int) & sqNNil,
    ("path", 2): sq(string),
    ("language", 3): sq(string),
    ("modification_time", 4): sq(string),
    ("indexed", 5): sq(bool),
    ("complete", 6): sq(bool),
    ("line_count", 7): sq(int),
  }, extra = """
PRIMARY KEY(id),
FOREIGN KEY(id) REFERENCES node(id) ON DELETE CASCADE
""")):
    for id, file in conf.m.fileInfos:
      let path = file.fullPath.string
      with prep:
        bindParam(1, id)
        bindParam(2, path)
        bindParam(3, "nim")
        bindParam(
          4, getFileInfo(path).lastWriteTime.format("YYYY-MM-dd hh:mm:ss"))

        bindParam(5, true)
        bindParam(6, true)
        bindParam(7, file.fullPath.string.readFile().count('\n'))

      conn.doExec(prep)

  var insertEdge = conn.prepare conn.newTableWithInsert("edge", {
    ("id", 1): sq(int) & sqNNil,
    ("type", 2): sq(int) & sqNNil,
    ("source_node_id", 3): sq(int) & sqNNil,
    ("target_node_id", 4): sq(int) & sqNNil,
  }, extra = """
PRIMARY KEY(id),
FOREIGN KEY(id) REFERENCES element(id) ON DELETE CASCADE,
FOREIGN KEY(source_node_id) REFERENCES node(id) ON DELETE CASCADE,
FOREIGN KEY(target_node_id) REFERENCES node(id) ON DELETE CASCADE
""")

  var pathOffset = 0
  withPrepared(conn, conn.newTableWithInsert("node", {
    ("id", 1): sq(int) & sqNNil,
    ("type", 2): sq(int) & sqNNil,
    ("serialized_name", 3): sq(string)
  }, extra = """
PRIMARY KEY(id),
FOREIGN KEY(id) REFERENCES element(id) ON DELETE CASCADE
""")):
    for id, entry in db.entries:
      if entry.kind notin {ndkParam, ndkArg, ndkInject, ndkComment, ndkFile}:

        var name: string

        const
          metaDelimiter = "\tm"
          nameDelimiter = "\tn"
          partDelimiter = "\ts"
          signatureDelimiter = "\tp"

        var path: seq[DocEntryId]
        var top = id
        while db[top].parent.isSome():
          path.add top
          top = db[top].parent.get()

        path.add top

        reverse(path)
        for idx, part in path:
          if 0 < idx:
            with insertEdge:
              bindParam(1, pathOffset)
              bindParam(2, strailRelAnnotationUsage)
              bindParam(3, path[idx - 1])
              bindParam(4, path[idx])

            inc pathOffset

            conn.doExec(insertEdge)

        name.add "::" # Name delimiter
        name.add metaDelimiter
        for idx, part in path:
          if 0 < idx:
            name.add nameDelimiter

          name.add db[part].name
          name.add partDelimiter

          if db[part].kind in ndkProcKinds:
            name.add "("
            for idx, arg in entry.nested:
              if 0 < idx:
                name.add ", "

              name.add db[arg].name
              name.add ": "
              name.add $db[arg].argType

            name.add ")"

          name.add signatureDelimiter

        prep.bindParam(1, id)
        prep.bindParam(2, entry.kind.toStrailNode())
        prep.bindParam(3, name)
        conn.doExec(prep)

  withPrepared(conn, insertEdge):
    for id, occur in db.occurencies:
      if occur.kind notin dokLocalKinds and occur.user.isSome():
        with prep:
          bindParam(1, id.int + pathOffset)
          bindParam(2, occur.kind.toStrail())
          bindParam(3, occur.user.get())
          bindParam(4, occur.refid)

        conn.doExec(prep)

  withPrepared(conn, conn.newTableWithInsert("local_symbol", {
    ("id", 1): sq(int) & sqNNil,
    ("name", 2): sq(string),
  }, extra = """
PRIMARY KEY(id),
FOREIGN KEY(id) REFERENCES element(id) ON DELETE CASCADE
""")):
    discard

  withPrepared(conn, conn.newTableWithInsert("error", {
    ("id", 1): sq(int) & sqNNil,
    ("message", 2): sq(string),
    ("fatal", 3): sq(bool),
    ("indexed", 4): sq(bool),
    ("translation_unit", 5): sq(string) ,
  }, extra = """
PRIMARY KEY(id),
FOREIGN KEY(id) REFERENCES element(id) ON DELETE CASCADE
""")):
    discard

  conn.exec sql"DROP TABLE IF EXISTS element"
  conn.exec sql"""
CREATE TABLE element(
  id INTEGER,
  PRIMARY KEY(id));
"""

  conn.exec sql"DROP TABLE IF EXISTS element_component"
  conn.exec sql"""
CREATE TABLE element_component(
  id INTEGER,
  element_id INTEGER,
  type INTEGER,
  data TEXT,
  PRIMARY KEY(id),
  FOREIGN KEY(element_id) REFERENCES element(id) ON DELETE CASCADE);
"""


  withPrepared(conn, conn.newTableWithInsert("symbol", {
    ("id", 1): sq(int) & sqNNil,
    ("definition_kind", 2): sq(int) & sqNNil
  }, extra = """
PRIMARY KEY(id),
FOREIGN KEY(id) REFERENCES node(id) ON DELETE CASCADE
""")):
    for id, entry in db.entries:
      prep.bindParam(1, id)
      prep.bindParam(2, 2)
      conn.doExec(prep)

  withPrepared(conn, conn.newTableWithInsert("occurrence", {
    ("element_id", 1): sq(int),
    ("source_location_id", 2): sq(int)
  }), prepOccur):
    withPrepared(conn, conn.newTableWithInsert("source_location", {
      ("id", 1): sq(int),
      ("file_node_id", 2): sq(int),
      ("start_line", 3): sq(int),
      ("start_column", 4): sq(int),
      ("end_line", 5): sq(int),
      ("end_column", 6): sq(int),
      ("type", 7): sq(int)
    }, extra = """
PRIMARY KEY(id),
FOREIGN KEY(file_node_id) REFERENCES node(id) ON DELETE CASCADE
"""), prepLocation):
      # Occurence source location information is stored together with
      # declaration location, so offsetting the indices.
      let declShift = len(db.occurencies)

      for id, occur in db.occurencies:
        if occur.kind notin dokLocalKinds:
          with prepLocation:
            bindParam(1, id.int + declShift)
            bindParam(2, occur.slice.file)
            bindParam(3, occur.slice.line)
            bindParam(4, occur.slice.column.a)
            bindParam(5, occur.slice.line)
            bindParam(6, occur.slice.column.b)

          conn.doExec(prepLocation)

          with prepOccur:
            bindParam(1, occur.refid)
            bindParam(2, id.int + declShift)

          conn.doExec(prepOccur)


#   for line in splitLines("""
# DROP INDEX IF EXISTS node_serialized_name_index
# CREATE INDEX node_serialized_name_index ON node(serialized_name);
# DROP INDEX IF EXISTS occurrence_element_id_index
# CREATE INDEX occurrence_element_id_index ON occurrence(element_id);
# DROP INDEX IF EXISTS occurrence_source_location_id_index
# CREATE INDEX occurrence_source_location_id_index ON occurrence(source_location_id);
# DROP INDEX IF EXISTS local_symbol_name_index
# DROP INDEX IF EXISTS edge_source_target_type_index
# CREATE INDEX edge_source_target_type_index ON edge(source_node_id, target_node_id, type);
# """):
#    conn.exec(sql(line))


proc writeSourcetrail*(conf: ConfigRef, db: DocDb, file: AbsoluteFile) =
  var conn = open(file.string, "", "", "")
  let (dir, name, ext) = file.string.splitFile()


  writeFile(dir / name & ".srctrlprj", settings)


  try:
    conf.registerSourcetrail(conn, db)

  except DbError:
    echo &"connection error: {errmsg(conn)}, code: {errcode(conn)}"
    raise

  conn.close()
