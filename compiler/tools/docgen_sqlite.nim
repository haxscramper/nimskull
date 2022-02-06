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

        conn.doExec prep

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
