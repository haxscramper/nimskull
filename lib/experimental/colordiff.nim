import ./diff, ./colortext
import std/[sequtils, strutils, strformat]

export toString, `$`, myersDiff, shiftDiffed

proc colorDollar*[T](arg: T): ColText = toColText($arg)

type
  DiffFormatConf* = object
    maxUnchanged*: int
    maxUnchangedWords*: int
    showLines*: bool
    lineSplit*: proc(str: string): seq[string]
    sideBySide*: bool
    explainInvisible*: bool
    explainWithUnicode*: bool
    inlineDiffSeparator*: ColText
    formatChunk*: proc(text: string, mode, secondary: SeqEditKind): ColText

  DiffFormatter[T] = object
    strConv*: proc(arg: T): string
    eqCmp*: proc(a, b: T): bool
    conf*: DiffFormatConf

proc chunk(
    conf: DiffFormatConf, text: string,
    mode: SeqEditKind, secondary: SeqEditKind = mode
  ): ColText =
  conf.formatChunk(text, mode, secondary)

func splitKeepSeparator*(str: string, sep: set[char] = {' '}): seq[string] =
  var prev = 0
  var curr = 0
  while curr < str.len:
    if str[curr] in sep:
      if prev != curr:
        result.add str[prev ..< curr]

      prev = curr
      while curr < str.high and str[curr + 1] == str[curr]:
        inc curr

      result.add str[prev .. curr]
      inc curr
      prev = curr

    else:
      inc curr

  if prev < curr:
    result.add str[prev ..< curr]

proc formatDiffed*[T](
    ops: seq[SeqEdit],
    oldSeq, newSeq: seq[T],
    conf: DiffFormatConf
  ): tuple[oldLine, newLine: ColText] =

  var unchanged = 0
  var oldLine: seq[ColText]
  var newLine: seq[ColText]
  for idx, op in ops:
    case op.kind:
      of sekKeep:
        if unchanged < conf.maxUnchanged:
          oldLine.add conf.chunk(oldSeq[op.sourcePos], sekKeep)
          newLine.add conf.chunk(newSeq[op.targetPos], sekKeep)
          inc unchanged

      of sekDelete:
        oldLine.add conf.chunk(oldSeq[op.sourcePos], sekDelete)
        unchanged = 0

      of sekInsert:
        newLine.add conf.chunk(newSeq[op.targetPos], sekInsert)
        unchanged = 0

      of sekReplace:
        oldLine.add conf.chunk(oldSeq[op.sourcePos], sekReplace, sekDelete)
        newLine.add conf.chunk(newSeq[op.targetPos], sekReplace, sekInsert)
        unchanged = 0

      of sekNone:
        assert false

      of sekTranspose:
        discard

  return (
    oldLine.join(conf.inlineDiffSeparator),
    newLine.join(conf.inlineDiffSeparator)
  )




func visibleName(ch: char): tuple[unicode, ascii: string] =
  case ch:
    of '\x00': ("␀", "[NUL]")
    of '\x01': ("␁", "[SOH]")
    of '\x02': ("␂", "[STX]")
    of '\x03': ("␃", "[ETX]")
    of '\x04': ("␄", "[EOT]")
    of '\x05': ("␅", "[ENQ]")
    of '\x06': ("␆", "[ACK]")
    of '\x07': ("␇", "[BEL]")
    of '\x08': ("␈", "[BS]")
    of '\x09': ("␉", "[HT]")
    of '\x0A': ("␤", "[LF]")
    of '\x0B': ("␋", "[VT]")
    of '\x0C': ("␌", "[FF]")
    of '\x0D': ("␍", "[CR]")
    of '\x0E': ("␎", "[SO]")
    of '\x0F': ("␏", "[SI]")
    of '\x10': ("␐", "[DLE]")
    of '\x11': ("␑", "[DC1]")
    of '\x12': ("␒", "[DC2]")
    of '\x13': ("␓", "[DC3]")
    of '\x14': ("␔", "[DC4]")
    of '\x15': ("␕", "[NAK]")
    of '\x16': ("␖", "[SYN]")
    of '\x17': ("␗", "[ETB]")
    of '\x18': ("␘", "[CAN]")
    of '\x19': ("␙", "[EM]")
    of '\x1A': ("␚", "[SUB]")
    of '\x1B': ("␛", "[ESC]")
    of '\x1C': ("␜", "[FS]")
    of '\x1D': ("␝", "[GS]")
    of '\x1E': ("␞", "[RS]")
    of '\x1F': ("␟", "[US]")
    of '\x7f': ("␡", "[DEL]")
    of ' ': ("␣", "[SPC]") # Space
    else: ($ch, $ch)

func toVisibleNames(str: string, unicode: bool): string =
  for ch in str:
    let (uc, ascii) = visibleName(ch)
    if unicode:
      result.add uc

    else:
      result.add ascii


func toVisibleNames(split: seq[string], unicode: bool): seq[string] =
  if 0 < split.len():
    for part in split:
      result.add toVisibleNames(part, unicode)

const Invis = { '\x00' .. '\x1F', '\x7F' }

func hasInvisible(text: char): bool = text in Invis
func hasInvisible(text: string): bool =
  for ch in text:
    if ch in Invis:
      return true

  if 0 < len(text):
    if text[^1] == ' ':
      return true

func hasInvisible(text: seq[string]): bool =
  for item in text:
    if item.hasInvisible():
      return true

func hasInvisibleChanges(diff: seq[SeqEdit], oldSeq, newSeq: seq[string]): bool =
  for edit in diff:
    case edit.kind:
      of sekDelete:
        if oldSeq[edit.sourcePos].hasInvisible():
          return true

      of sekInsert:
        if newSeq[edit.targetPos].hasInvisible():
          return true

      of sekNone, sekKeep, sekTranspose:
        discard

      of sekReplace:
        if oldSeq[edit.sourcePos].hasInvisible() or
           newSeq[edit.targetPos].hasInvisible():
          return true

func diffFormatter*[T](): DiffFormatter[T] =
  const style = (
    deleted: bgDefault + fgRed,
    inserted: bgDefault + fgGreen,
    kept: bgDefault + fgDefault,
    changed: bgDefault + fgYellow
  )

  DiffFormatter[T](
    strConv:            (proc(arg:  T):      string = $arg),
    eqCmp:              (proc(a, b: T):      bool = (a == b)),
    conf: DiffFormatConf(
      maxUnchanged:       high(int),
      explainInvisible:   true,
      explainWithUnicode: true,
      maxUnchangedWords:  high(int),
      showLines:          false,
      lineSplit:          (
        proc(a:    string): seq[string] = splitKeepSeparator(a)
      ),
      sideBySide:         false,
      formatChunk:        (
        proc(word: string, mode, secondary: SeqEditKind): ColText =
          case mode:
            of sekDelete: word + style.deleted
            of sekInsert: word + style.inserted
            of sekKeep: word + style.kept
            of sekReplace, sekTranspose: word + style.changed
            of sekNone: word + fgDefault
      )
    )
  )


proc formatDiffed*[T](
    shifted: ShiftedDiff,
    oldSeq, newSeq: openarray[T],
    fmt: DiffFormatter[T] = diffFormatter[T]()
  ): ColText =

  ## - @arg{stackLongLines} :: If any of two diffed lines are longer than
  ##   threshold, display then one on top of another instead of side by
  ##   side

  var
    oldText, newText: seq[tuple[text: ColText, changed: bool]]
    lhsMax = 0

  let conf = fmt.conf

  let maxLhsIdx = len($shifted.oldShifted[^1].item)
  let maxRhsIdx = len($shifted.newShifted[^1].item)

  proc editFmt(edit: SeqEditKind, idx: int, isLhs: bool): ColText =
    if conf.showLines:
      let num =
        if edit == sekNone:
          alignRight(clt(" "), maxLhsIdx)

        elif isLhs:
          alignRight(clt($idx), maxLhsIdx)

        else:
          alignRight(clt($idx), maxRhsIdx)

      case edit:
        of sekDelete: "- " & num
        of sekInsert: "+ " & num
        of sekKeep: "~ " & num
        of sekNone: "? " & num
        of sekReplace: "-+" & num
        of sekTranspose: "^v" & num

    else:
      case edit:
        of sekDelete:
          conf.chunk("- ", sekDelete)
        of sekInsert:
          conf.chunk("+ ", sekInsert)
        of sekReplace:
          conf.chunk("-+", sekReplace)
        of sekKeep:
          conf.chunk("~ ", sekKeep)
        of sekTranspose:
          conf.chunk("^v", sekTranspose)
        of sekNone:
          conf.chunk((if isLhs: "? " else: "?"), sekNone)


  var unchanged = 0
  for (lhs, rhs, lhsDefault, rhsDefault, idx) in zipToMax(
    shifted.oldShifted, shifted.newShifted
  ):
    if lhs.kind == sekKeep and rhs.kind == sekKeep:
      if unchanged < conf.maxUnchanged:
        inc unchanged

      else:
        continue

    else:
      unchanged = 0

    oldText.add((editFmt(lhs.kind, lhs.item, true), true))

    newText.add((
      editFmt(rhs.kind, rhs.item, false),
      # Only newly inserted lines need to be formatted for the unified
      # diff, everything else is displayed on the 'original' version.
      not conf.sideBySide and rhs.kind in {sekInsert}
    ))

    var lhsEmpty, rhsEmpty: bool
    if not lhsDefault and
       not rhsDefault and
       lhs.kind == sekDelete and
       rhs.kind == sekInsert:

      let
        oldSplit: seq[string] = conf.lineSplit(fmt.strConv(oldSeq[lhs.item]))
        newSplit: seq[string] = conf.lineSplit(fmt.strConv(newSeq[rhs.item]))
        diffed = levenshteinDistance[string](oldSplit, newSplit)

      var oldLine, newLine: ColText

      if conf.explainInvisible and
         diffed.operations.hasInvisibleChanges(oldSplit, newSplit) or
         oldSplit.hasInvisible() or
         newSplit.hasInvisible():
        (oldLine, newLine) = formatDiffed(
          diffed.operations,
          oldSplit.toVisibleNames(conf.explainWithUnicode),
          newSplit.toVisibleNames(conf.explainWithUnicode),
          conf
        )

      else:
        (oldLine, newLine) = formatDiffed(
          diffed.operations,
          oldSplit, newSplit,
          conf
        )

      oldText[^1].text.add oldLine
      newText[^1].text.add newLine


    elif rhs.kind == sekInsert:
      let tmp = fmt.strConv(newSeq[rhs.item])
      rhsEmpty = tmp.len == 0
      newText[^1].text.add conf.chunk(tmp, sekInsert)

    elif lhs.kind == sekDelete:
      let tmp = oldSeq[lhs.item]
      lhsEmpty = tmp.len == 0
      oldText[^1].text.add conf.chunk(tmp, sekDelete)

    else:
      let ltmp = fmt.strConv(oldSeq[lhs.item])
      lhsEmpty = ltmp.len == 0
      oldText[^1].text.add conf.chunk(ltmp, lhs.kind)

      let rtmp = fmt.strConv(newSeq[rhs.item])
      rhsEmpty = rtmp.len == 0
      newText[^1].text.add conf.chunk(rtmp, rhs.kind)


    if lhsEmpty and idx < shifted.oldShifted.high:
      oldText[^1].text.add toVisibleNames("\n", conf.explainWithUnicode)

    if rhsEmpty and idx < shifted.newShifted.high:
      newText[^1].text.add toVisibleNames("\n", conf.explainWithUnicode)

    lhsMax = max(oldText[^1].text.len, lhsMax)

  var first = true
  for (lhs, rhs) in zip(oldtext, newtext):
    if not first:
      # Avoid trailing newline of the diff formatting.
      result.add "\n"
    first = false

    if conf.sideBySide:
      result.add alignLeft(lhs.text, lhsMax + 3)
      result.add rhs.text

    else:
      result.add lhs.text
      if rhs.changed:
        result.add "\n"
        result.add rhs.text

proc formatDiffed*[T](
    oldSeq, newSeq: openarray[T],
    fmt: DiffFormatter[T] = diffFormatter[T]()
  ): ColText =

  myersDiff(oldSeq, newSeq, fmt.eqCmp).
    shiftDiffed(oldSeq, newSeq).
    formatDiffed(oldSeq, newSeq, fmt)


proc formatDiffed*(
    text1, text2: string,
    fmt: DiffFormatter[string] = diffFormatter[string]()
  ): ColText =
  ## Format diff of two text blocks via newline split and default
  ## `formatDiffed` implementation
  formatDiffed(text1.split("\n"), text2.split("\n"), fmt)

when isMainModule:
  for (a, b) in @[
    ("1\n2", "1\n3"),
    ("word 1", "word 2"),
    ("word ", "word"),
    ("\e[32mword\e[39m", "\e[31mword\e[39m"),
    ("word\n", "word"),
    ("1\n2\n3\n4\n5\n", "1\n2\n3\n8\n9\n0\n\n"),
    ("", "\n"),
  ]:
    echo ">>>>"
    var fmt = diffFormatter[string]()
    # fmt.explainWithUnicode = false
    fmt.conf.lineSplit = proc(line: string): seq[string] = mapIt(line, $it)
    fmt.conf.inlineDiffSeparator = clt(" ")
    fmt.conf.sideBySide = true
    echo formatDiffed(a, b, fmt)

  for (a, b) in @[
    ("""
tundeclared_routine.nim(34, 17) Error: attempting to call routine: 'myiter'
  found tundeclared_routine.myiter(a: string) [iterator declared in tundeclared_routine.nim(32, 12)]
  found tundeclared_routine.myiter() [iterator declared in tuedeclared_routine.nim(33, 12)]
tundeclared_routine.nim(34, 17) Error: expression has no type: myiter(1)
tundeclared_routine.nim(39, 28) Error: invalid pragma: myPragma
tundeclared_routine.nim(46, 14) Error: undeclared field: 'bar4' for type tundeclared_routine.Foo [type declared in tundeclared_routine.nim(43, 8)]
tundeclared_routine.nim(46, 13) Error: expression has no type: `.`(a, bar3)
tundeclared_routine.nim(51, 14) Error: undeclared field: 'bar4' for type tundeclared_routine.Foo [type declared in tundeclared_routine.nim(49, 8)]
tundeclared_routine.nim(51, 13) Error: expression has no type: `.`(a, bar4)
tundeclared_routine.nim(54, 11) Error: undeclared identifier: 'bad5'
tundeclared_routine.nim(54, 15) Error: expression has no type: bad5(1)""", """
tundeclared_routine.nim(34, 17) Error: attempting to call routine: 'myiter'
  found tundeclared_routine.myiter(a: string) [iterator declared in tundeclared_routine.nim(32, 12)]
  found tundeclared_routine.myiter() [iterator declared in tundeclared_routine.nim(33, 12)]
tundeclared_routine.nim(34, 17) Error: expression has no type: myiter(1)
tundeclared_routine.nim(39, 28) Error: invalid pragma: myPragma
tundeclared_routine.nim(51, 13) Error: expression has no type: `.`(a, bar4)
tundeclared_routine.nim(54, 11) Error: undeclared identifier: 'bad5'
tundeclared_routine.nim(54, 15) Error: expression has no type: bad5(1)""")
  ]:
    echo ">>>>"
    var fmt = diffFormatter[string]()
    fmt.conf.lineSplit = proc(line: string): seq[string] = mapIt(line, $it)
    let prev = fmt.conf.formatChunk
    fmt.conf.formatChunk = proc(text: string, mode, secondary: SeqEditKind): ColText =
                             if mode == sekReplace:
                               prev(text, secondary, secondary) + styleReverse

                             else:
                               prev(text, mode, mode)

    echo formatDiffed(a, b, fmt)

  proc diff(a, b: string, sideBySide: bool = false): string =
    # Configure diff formattter to use no unicode or colors to make testing
    # easier
    var fmt = diffFormatter[string]()
    fmt.conf.explainWithUnicode = false
    fmt.conf.sideBySide = sideBySide
    fmt.conf.inlineDiffSeparator = clt("#")
    fmt.conf.formatChunk = proc(
      word: string, mode, secondary: SeqEditKind
    ): ColText = &"[{($mode)[3]}/{word}]" + fgDefault

    return formatDiffed(a, b, fmt).toString(false)

  proc assertEq(a, b: string) =
    if a != b:
      assert false, &"expected:\n{a}\nfound:\n{b}\ndiff:\n{diffText(a, b, true)}"

  diff("a", "b").assertEq:
    """
[D/- ][R/a]
[I/+ ][R/b]"""
    # `-` and `+` are formatted as delete/insert operations, `a` is formatted as `Replace`

  diff("a b", "b b").assertEq:
    """
[D/- ][R/a]#[K/[SPC]]#[K/b]
[I/+ ][R/b]#[K/[SPC]]#[K/b]"""
    # `Keep` the space and last `b`, replace first `a -> b`

  diff("", "\n", true).assertEq:
    """
[K/~ ][K/]   [K/~ ][K/][LF]
[N/? ]       [I/+ ][I/]"""

    # Keep the empty line. `[LF]` at the end is not considered for diff
    # since it used to *separate* lines.
