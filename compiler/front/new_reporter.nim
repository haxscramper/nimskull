## New-style error message reporter

import
  experimental/[
    diff,
    colordiff,
    colortext
  ],
  std/[
    algorithm,
    sequtils,
    strformat,
    strutils
  ],
  compiler/front/[
    options
  ],
  compiler/ast/[
    ast_types,
    ast,
    reports,
    astalgo,
    renderer,
    typesrenderer
  ],
  compiler/utils/[
    astrepr
  ]

import cli_reporter as old

type
  StringMismatchCandidate* = object
    ## Description of a single string edit operation
    distance*: int ## Edit distance between target (provided) and input string (not provided)
    edits*: seq[SeqEdit] ## Sequence of edit operations to convert input string into target
    target*: string ## Target string

proc stringMismatchCandidates*(
    input: string,
    expected: openArray[string]
  ): seq[StringMismatchCandidate] =

  var results: seq[tuple[
    edits: tuple[distance: int, operations: seq[SeqEdit]],
    target: string
  ]]

  for str in expected:
    if str == input:
      return @[]

    else:
      let (distance, edits) = levenshteinDistance(input.toSeq(), str.toSeq())
      result.add StringMismatchCandidate(
        distance: distance,
        edits: edits,
        target: str
      )

proc stringMismatchMessage*(
    input: string,
    expected: openArray[string],
    fixSuggestion: bool = true,
    showAll: bool = true,
  ): ColText =

  coloredResult()

  let expected = deduplicate(expected)

  if expected.len == 0:
    add "No matching alternatives"
    return

  var results = stringMismatchCandidates(input, expected).
    sortedByIt(it.distance)

  let best = results[0]

  var fmt = diffFormatter(false)
  fmt.lineSplit = proc(s: string): seq[string] =
                    mapIt(s, $it)

  if best.distance > int(input.len.float * 0.8):
    add "no close matches to "
    add input + fgRed
    add ", possible alternative(s): "
    var first = true
    for it in results[0 .. min(results.high, 3)]:
      if not first: add " or "
      first = false
      add it.target + fgYellow

  else:
    add "Did you mean to use '"
    add best.target + fgYellow
    add "'?'"
    var edits = 0
    for edit in best.edits:
      if edit.kind != sekKeep:
        inc edits

    if fixSuggestion:
      if edits < min(3, input.len div 2):
        add " ("
        add formatInlineDiff(input, best.target, fmt)
        add ")"

      else:
        add " ("
        add input + fgRed
        add " -> "
        add best.target + fgGreen
        add ")"

    if showAll and expected.len > 1:
      add "\n  ("
      for idx, alt in results[1 ..^ 1]:
        if idx > 0:
          add " "

        add alt.target + styleItalic + termFg(4, 2, 3)
        add "?"

      add ")"

## Large rank/cost value means the error was severe (error in the first
## argument, completely different types). Smaller cost means the error was
## pretty minor (typo in the named argument, mismatch in the 8th position)

const cost = (
  genericLayer: 1 shl 10, # Base value of the first level of the type
                          # mismatch. When recursing into generics cost
                          # decreases exponentially, so `int-float` mismatch
                          # is ranked higher than `seq[int]-seq[float]`.

  literalMultiplier: 0.3  # Multiplier applied when provided expression has
                          # type mismatched /and/ it was a literal value.
                          # For example function expects `uint8` and `int`
                          # literal was provided.
)

type
  Arg = PNode
  ArgList = seq[Arg] ## List of the procedure arguments - either passed,
                       ## or expected

  RankedCallMismatch = object
    sem: SemCallMismatch ## Original call mismatch
    rank: int ## Final rank value
    mismatches: seq[ArgCompare]

  ArgCompare = object
    ## Structural diff between two types
    wanted, found: PType
    rank: int
    nested: seq[ArgCompare]

func rankMismatch(wanted, found: Arg): ArgCompare =
  assert not isNil(wanted.typ)
  assert not isNil(found.typ)
  ArgCompare(wanted: wanted.typ, found: found.typ)

func typoCost(used: string, expected: seq[string]): int =
  # Specific details of the typo correction costs can be checked for later,
  # since candidate that has /only/ typo in the name would have a low cost
  # anyway (all types match, the only error is in the named parameter).
  12

func argSyms(procType: PType): seq[PNode] =
  procType.n.sons[1 ..^ 1]

proc updateRank*(mis: var RankedCallMismatch, args: ArgList) =
  let sem = mis.sem
  let argSyms = sem.target.typ.argSyms()
  let matchingLen = min(argSyms.len(), args.len())
  for idx in 0 ..< matchingLen:
    var mismatch = rankMismatch(argSyms[idx], args[idx])
    mis.mismatches.add mismatch
    if idx <= sem.firstMismatch.pos:
      # Cost is declreased from the first mismatch position - error in the
      # first argument is more likely
      mis.rank += mismatch.rank * ((matchingLen - idx) - sem.firstMismatch.pos)

  case sem.firstMismatch.kind:
    of kTypeMismatch:
      # Type mismatch arguments are handled uniformly and decreasing
      # ranking based on the first mismatch position
      discard

    of kUnknownNamedParam:
      mis.rank += typoCost(
        $sem.firstMismatch.arg,
        sem.target.typ.argSyms().mapIt(it.sym.name.s))

    else:
      discard


proc toRanked(
    mis: seq[SemCallMismatch],
    args: ArgList
  ): seq[RankedCallMismatch] =
  for m in mis:
    result.add RankedCallMismatch(sem: m)
    result[^1].updateRank(args)

proc typeHeadName(t: PType, withModule: bool = false): string =
  case t.kind:
    of tyGenericBody:
      result = t.lastSon.typeToString()

    of tyInt: result = "int"

    else:
      result = t.typeToString()

proc format(target: PType, other: PType = nil): ColText =
  ## Custom type format implementation, used *specifically* for error
  ## reporting - it might fall back to general type rendering logic from
  ## time to time, but otherwise is specifically geared towards
  ## human-readable, colored representation.
  coloredResult()

  proc aux(target, other: PType) =
    if other.isNil:
      add typeToString(target)
      return

    else:
      var tname = target.typeHeadName()
      var oname = other.typeHeadName()
      if tname == oname:
        add tname

      else:
        add &"{tname}" + fgGreen
        add " != "
        add &"{oname}" + fgRed

    case target.kind:
      of tyGenericBody, tyBuiltInTypeClass:
        add "["
        for idx in 0 ..< len(target):
          if idx > 0: add ", "
          aux(target[idx], other[idx])

        add "]"

      else:
        discard

  aux(target, other)

  endResult()

proc formatProc(p: PSym): ColText =
  coloredResult()

  add "proc "
  add p.name.s + fgGreen
  add "("
  for idx, arg in pairs(p.typ.n.sons[1 ..^ 1]):
    if idx > 0: add ", "
    add arg.sym.name.s + fgCyan
    add ": "
    add arg.typ.format()

  add ")"

  endResult()



proc format(arg: ArgCompare): ColText =
  assert not isNil(arg.wanted)
  assert not isNil(arg.found)
  result.add format(arg.wanted, arg.found)

proc format(mis: RankedCallMismatch): ColText =
  ## Format single call mismatch instance - procedure with incorrect
  ## argument types, unknown named parameter.
  coloredResult()

  let sem = mis.sem
  case sem.firstMismatch.kind:
    of kUnknownNamedParam:
      # This part deals with typos in the arguments - current
      # implementation provides rather strange-looking elements
      add "unknown named argument - "
      add sem.target.formatProc()
      add ":\n  "
      add stringMismatchMessage(
        $sem.firstMismatch.arg,
        sem.target.typ.argSyms().mapIt(it.sym.name.s))

    of kTypeMismatch:
      # Argment type mismatch message
      add "("
      var first = true
      let syms = sem.target.typ.argSyms()
      for idx, argMis in mis.mismatches:
        if not first: add ", "
        first = false
        add $syms[idx]
        add ": "
        add argMis.format()

      add ")"

    else:
      discard

  endResult()


func groupByIdx(mis: sink seq[SemCallMismatch]): tuple[
    byType: seq[seq[SemCallMismatch]],
    other: seq[SemCallMismatch]
  ] =
  ## Split overload candidates into ones that failed because of the
  ## argument mismatch, and everything else.

  var mis = mis
  mis.sort(proc(a, b: SemCallMismatch): int =
             cmp($a.firstMismatch.arg, $b.firstMismatch.arg))

  var prev = -1
  for mis in mis:
    if mis.firstMismatch.kind != kTypeMismatch:
      result.other.add mis

    elif mis.firstMismatch.pos != prev:
      prev = mis.firstMismatch.pos
      result.byType.add @[mis]

    else:
      result.byType[^1].add mis

proc reportCallMismatch(conf: ConfigRef, r: SemReport): ColText =
  let args = mapIt(r.ast, it)[1 .. ^1]
  # First group call candidates by argument index and mismatch kind. This
  # allows to show 'but expression is of type' and other diagnostics only
  # once per argument position.
  let (byType, other) = r.callMismatches.groupByIdx()

  coloredResult()
  add "Function call error:\n"
  add "Type mismatch failures"
  for group in byType:
    # Convert each group's items into ranked nodes and sort candidates
    # within the byType using more advanced heuristics.
    let first = group[0].firstMismatch
    let expr = first.arg
    add "\nMismatch for argument #"
    add $first.pos
    add " of proc "
    add $group[0].target.name.s + fgGreen
    add " - expression '"
    add $first.arg
    add "' is of type "
    add first.arg.typ.format() + fgRed
    add ", but expected any of\n"
    for mis in group.toRanked(args).sortedByIt(-it.rank):
      add "\n  "
      add mis.format()

  add "\nOther mismatches"
  for mis in other.toRanked(args).sortedByIt(-it.rank):
    add "\n"
    add mis.format()

proc reportBody*(conf: ConfigRef, r: SemReport): ColText =
  case r.kind:
    of rsemCallTypeMismatch:
      result = reportCallMismatch(conf, r)

    else:
      result.add old.reportBody(conf, r)

proc getContext(conf: ConfigRef, ctx: seq[ReportContext]): ColText =
  ## Format report context message
  coloredResult()
  for ctx in items(ctx):
    # Instantiation reports have their own context message information
    add(old.toStr(conf, ctx.location))
    case ctx.kind:
      of sckInstantiationOf:
        debug ctx.entry
        add " instantiation of "
        add ctx.entry.name.s
        if 0 < ctx.params.data.len:
          add "["
          var first = true
          for pair in ctx.params.data:
            # Instantiation context has random items mixed in - not
            # everything is a type or a symbol, so we need to filter out
            # unwanted elements here.
            if pair.key of PType:
              if not first:
                add ", "
              first = false
              add pair.key.PType().format() + fgYellow
              add " = "

            if pair.val of PType:
              add pair.val.PType().format() + fgGreen

          add "]"
        add " from here\n"

      of sckInstantiationFrom:
        add(" template/generic instantiation from here\n")

proc reportFull*(conf: ConfigRef, r: SemReport): ColText =
  if r.kind == rsemProcessing and conf.hintProcessingDots:
    result.add "."
    return

  result.add conf.getContext(r.context)
  result.add reportBody(conf, r)

proc reportFull*(conf: ConfigRef, r: LexerReport): ColText =
  result.add old.reportFull(conf, r)

proc reportFull*(conf: ConfigRef, r: CmdReport): ColText =
  result.add old.reportFull(conf, r)

proc reportFull*(conf: ConfigRef, r: ParserReport): ColText =
  result.add old.reportFull(conf, r)

proc reportFull*(conf: ConfigRef, r: DebugReport): ColText =
  result.add old.reportFull(conf, r)

proc reportFull*(conf: ConfigRef, r: InternalReport): ColText =
  result.add old.reportFull(conf, r)

proc reportFull*(conf: ConfigRef, r: BackendReport): ColText =
  result.add old.reportFull(conf, r)

proc reportFull*(conf: ConfigRef, r: ExternalReport): ColText =
  result.add old.reportFull(conf, r)

proc reportFull*(conf: ConfigRef, r: Report): ColText =
  ## Generate full version of the report (location, severity, body,
  ## optional suffix)
  case r.category:
    of repLexer:    result = conf.reportFull(r.lexReport)
    of repParser:   result = conf.reportFull(r.parserReport)
    of repCmd:      result = conf.reportFull(r.cmdReport)
    of repSem:      result = conf.reportFull(r.semReport)
    of repDebug:    result = conf.reportFull(r.debugReport)
    of repInternal: result = conf.reportFull(r.internalReport)
    of repBackend:  result = conf.reportFull(r.backendReport)
    of repExternal: result = conf.reportFull(r.externalReport)

proc reportHook*(conf: ConfigRef, r: Report): TErrorHandling =
  let wkind = conf.writabilityKind(r)
  if wkind == writeDisabled:
    return

  elif wkind in { writeForceEnabled, writeEnabled }:
    echo conf.reportFull(r)
    echo ""

  else:
    echo "?"
