import
  ast/[ast, renderer],
  sem/[
    passes,
    semdata
  ],
  modules/modulegraphs,
  experimental/dod_helpers,
  std/[
    options,
    intsets,
    tables,
    hashes,
    strformat,
    sequtils,
    strutils
  ]

type
  DocEntryKind* = enum
    ## - NOTE :: Different procedure kinds are also used to describe
    ##   operator implementations.
    # procedure kinds start
    ndkProc = "proc" ## Procedure definition
    ndkFunc = "func" ## Procedure definition

    ndkMacro = "macro" ## Macro
    ndkMethod = "method" ## Method
    ndkTemplate = "template" ## \
    ndkIterator = "iterator" ## \
    ndkConverter = "converter" ## User-defined implicit conversion
    # procedure kinds end


    ndkParam = "param" ## Generic parameters
    ndkArg = "arg" ## Entry (function, procedure, macro, template) arguments
    ndkInject = "inject" ## Variable injected into the scope by
    ## template/macro instantiation.
    ndkPragma = "pragma" ## Compiler-specific directives `{.pragma.}` in
    ## nim, `#pragma` in C++ and `#[(things)]` from rust.

    # new type kinds start
    ndkBuiltin = "builtin" ## Builtin type, not defined using any other types
    ndkObject = "object"
    ndkException = "exception" ## Exception object
    ndkDefect = "defect" ## Nim defect object
    ndkConcept = "concet" ## General concept
    ndkTypeclass = "typeclass"
    ndkUnion = "union"
    ndkEnum = "enum" ## Enumeration
    ndkEffect = "effect" ## Side effect tag
    ndkAlias = "alias" ## Typedef
    ndkDistinctAlias = "distinct" ## strong typedef


    # variable-like entries
    ndkCompileDefine = "define" ## Compile-time `define` that might affect
    ## compilation of the program.

    ndkGlobalConst = "globalconst" ## Global immutable compile-time constant
    ndkGlobalVar = "globalvar" ## Global mutable variable
    ndkGlobalLet = "globallet" ## Global immutable variable
    ndkField = "field" ## object/struct field
    ndkEnumField = "enumfield" ## Enum field/constant
    # end

    # merging all toplevel documentation elemetns into parent module is a
    # wrong approach, because we are loosing the ordering provided by the
    # programmer, and won't be able to recover it later on without
    # implementing annoying location-dependent ordering hacks.
    ndkComment = "comment" ## Toplevel documentation comment that is not
    ## directly attached to any entry (first 'comment' entries in the
    ## module are attached to said module, but it is also possible to place
    ## ones in the middle of the module - those will go into nested comment
    ## entries)
    ndkModule = "module" ## Module (C header file, nim/python/etc. module)
    ndkFile = "file" ## Global or local file
    ndkPackage = "package" ## System or programming language package
    ## (library). If present used as toplevel grouping element.

  DocProcKind* = enum
    dpkRegular = "regular"
    dpkOperator = "operator"
    dpkConstructor = "=init"
    dpkDestructor = "=destroy"
    dpkMoveOverride = "=sink"
    dpkCopyOverride = "=copy"
    dpkAsgnOverride = "asgn"
    dpkPropertyGet = "getter"
    dpkPropertySet = "setter"
    dpkPredicate = "predicate"



const
  ndkStructKinds* = {
    ndkObject, ndkDefect, ndkException, ndkEffect
  }

  ndkNewtypeKinds* = { ndkObject .. ndkDistinctAlias }
  ndkProcKinds* = { ndkProc .. ndkConverter }
  ndkAliasKinds* = { ndkTypeclass, ndkAlias, ndkDistinctAlias }

type
  DocOccurKind* = enum
    dokNone

    dokTypeDirectUse = "typeDirect" ## Direct use of non-generic type
    dokTypeAsParameterUse = "typeAsParam" ## Use as a parameter in generic
                                          ## specialization
    dokTypeSpecializationUse = "typeAsSpecialization" ## Specialization of
                             ## generic type using other types

    dokTypeAsArgUse = "typeasArg"
    dokTypeAsReturnUse = "typeAsReturn"
    dokTypeAsFieldUse = "typeAsField"
    dokTypeConversionUse = "typeconv"

    dokUsage = "usage"
    dokCall = "call"
    dokExpansion = "expansion"

    dokInheritFrom = "inheritFrom"
    dokOverride = "override"
    dokMacroUsage = "macroUse"
    dokAnnotationUsage = "annotation"

    # local section start
    dokLocalUse = "localUse" ## Generic 'use' of local entry
    dokLocalWrite = "localWrite"
    dokLocalRead = "localRead"


    # local declaration section start
    dokLocalArgDecl = "localArgDecl"
    dokLocalVarDecl = "localVarDecl"
    # local declarations section end
    # local section end

    dokGlobalWrite = "globalWrite" ## Asign value to global variable
    dokGlobalRead = "globalRead" ## Non-asign form of global variable
    ## usage. Taking address and mutating, passing to function that accepts
    ## `var` parameter etc. would count as 'read' action.
    dokGlobalDeclare = "globalDecl"

    dokFieldUse = "fieldUse"
    dokFieldSet = "fieldSet"
    dokEnumFieldUse = "enumFieldUse"

    dokFieldDeclare = "fieldDecl"
    dokCallDeclare = "callDecl"
    dokAliasDeclare = "aliasDecl"
    dokObjectDeclare = "objectDecl"
    dokEnumDeclare = "enumDecl"
    dokEnumFieldDeclare = "enumfieldDecl"

    dokDefineCheck

    dokImported = "imported"
    dokExported = "exported"
    dokIncluded = "included"

    dokInMacroExpansion = "inExpansion"


const
  dokLocalKinds* = {dokLocalUse .. dokLocalArgDecl }
  dokLocalDeclKinds* = { dokLocalArgDecl .. dokLocalVarDecl }

declareIdType(Expansion, addHash = true)
declareIdType(DocOccur, addHash = true)
declareIdType(DocEntry, addHash = true)

type
  Expansion* = object
    ## Information about macro or template expansion
    expansionOf*: PSym ## Expanded symbol
    expandDepth*: int ## Current active macro/template expansion depth
    expandedFrom*: PNode ## Original expression that node expanded from
    immediateResult*: PNode
    resultNode*: PNode ## Resulting expanded node
    expansionUser*: DocEntryId ## Parent documentable entry that contained macro
    ## expansion (for toplevel entries it is a module)
    nested*: seq[ExpansionId] ## List of the nested expansions in
    resolveMap*: Table[int, PSym] ## Map form node ids generated in the
    ## immediate macro expansion, to their final symbols.

declareStoreType(Expansion)

type
  DocCodeLocation* = object
    ## Single continious slice of the code in file -
    ## file/line:col-start:col-end information
    file*: FileIndex
    line*: int ## Code slice line /index/
    column*: Slice[int] ## Column slice - start and stop position


  DocOccur* = object
    ## Single occurence of documentable entry. When DOD AST and token
    ## storage is implemented
    ## (https://github.com/nim-works/nimskull/discussions/113) this will be
    ## replaced by extra data table associated with each token.
    user*: Option[DocEntryId] ## For occurence of global documentable
    ## entry - lexically scoped parent (for function call - callee, for
    ## type - parent composition). For local occurence - type of the
    ## identifier (for local variables, arguments etc).
    inExpansionOf*: Option[ExpansionId]

    slice*: DocCodeLocation ## Position of the occurence in the project
    ## files.
    node*: PNode ## Node that occurence happened in. In the future this
    ## should be converted to node IDs
    case kind*: DocOccurKind ## Type of entry occurence
      of dokLocalKinds:
        localId*: string
        withInit*: bool ## For 'local decl' - whether identifier
        ## was default-constructed or explicitly initialized.

      else:
        refid*: DocEntryId ## Documentable entry id


declareStoreType(DocOccur)

type
  DocTypeHeandkind* = enum
    dthGenericParam ## Unresolved generic parameter
    dthTypeclass ## Typeclass
    dthConcreteType ## Concrete resolved class

  DocIdentKind* = enum
    diValue ## Pass-by value function argument or regular identifier
    diPointer ## Identifier passed by pointer
    diMutReference ## Mutable reference
    diConstReference ## Immutable reference
    diSink ## rvalue/sink parameters

  DocIdent* = object
    ## Identifier.
    ##
    ## - WHY :: Callback itself is represented as a type, but it is also
    ##   possible to have named arguments for callback arguments (though
    ##   this is not mandatory). @field{entry} should only point to
    ##   documentable entry of kind [[code:ndkField]].

    ident*: string ## Identifier name
    kind*: DocIdentKind ##
    identType*: PType ## Identifier type
    value*: Option[string] ## Optional expression for initialization value
    entry*: DocEntryId

  DocEntrySet* = object
    ids*: IntSet

  DocIdTableN* = object
    table*: Table[DocEntryId, DocEntrySet]

  DocEntryGroup* = ref object
    ## Arbitrary grouping of the documentable entries
    entries*: seq[DocEntryId]
    nested*: seq[DocEntryGroup]

  DocExtent* = object
    start*: TLineInfo
    finish*: TLineInfo

  DocTextPart* = object
    ## Single chunk of documentation text, can be either source code, or
    ## runnable examples.
    text*: string
    case isRunnable*: bool
      of true:
        implicitModule*: DocEntryId ## Current module id, implicitly
        ## available in the runnable examples execution environment.

      of false:
        discard

  DocText* = object
    ## Full documentation text for an entry, might consist of several parts
    ## - text blocks or runnable examples.
    parts*: seq[DocTextPart]

  DocVisibilityKind* = enum
    dvkPrivate = "private" ## Not exported
    dvkInternal = "internal" ## Exported, but only for internal use
    dvkPublic = "public" ## Exported, available for public use

  DocRequires* = object
    name*: string
    version*: string # TODO expand
    resolved*: Option[DocEntryId]

type
  DocDeclarationContext* = object
    preSem*: bool ## Whether documentation entry was registered before
                  ## semantic pass or after.
    ## Active context of the documentable entry declarations
    whenConditions*: seq[PNode] ## List of nested 'when' statements that were
    ## encountered during recursive visitation.

  DocPotentialDependency* = object
    context*: DocDeclarationContext

  DocEntry* = object
    ## Documentation entry for the
    sym*: PSym ## Symbol that documentable entry was generated from.
    ## Not all entries have that - `define()` targets, projects, libraries
    ## and other elements might be constructed only during pre-sem analysis
    ## and as a result might not have the symbols available.
    node*: PNode ## Node that documentable entry was generated from

    context*: DocDeclarationContext
    location*: Option[TLineInfo]
    extent*: Option[DocExtent]
    declHeadExtent*: Option[DocExtent] ## Source code extent for
    ## documentable entry 'head'. Points to single identifier - entry name
    ## in declaration.
    ## - WHY :: Used in sourcetrail
    nested*: seq[DocEntryId] ## Nested documentable entries. Use to store
    ## information about fields of a type (for variant fields this might have
    ## more nested fields), arguments of a procedure, enum values and so on.
    ## Module has all elements declared in it listed here, project has a list of
    ## modules
    parent*: Option[DocEntryId] ## Parent declaration entry
    name*: string ## Original identifier name, not disambiguated, without any
    ## extra information about owner/type/generic parameters and so on.
    visibility*: DocVisibilityKind ## Entry visibility from the
    ## documentation reader prespective
    deprecatedMsg*: Option[string] ## If entry was annotated with
    ## `{.deprecated.}` contains the pragma text.
    docText*: DocText

    case kind*: DocEntryKind
      of ndkPackage:
        version*: string ## Textual version of the package
        author*: string ## Package author name
        license*: string ## Package license as written in the manifest file
        requires*: seq[DocRequires] ## List of required packages

      of ndkModule:
        imports*: DocEntrySet ## Modules imported by this module
        exports*: DocEntrySet ## Documentable entries (modules, procs, types)
        ## exported by the module.
        includes*: DocEntrySet ## Visited includes

        maybeIncludes*, maybeImports*: seq[DocPotentialDependency]

      of ndkStructKinds:
        superTypes*: seq[DocEntryId]

      of ndkEnumField:
        enumValueOverride*: Option[PNode]
        enumStringOverride*: Option[string]

      of ndkArg:
        argType*: PNode ## Argument type description
        argDefault*: PNode ## Expression for argument default value.

      of ndkField:
        fieldType*: PNode
        switchesInto*: seq[tuple[
          expr: seq[PNode], subfields: seq[DocEntryId]]] ## Identifiers for
        ## nested fields that can be accessed based on this field's value

      of ndkAliasKinds:
        baseType*: PNode ## Base type /expression/ of the alias. Might contain
        ## generic type with multiple parameters, so `PNode` is used here
        ## instead of `DocEntryId`

      of ndkProcKinds:
        procKind*: DocProcKind
        wrapOf*: Option[string] ## Optional C, C++ or JS pattern used
        ## in the `.importX` pragma annotation
        dynlibOf*: Option[string] ## Dynamic library pattern for the
        ## procedures
        calls*: DocEntrySet ## Procedures called by entry, for callgraph
        ## construction.
        raises*: DocEntrySet ## Full list of potential raises of a procedure
        ## including both direct and indirect ones
        effects*: DocEntrySet ## All effects for procedure body, including
        ## direct and indirect ones.
        raisesVia*: Table[DocEntryId, DocEntrySet] ## Mapping between
        ## particular raise and called procedure. Direct raises via `raise`
        ## statement are not listed here.
        raisesDirect*: DocEntrySet ## List of exception types that
        ## can be directly raised by the body
        effectsVia*: Table[DocEntryId, DocEntrySet] ## Effect -> called
        ## procMapping. Allows to provide information about causes of the
        ## particular effects in the procedure.
        globalIO*: DocEntrySet ## Global variables that procedure reads from
        ## or writes into.

      else:
        discard

declareStoreType(DocEntry)

type
  DocDb* = ref object of RootRef
    entries*: DocEntryStore
    currentTop*: DocEntry
    top*: seq[DocEntryId]
    named*: Table[string, DocEntryId]
    expandedNodes*: Table[int, ExpansionId]
    expansions*: ExpansionStore ## List of known expansion bettween
    ## open/close for module
    occurencies*: DocOccurStore
    sigmap*: Table[PSym, DocEntryId]

  DocPreContext* = ref object of TPassContext
    ## Initial documntation analysis context that constructs a list of potential
    ## documentable entries using pre-sem visitation.
    db*: DocDb
    graph*: ModuleGraph
    docModule*: DocEntryId ## Toplevel entry - module currently being
    ## processed

  DocContext* = ref object of PContext
    ## Documentation context object that is constructed for each open and close
    ## operation. This documentation further elaborates on analysis of the daa

    db*: DocDb ## Documentation database that is persistent across all
    ## processing passes, for both pre-sem and in-sem visitation.
    docModule*: DocEntryId ## Toplevel entry - module currently being
    ## processed

    inModuleBody*: bool
    # Fields to track expansion context
    activeExpansion*: ExpansionId ## Id of the current active expansion.
    ## Points to valid one only if the expansion stack is not empty,
    ## otherwise stores the information about the last expansion.
    expansionStack*: seq[ExpansionId] ## Intermediate location for
    ## expansion data store - when expansion is closed it is moved to
    ## `db.expansion`
    toplevelExpansions*: seq[ExpansionId] ## List of toplevel macro or
    ## template expansions that were registered in the module

# DOD helper function declarations for the documentable entry database.
declareStoreField(DocDb, entries, DocEntry)
declareStoreField(DocDb, expansions, Expansion)
declareStoreField(DocDb, occurencies, DocOccur)

func add*(de: var DocEntry, id: DocEntryId) =
  de.nested.add id

func addTop*(db: var DocDb, entry: DocEntry): DocEntryId =
  result = db.add entry
  db.top.add result

func len*(s: DocEntrySet): int = s.ids.len
func incl*(s: var DocEntrySet, id: DocEntryId) =
  if id.int != 0:
    s.ids.incl id.int

func `*`*(s1, s2: DocEntrySet): DocEntrySet = DocEntrySet(ids: s1.ids * s2.ids)
func `-`*(s1, s2: DocEntrySet): DocEntrySet = DocEntrySet(ids: s1.ids - s2.ids)

func excl*(s: var DocEntrySet, id: DocEntryId) = s.ids.excl id.int
func contains*(s: DocEntrySet, id: DocEntryId): bool = id.int in s.ids
iterator items*(s: DocEntrySet): DocEntryId =
  for i in s.ids:
    yield DocEntryId(i)

func pop*(s: var DocEntrySet): DocEntryId =
  for it in s:
    result = it
    s.excl result

func incl*(table: var DocIdTableN, idKey, idVal: DocEntryId) =
  table.table.mgetOrPut(idKey, DocEntrySet()).incl idVal

func incl*(s: var DocEntrySet, entry: DocEntry) =
  s.incl entry

proc getSub*(db: DocDb, parent: DocEntryId, subName: string): DocEntryId =
  ## Get nested entry by name. Might return empty doc entry id if name is
  ## not found.
  for sub in db[parent].nested:
    if db[sub].name == subName:
      return sub

proc newDocEntry*(
    db: var DocDb, kind: DocEntryKind, name: string,
    context: DocDeclarationContext = DocDeclarationContext()
  ): DocEntryId =
  ## Create new toplevel entry (package, file, module) directly using DB.
  result = db.add(DocEntry(name: name, kind: kind, context: context))

proc newDocEntry*(
    db: var DocDb,
    parent: DocEntryId, kind: DocEntryKind, name: string,
    context: DocDeclarationContext = DocDeclarationContext()
  ): DocEntryId =
  ## Create new nested document entry. Add it to subnode of `parent` node.
  result = db.add DocEntry(
    name: name, kind: kind, context: context, parent: some parent)
  db[parent].nested.add result

proc getOrNewNamed*(
    db: var DocDb, kind: DocEntryKind, name: string): DocEntryId =
  ## Get new documentable entry or construct a new one using kind and name.
  ## Used primarilily for documentable entries such as `define()` flags,
  ## that have no definition itself, and need to be added when first used.
  if name in db.named:
    result = db.named[name]

  else:
    result = db.newDocEntry(kind, name)
    db.named[name] = result

func isFromMacro*(db: DocDb, node: PNode): bool =
  ## Check if the node node was created during macro expansion
  assert not node.isNil()
  node.id in db.expandedNodes

proc getExpansion*(db: DocDb, node: PNode): ExpansionId =
  ## Get expansion tha tnode was generated from
  return db.expandedNodes[node.id]

func `$`*(slice: DocCodeLocation): string =
  &"{slice.file.int}/{slice.line}:{slice.column.a}..{slice.column.b}"

func initDocPart*(str: string): DocTextPart =
  DocTextPart(text: str, isRunnable: false)

func initDocText*(str: string): DocText =
  DocText(parts: @[initDocPart(str)])

proc fullName*(db: DocDb, id: DocEntryId): string =
  result.add db[id].name
  case db[id].kind:
    of ndkProcKinds:
      result.add "("
      for idx, arg in db[id].nested:
        if 0 < idx: result.add ", "
        result.add db[arg].name
        result.add ": "
        result.add $db[arg].argType

      result.add ")"

    of ndkField:
      result.add ": "
      result.add $db[id].fieldType

    else:
      discard


proc `$`*(db: DocDb, id: DocOccurId): string =
  var r: string
  template e(): untyped = db[id]

  r.add &"[{id.int}]: {e().kind}"
  if e().kind notin dokLocalKinds and
     not e().refid.isNil():

    r.add &" of [{e().refid.int}] "
    r.add &"({db[e().refid].kind} '{db.fullName(e().refid)}')"

  r.add &" at {e().slice}"

  if e().user.isSome():
    r.add &" user: {e().user.get().int}"

  if e().inExpansionOf.isSome():
    r.add &" expansion: {e().inExpansionOf.get().int}"

  return r

proc `$`*(db: DocDb, id: DocEntryId): string =
  var r: string
  template e(): untyped = db[id]

  r.add &"[{id.int}]: {e().visibility} {e().kind} '{db.fullName(id)}'"

  if e().parent.isSome():
    r.add &" parent [{e().parent.get.int}]"

  if e().location.isSome():
    let l = e().location.get()
    r.add &" in {l.fileIndex.int}({l.line}, {l.col})"

  if e().context.preSem:
    r.add " (presem)"

  if e().context.whenConditions.len > 0:
    let conds = e().context.whenConditions.mapIt(
      "(" & $it & ")").join(" and ")

    r.add " available when "
    r.add conds

  if e().deprecatedMsg.isSome():
    r.add " deprecated"
    if e().deprecatedMsg.get().len > 0:
      r.add ": '"
      r.add e().deprecatedMsg.get()
      r.add "'"

  if e().docText.parts.len > 0:
    r.add " doc: '"
    for part in e().docText.parts:
      r.add part.text.replace("\n", "\\n")

    r.add "'"

  return r
