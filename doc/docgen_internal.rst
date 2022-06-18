=====================================
   Nim DocGen Tools implementation
=====================================

.. default-role:: code
.. include:: rstcommon.rst
.. contents::

:Author: haxscramper
:Version: |nimversion|


.. raw:: html
  <blockquote><p>
  "Documentation - a set of all information used by the developer or user to understand the principles of operation and use of the system." -- haxscramper
  </p></blockquote>

Introduction
============

This document provides explanation of the internal structure and
implementation of the documentation. First, high-level overview is provided
and then more detailed explanation is offered, with links to the relevant
documentation and module sections.

Second part of this document provides explanation on usage of the
documentation generator as a code analysis and data extraction tool. If you
are looking to implement a completely new output generator for the
documentation (either reimagining current HTML output, or supporting new
output format) or get some insights from the collected data you should look
into this section.

Internal implementation
=======================

Custom targets
==============

Previous section explains the full pipeline in more details, but for this
one you only need to know a general algorithm for data processing:
generator compiles and analyses all the code and generates intermediate
SQLite database, putting all the data into it. Then, second stage takes the
database and converts it into HTML you see in the default documentation.
Here we explain how to re-implement second part of this process: what do
you need to know, how to use this knowledge and so on.

Database structure
------------------

Database consists of several tables with similar structure. Most entries
have unique primary ID. All tables are semi-automatically generated and
populated (see implementation in the `docgen_sqlite.nim`) based on types
defined in `docgen_types.nim`

1. `occurrences` - information about concrete location of the entry uses
   in the source code.
   + `kind`: column indicates the type of usage, with specific usage *name*
     stored in the `DocOccurKind` (1:1 mapping of the ) table. Specific
     values of the occurrence kinds are subject to change, so better use
     their explicit names.
   + `refid` (refs `entries.id`): used entry
   + `loc` (refs `locations.id`): location where occurrence had been
     registered.
   + `user` (refs `entries.id`): column shows the id of the user
     (`entries.id`). For example, when one procedure (`procA()`) calls
     another one (`procB()`), occurrence will have `refid=<id of procB>`
     and `user=<id of procA>`
2. `docs` - textual content of the documentation. Contains all the
   user-provided explanation - both regular text and runnable examples.
   + `runnable`: whether piece of text is a runnable code example or a
     freeform text.
   + `implicit`: if the code is an implicitly runnable example contains
     the ID of the file from which code had been extracted.
   + `text`: original text or code
   + `location` (refs `locations.id`): ID of the location of the text
   + `tree`: if entry is not a runnable examples, contains serialized
     **processed** tree in JSON form. For more details see the next section
     on documentation text representation.
3. `entries`: complete list of all documentable entries that had ever
   been encountered in the code
   + `name`: the textual name (not unique) of the documented entry.
   + `kind`: the type of the found object, which in turn refers to
     `DocEntryKind` (reasoning is identical with `DocOccurKind`), which is
     responsible for storing the names of all possible and supported types.
   + `extent` (refs `extents.id`): id of the range of the found object in
     the source code. Using this field, you can access the full range of
     source code from which the entry was generated.
   + `location` (refs `locations.id`): location of the object in the source
     code. Using this field, you can access the location where the object
     was defined.
   + `parent` (refs `entries.id`): Optional id of the parent entry (if
     any). This field allows for tree-like structure of the project
     documentation to be preserved.
   + `node`: column contains a textual representation of the part of the
     syntax tree from which the entry was generated.
4. `docMap`: 1:N mapping between entry IDs and all the associated pieces of
   documentation.
   + `entry` (refs `entries.id`): Documentation target id
   + `idx`: Which piece of the documentation it is. Procedure might have
     several chunks - text spliced with blocks of `runnableExample`. This
     field allows to discern their order.
   + `doc` (refs `docs.id`): the content itself.
5. `locations`: information about specific singular locations in the code.
   + `file` (refs `files.id`): ID of the file in the `files` table
   + `line`, `col_end`, `col_start`: integers with line + column range of
     the location
6. `extents`: information about range of the source code. Structurally
   similar to the `locations`, but provides both `line_start` and
   `line_end` instead of a single point in code.
7. `files`: Extended information about files used in the project. Contains
   ID, absolute (`abs`) and relative (`rel`) (to the project) fields.
8. `deprecated`: Extra information about deprecation annotations for
   entries that had it.
   + `id` (refs `entries.id`): deprecation annotation target
   + `msg`: Deprecation message if any.
