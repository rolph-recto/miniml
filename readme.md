# MiniML

A compiler for a subset of OCaml.

## Roadmap:

Implemented:
- lexer and parser

Todo:
- FIX operator
- Hindley-Milner type-checker
  - typechecking rules for patterns, tuples, matches, records and fields
- frontend for viewing multiple compiler passes simultaneously
  - have ability to highlight corresponding sections of code between passes
- records will add a lot of complexity to the typechecker. should we remove them?
