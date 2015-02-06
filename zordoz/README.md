Zordoz
======

Simple bytecode analyzer.
Run all tests with `racket main.rkt`.

- `dispatch-table.rkt` macro for `zo-transition` and `zo-string`.
- `zo-string.rkt` convert any zo-struct to a struct
- `zo-transition.rkt` access zo-struct fields using a string at runtime
- `zo-find.rkt` search a zo-struct for sub-structs matching a given name
- `zo-shell.rkt` interactive front-end for the zo functions
