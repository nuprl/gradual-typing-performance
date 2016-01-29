Zordoz
======

Simple bytecode analyzer.
Run `racket main.rkt` to run a small stress test, searching bytecode file(s) for occurrences of `branch` zo structs.

- `dispatch-table.rkt` macro for `zo-transition` and `zo-string`.
- `zo-string.rkt` convert any zo-struct to a struct
- `zo-transition.rkt` access zo-struct fields using a string at runtime
- `zo-find.rkt` search a zo-struct for sub-structs matching a given name
- `zo-shell.rkt` interactive front-end for the zo functions

The base folder contains
- bytecode for all files in this project
- `hello-world.zo` bytecode for a simple "hello world" program
- `large-test.zo` large bytecode file from the racket repository (gui toolkit, or something)

---

The core data are zo structs from the compiler library.
These structs are UNTYPED.
Typed files go through a contract boundary.
