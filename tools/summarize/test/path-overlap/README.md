path-overlap
===

Check that gtp-trace still works if we happen to use a file
 with the same name from a different directory.

1. Define 2 project files, "main.rkt" and "lib.rkt"
2. Define 1 library file, "src/lib.rkt"
3. Run gtp-trace, make sure imports from "src/lib.rkt" were not replaced
