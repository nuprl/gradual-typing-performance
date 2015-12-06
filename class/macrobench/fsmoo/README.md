fsmoo
=====

What happened here?

- 0100 is BAD (only `main.rkt` is typed)
- Boundary between `main` and `population` is crucial


Seems like the same issue. Using objects "too functionally".

Yes, yes it is.
Changing 2 return types from "this" to "void" solved it.
