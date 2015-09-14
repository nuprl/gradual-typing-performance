Many Adaptor
============

Re-editions of certain benchmarks with many adaptors, instead of just one.
Should remove bottlenecks.

- gregor : the `gregor-structs` adaptor took definitions from 4 modules
- suffixtree : the `data.rkt` adaptor took definitions from label & structs
- synth : the `array-structs` adaptor removed the mutable-array file


Warning
-------
Failed to adapt `synth` on the first go -- can't be faithful to the original structure.

Get an error "attempt to use typed struct reflectively in untyped code".
(I tried the `define-struct ... #:prefab)` trick and got "make-struct-type: chaperoned supertype disallowed for non-generative structure type"

