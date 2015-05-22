funkytown
=========

Funkytown benchmark, micro-ized.
Original code taken from [the chaperones benchmarks](http://github.com/stamourv/contract-benchmarks) and the Racket [math/array](http://docs.racket-lang.org/math/array.html) library.

Run `main.rkt` to create (and discard) some music.

Some quirks about the micro version
- Macros are bundled with the function they affect (luckily no functions shared macros)
- `array-transform.rkt` is not micro'd, because it contains exactly two mutually recursive functions.
- Tried and failed to separate the 3 structs into separate files. Still debugging the issue (2015-05-08).
