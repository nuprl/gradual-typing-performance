funkytown
=========

Funkytown benchmark.
Original code taken from [the chaperones benchmarks](http://github.com/stamourv/contract-benchmarks) and the Racket [math/array](http://docs.racket-lang.org/math/array.html) library.

Run `main.rkt` to create (and discard) some music.

Some modules are taken from the `math/array` library.
Remember, an array is a function with a rectangular domain.
- `array-broadcast.rkt` interface to fine-tune array operations.
- `array-for-each.rkt` syntax rules to iterate over arrays.
- `array-pointwise.rkt` defines the `array-map` function (which is only used by `mix`).
- `array-struct.rkt` basic array operations
- `array-transform.rkt` defines the `array-append*` function (used by `drum` and `sequencer`).
- `array-types.rkt` type and data definitions (shared file for typed/untyped implementations).
- `array-utils.rkt` important helpers for working with arrays (could probably merge with `array-struct`).

Other modules implement the benchmark.
- `drum.rkt` builds drum beats.
- `funky-town.rkt` main function: build a sequence of integers from some notes.
- `mixer.rkt` combine some weighted signals into a single signal.
- `sequencer.rkt` interpret note representations into an array of floats.
- `synth.rkt` utils, build oscillators and convert music to integers.
