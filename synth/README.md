synth
=====

Synth benchmark.
Original code taken from [the chaperones benchmarks](http://github.com/stamourv/contract-benchmarks) and the Racket [math/array](http://docs.racket-lang.org/math/array.html) library.

Run `main.rkt` to create (and discard) some music.

Some modules are taken from the `math/array` library.
(Remember, an array is a function with a rectangular domain.)
- `array-broadcast.rkt` tweak array performance (strict vs lazy)
- `array-struct.rkt` basic array operations
- `array-transform.rkt` defines the `array-append*` function (used by `drum` and `sequencer`).
- `array-utils.rkt` important helpers for working with arrays

Other modules implement the benchmark.
- `drum.rkt` builds drum beats.
- `funky-town.rkt` main function: build a sequence of integers from some notes.
- `mixer.rkt` combine some weighted signals into a single signal.
- `sequencer.rkt` interpret note representations into an array of floats.
- `synth.rkt` utils, build oscillators and convert music to integers.

---

This version uses an ADAPTER module.
Thus typed modules get typed data (through the adapter)
and untyped modules get untyped data.
