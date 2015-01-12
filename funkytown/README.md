funkytown
=========

Gradually typed funkytown benchmark.
Original code taken from [the chaperones benchmarks](http://github.com/stamourv/contract-benchmarks) and the Racket [math/array](http://docs.racket-lang.org/math/array.html) library.

Sub-folders implement funkytown suite using different amounts of type information.

- `funk-master/` is the original suite (containing the contract-benchmarks source and the essentials from the math/array library).
- `funk-untyped/` is a fully untyped implementation
- `funk-flat-contracts/` modifies the untyped implementation to include `define/contract` on every procedure. All contracts are flat, like `vector?`.
- `funk-mix-typed/` uses an optimization for `funk-master` due to `stamourv` -- the module `mix.rkt` is typed.
- `funk-math-typed/` uses a fully-typed math library and fully untyped funkytown

Companion spreadsheet online here:
https://docs.google.com/spreadsheets/d/1g-CtozvAZ5zKMNxBJVhfS-i4VcJwREr8XMyFIuRMt_Y/edit?usp=sharing
