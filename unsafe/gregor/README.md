gregor
======

Adapted from the [gregor](https://github.com/97jaz/gregor) time and date library, by `97jaz`.

Benchmark creates a bunch of datetime objects and runs library functions on them.

Notes
-----
- Added Adapters:
  - `tzinfo-adapter.rkt` to interface with `tz` library
  - `core-adapter.rkt` to interface with core modules
  - `gregor-adapter.rkt` to handle the main structs
- Removed all generics (generics are incompatible with typed racket)
- Removed `compare.rkt` because of issues passing "higher order value as Any"
- Renamed `time` function, to avoid colliding with `racket/base`
