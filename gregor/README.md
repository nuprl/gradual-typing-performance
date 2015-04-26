gregor
======

Adapted from the [gregor](https://github.com/97jaz/gregor) time and date library, by `97jaz`.

Benchmark creates a bunch of datetime objects and runs library functions on them.

Notes
-----
- Added Adapters:
  - `tzinfo-adapter.rkt` to interface with `tz` library
  - `structs-adapter.rkt` to interface with core modules
- Made psuedo-adapters
  - `datetime.rkt` exports opaque types from `date.rkt` and `time.rkt`, to avoid generating new type definitions
  - `clock.rkt` does likewise, for `moment.rkt` and `moment-base.rkt`
- Removed all generics (generics are incompatible with typed racket)
