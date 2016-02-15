acquire
===

Revision of Acquire benchmark.

- Contracts are encoded as preconditions (see the `ext:...` functions)
- No unit tests
- Made `player-external` field be an `(Instance Player%)` object, instead of `Any`



vs. ECOOP'15
---

Compared to the [ECOOP'15](http://drops.dagstuhl.de/opus/volltexte/2015/5215/pdf/5.pdf) version:

- Converts 8 modules (enough to run the benchmark) instead of 4.
  - 1978 lines typed vs. ECOOP's 563 typed lines (comments excluded)
  - ECOOP ported `admin.rkt`, `player-factory.rkt`, `player.rkt`, and `tree.rkt`.
    We cut most of `player-factory` and `tree` as the benchmark didn't use it,
     and moved the remainder into their only client (`admin` and `player`, respectively).
- The type definitions are very similar (thanks Matthias!)
- ECOOP ignored dynamic checks (for example, `*create-player` in `state.rkt`)
