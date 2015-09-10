Changelog
=========

Describes recent changes to MBTA and zordoz.
The pdf shows full experimental results.


Changes to MBTA:
- Calling `run-t` no longer spawns a thread
- Main module & `run-t` pass strings directly, instead of reading ports
- Longer stress test: takes 700ms untyped instead of 60


Changes to zordoz:
- Updated structs to match current bytecode format
- Using "same" test on both versions.
  Explores its own bytecode tree exhaustively for `branch` structs.


Error
-----

One more problem: cannot run zordoz 6.2.900.15 through the profiler on its
"own code" test right now.

```
compiled/expanded code out of context; cannot find exports to restore imported renamings for module: "/home/ben/code/racket/fork/racket/share/pkgs/typed-racket-lib/typed-racket/typecheck/tc-structs.rkt"
  context...:
     /home/ben/code/racket/fork/racket/collects/syntax/private/id-table.rkt:77:2: do-ref
        /home/ben/code/racket/fork/racket/share/pkgs/typed-racket-lib/typed-racket/rep/interning.rkt:22:13: *Name
           (submod /home/ben/code/racket/benchmark/gradual-typing-performance/zordoz.6.2.900.15/benchmark/base/typed-zo-structs.rkt #%type-decl): [running body]
              /home/ben/code/racket/benchmark/gradual-typing-performance/zordoz.6.2.900.15/benchmark/variation01000/main.rkt: [running body]
```
