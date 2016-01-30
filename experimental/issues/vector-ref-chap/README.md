troubles
====

Difficulties running the lattice.

#### vector-ref-chap/
Maybe due to the `vector-set!` that happen in the loop?

```
vector-ref: chaperone produced a result that is not a chaperone of the original result
  chaperone result: #<a10>
  original result: #<a10>
  context...:
  /home/ben/code/racket/benchmark/gradual-typing-performance/sample-fsm/foobar/population.rkt:56:2: pair-up-loop
  /home/ben/code/racket/benchmark/gradual-typing-performance/sample-fsm/foobar/evolution.rkt:21:0: evolve
  /home/ben/code/racket/benchmark/gradual-typing-performance/sample-fsm/foobar/main.rkt:35:0: simulation->lines
  .../more-scheme.rkt:336:52
  /home/ben/code/racket/benchmark/gradual-typing-performance/sample-fsm/foobar/main.rkt: [running body]
```
