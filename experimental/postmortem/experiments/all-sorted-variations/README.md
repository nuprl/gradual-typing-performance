All Sorted Variations
=====================

Comparing the runtime of variations shows some interesting patterns

##### Typed vs. Untyped Data
- Gregor, LNM, Quad, Snake, Suffixtree, Tetris do worst with untyped data
  - TODO % for each
- Synth does worst with typed data
  - Top 8.6%, from 64x to 85x overhead, all have typed data
  - Benchmark spends more time building structures than accessing them
    - mixer, array-map, 26% of contracts
    - transform, append, 12% of contracts
    - synth, next-indexes! (mutation), 8% of contracts


##### Library Dependencies
- Confirms the importance of library boundaries
  - `gregor` : Best variations have `moment.rkt` untyped
  - `lnm` : Performance better than untyped if either `lnm-plot` or `summary` are typed
  - `mbta` : All with `t-graph` typed are 2.59x -- 2.80x
  - 


##### Tight Coupling
- `gregor` : `clock.rkt` and `offset-resolvers.rkt`
- `kcfa` : `denotable.rkt` and everything (denotable is the store)
           But `denotable`, `ai`, and `ui` combined is 0.94x
- `morsecode` : Typing the table & string->morse converter speeds things a little
- `suffixtree`: `data.rkt` and `label.rkt`, the worst are when they're flipped (bounce up to 73x--100x)
- `synth` : `data.rkt` and `array-structs.rkt`. Also see the "adaptor-free" version
- `tetris` : Typed `block` and untyped `bset` is very bad


##### Client/Server
Clients are typed in the best case, servers untyped
- `gregor` : `clock` should be typed, moment should be untyped.
- `synth` : Typed-checked constructors are a bottleneck


##### Confusion
- `quad` : The best have `quick-sample` typed, and all else untyped


