meeting-12-04
===

01-fsmoo
---

- 100,000x slowdown (100 ms to 18-million ms)
- worst case happens when MAIN is typed
  (modules are `main automata population utilities`
   bad boundary: main -- population)
- main runs an "evolve" loop
  - match up
  - compute payoff
  - death-birth
- loop looks function, but really changes state in a population object!
- solution: change "this" to "void" in 2 method returns

Down to 170 ms.
(Still have a modest 40,000ms bottleneck in some cases)


02-forth
---

- 1.5 million slowdown (1.2 ms to 1.8 million ms)
- modules are `main eval command stack`
  eval -- command is the bad boundary
- Problem: command are functions on the context.
  The context contains commands (objects)
- Exponential slowdown in number of commands
- Keeping the context outside & updating statefully fixes it

Down to 3.7x slowdown, now possible to run the larger test case
(Still modest 20x slowdown in some cases)


03-microbench
---

A microbenchmark illustrating the problem.

1. Create a "sufficiently complicated" class.
   Enough to warrant a higher-order contract.
   (Mixins are perfect)
2. Create a mixin M. It can be the identity function.
3. Repeatedly call (M (M (M ...))) on the class

Question: when is the contract being checked?


04-ideas
---

1. Typeful contracts would avoid creating the higher-order wrapper _in the mixin_.
2. ???
