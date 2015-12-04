forth
===

Why so slow?

- Original program ran quickly
- OO program is 1 million X slowdown


A fix
- Stop passing env + stack to commands, do stateful update. Down to 20x
  (really, just env)


What was wrong?
- folding over the list of objects was the bad thing
- does not seem exponential with a singleton list


A microbenchmark:
1. object type C with a (-> Obj Obj) method
   where Obj is "complicated enough" i.e. a mixin
2. from untyped, repeatedly call the C mixin
