Removing Other Contracts
========================

1. Removed `(-> (vectorof Integer) Integer (box/c (or/c #f #t)) (-> Void) (-> (vectorof Integer) Float) any)` from synth.
   NO DIFFERENCE.
   Not sure what happened.


2. Removed `(-> (hash/c Binding? (set/c Closure?)) (hash/c Binding? (set/c Closure?)) (hash/c Binding? (set/c Closure?)))` from kcfa.
   Got the attached kcfa.txt.
   Looks good.
