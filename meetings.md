Meeting notes
=============

2015-08-28 (all NU authors present)
-----------------------------------
  * Discussed new contract profiling results on worst case variations
  * Got interesting numbers on % runtime in contracts, % contract runtime
    in predicate contracts, library contracts, & HOF contracts

####  Unresolved questions on numbers
  * [X] MBTA numbers are zero, need to run longer
  * [X] What is the % of "misc" other contracts made up of (see `./postmortem/profile`)
  * [X] What is the profiler measuring in the predicate cases?
        (probably the (-> any/c boolean?) contract and not the
         actual predicate--surprising that that is slow)
        (yes, the (-> any/c boolean?), see the file `./postmortem/experiments/what-does-contract-profile-measure/typed.rkt`
         and the raw data in `./postmortem/profile/HEAD-any-bool/`)

####  Goals for next meeting
  * [X] "Verify" contract overhead numbers by removing select contracts and
        checking runtime.
        (see `./postmortem/experiments/what-does-contract-profile-measure/other-contracts`
         and the "full results" in `./postmortem/experiments/what-does-contract-profile-measure/README.md`)
  * [X] See contract profiler accuracy.
      ~~Also see if Racket HEAD improves it.~~ (bg: Things are ok on `v6.2.900.11`)
  * [ ] What's going on with profiling for, e.g., (listof ...) contracts?
        (unmarked, need to try adding the marks suggested [here](http://github.com/nuprl/gradual-typing-performance/pull/96).
  * [ ] Footnotes in paper about limitations/problems that ruled out some
        potential projects

2015-09-08 (all NU authors present)
-----------------------------------
  * [X] Discussed contract profiling accuracy, concluded that we should try
    profiling with HEAD even if we're using 6.2 for the paper. If it's
    better, we can also try backporting just the profiling improvements to 6.2.
    (see `./postmortem/profile/6.2.900.15`, and `./postmortem/profile/6.2/` for older results
     we decided on `09-11` to use 6.2 because we cannot run the contract profiler
     on the zo analyzer)
  * Will meet in 2 days when we have more results.

2015-09-11
----------
  * Discussed contract profiler accuracy, results suggest that the
    profiling results are accurate since removing the contracts does speed
    up the program.
  * Decided to look into a table for the results for Monday.

2015-09-14
----------
  * Looked at and discussed a table for the profiling results. The goal for
    Friday is to begin some prose and discuss the reviews.

2015-12-04
----------
  * Diagnosed issues with forth and fsmoo. Complex object types were being double-wrapped.
    (In fsmoo, a method changed object state and returned 'this'. Returning 'void' would not add wraps.)
  * samth visited and he ...
    - Asked for more statistics (how many chaperones total? how many of each type? how many checks of each type? how many wraps?)
    - Asked for a deeper explanation of suffixtree. Why does it use so much memory? What is being wrapped?
    - Suggested we port Zombie because "it was slow for a different reason". It simulated objects with higher-order functions.

2015-12-11
----------
  * Agreed to emphasize non-unique types in the journal paper, using quad as a leading example
  * Discussed performance of Acquire and Zombie
    - Acquire is very good, thanks to encapsulation
    - Zombie is terrible, thanks to higher-order types like (-> Symbol (-> (U (List 'fst (-> Real)) (List 'snd (-> Real)))))
  * Noticed issue that pinning 30 iterations to one core bottlenecks our benchmarking.
  * Full run of v6.3 in progress since 2015-12-07


2015-12-14
----------
  * Discuss contract statistics. How many double-wraps? How many times was each type checked?
  * Acquire:
    - try removing the preconditions & re-measuring (no difference)
    - try adding a boundary between player.rkt and player-factory.rkt (no difference)
  * Zombie: is the program accumulating wrappers? Or is the cost just repeated checks?
    (accumulating wrappers, need to improve Typed Racket's case-> to dispatch both on
     arities AND domain types that happen to be symbols)

2016-01-06
----------
  * v6.3 still running (since 12-17)
  * will start 6.4 (to have partial numbers for 01-17)
  * TR slowly improving
    - late-negs help stronger?
    - learned that macros can change boundaries

2016-01-29
----------
  * Review data: L-N/M, paths, memory profiling
    - Histogram = meh
    - L-N/M looks good
    - paths are not important
    - need to carefully structure the prose
  * Outline JFP paper
    - outline torn apart, have advice for a new one
  * Feb. meeting format will continue as usual, JFP due Feb 28
  * For next time, Asumu & Ben & Max each have 1 paper to read
    - While reading, look for ways to scale our technique to "large macro" or "micro" systems
