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
