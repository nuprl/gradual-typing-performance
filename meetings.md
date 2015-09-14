Meeting notes
=============

2015-08-28 (all NU authors present)
-----------------------------------
  * Discussed new contract profiling results on worst case variations
  * Got interesting numbers on % runtime in contracts, % contract runtime
    in predicate contracts, library contracts, & HOF contracts

####  Unresolved questions on numbers
  * [X] MBTA numbers are zero, need to run longer
  * [ ] What is the % of "misc" other contracts made up of
  * [ ] What is the profiler measuring in the predicate cases?
        (probably the (-> any/c boolean?) contract and not the
         actual predicate--surprising that that is slow)

####  Goals for next meeting
  * [ ] "Verify" contract overhead numbers by removing select contracts and
        checking runtime.
  * [ ] See contract profiler accuracy.
      ~~Also see if Racket HEAD improves it.~~ (bg: Things are ok on `v6.2.900.11`)
  * [ ] What's going on with profiling for, e.g., (listof ...) contracts?
  * [ ] Footnotes in paper about limitations/problems that ruled out some
        potential projects

2015-09-08 (all NU authors present)
-----------------------------------
  * Discussed contract profiling accuracy, concluded that we should try
    profiling with HEAD even if we're using 6.2 for the paper. If it's
    better, we can also try backporting just the profiling improvements to 6.2.
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
