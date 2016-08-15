#lang scribble/base

@require["common.rkt" "fig-contract-profile.rkt" "benchmark.rkt" "util.rkt"]

@title[#:tag "sec:threats"]{Threats to Validity of Conclusion}


The application of our evaluation method projects a negative image of
 sound gradual typing.
Our measurements of every possible way of gradually adding types to
 @id[(*NUM-BENCHMARKS*)] programs and show that only a small fraction of
 configurations run with practical overhead.
@todo{y-axis average for 2x?}
While we are confident that the method captures the spirit of the goals of
 gradual typing, our particular application must be put in perspective.

First, our benchmarks are relatively small.
Gradual typing offers the most promise for maintainers of large (100+ module)
 projects, but with our method benchmarking even 20 modules is prohibitively
 time-consuming.
Even on the mid-sized @bm[quad] benchmark, just compiling each configuration
 sequentially took a week on a standard desktop machine.
To run the benchmarks in a reasonable amount of time we used @todo{the cluster, what are the tradeoffs / confounders?}
@;Running benchmarks simultaneously on different cores of the same machine
@; may introduce confounding variables due to, e.g., shared caches or main memory.
@;We have attempted to control for this case and, as far as we can tell,
@; executing on an unloaded machine does not make a significant difference.

@; QUESTION: how to scale?

@;bg I don't understand this threat
Second, several of our benchmarks are not self-contained, but import
 code from an external library.
These external boundaries impose a cost on many configurations and in principle
 might substantially contribute to the running-time overhead of partially typed
 configurations.
Regardless, given the low typed/untyped ratios of benchmarks with external
 libraries (@todo{where are these numbers}) these libraries are unlikely to
 affect our conclusion that gradually typing modules within a program frequently
 introduces unacceptable overhead.

Third, the feasible set of type annotations for a program component is
 rarely unique in a gradually typed system.
Since types are translated into contracts in Typed Racket, the choice of
 type annotations affects performance; however, except for @tt{quadBG} and @tt{quadMB}
  which differ only in their type annotations, we benchmarked one fixed
  set of type annotations for each program.
Then again, it is still a failure of gradual typing if a programmer must
 divine the best possible type annotations to obtain reasonable
 performance.

Finally, we articulate our conclusions on the basis of current
 implementation technology.
Typed Racket compiles to Racket, which uses
 rather conventional JIT compilation technology.
It makes no attempt to reduce the overhead of contracts or to exploit contracts
 for optimizations.
It remains to be seen whether contract-aware compilers such as those being
 explored at the University of Maryland (@exact{@|PHIL|} @|etal| 2014) @; TODO HACK @elem{@~cite[nthvh-icfp-2014]}
 and Indiana University@~cite[bauman-et-al-icfp-2015]
 can reduce the significant overhead that our evaluation shows.
