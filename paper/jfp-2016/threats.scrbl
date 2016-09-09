#lang scribble/base

@require[
  "common.rkt"
  "benchmark.rkt"
  "util.rkt"
  (only-in "typed-racket.rkt" deliverable*)
  (only-in racket/format ~r)
  (only-in racket/list last)
]

@title[#:tag "sec:threats"]{Threats to Validity}

@(define version
   (last (*RKT-VERSIONS*)))
@(define num-configs (*TOTAL-NUM-CONFIGURATIONS*))
@(define (format-% n)
   (format "~a%" (round (* 100 (/ n num-configs)))))
@(define lo (number->string (*LO*)))
@(define hi (number->string (*HI*)))
@(define lo-prop
   (format-% (deliverable* (*LO*) version ALL-BENCHMARKS)))
@(define hi-prop
   (format-% (- num-configs (deliverable* (*HI*) version ALL-BENCHMARKS))))

The application of our evaluation method projects a negative image of Typed Racket's sound gradual typing.
Only a small number of configurations in our benchmark suite run with low overhead; @|lo-prop| of all configurations are @deliverable[lo] on Racket v@|version|.
Many demonstrate extreme overhead; @|hi-prop| of all configurations are not even @deliverable[hi] on v@|version|.
While we are confident that the method addresses the goals of gradual typing, our particular application has some threats to validity.
In particular, both our @emph{experimental protocol} and the @emph{conclusions} we draw have limitations.

@; -- small bm, some aggressively modularized
We have identified four experimental threats.
First, the benchmark programs are relatively small.
Gradual typing offers the most promise for maintainers of large projects,@note{For example, the Typed Racket @library{plot} library has over 80 modules.} but benchmarking even a 20-module application with our method is prohibitively time-consuming.
@; just compiling the 16-module quad sequentially took over a week
@Secref{sec:scale} offers suggestions for scaling the method.

Second, we collected a low number of samples (less than 10) for a few benchmarks due to time limitations.
Instead of running a larger number of iterations in parallel,@note{In fact, we experimented with computing clusters but found the scheduler unreliable and the data prone to outliers.}
 we ran at most two configurations simultaneously on a reserved desktop machine.
This protocol yielded low-variance samples, but it is possible that some data is not truly representative.

Third, configurations running in parallel referenced the same Racket executable and external libraries.
This is a potential source of bias, though we did not detect any anomalies when running two configurations simultaneously.

Fourth, we did not rigorously control for heuristics in the Racket compiler or JIT.
We compiled once before collecting one iteration for a configuration using Racket's default options and repeated this simple compile/collect cycle.
Nevertheless, the overheads we recorded are generally much larger than those attributed to heuristic biases in the literature@~cite[mdhs-asplos-2009 gvg-siu-2005 cb-asplos-2013].

@; ===

Our conclusions suffer from three limitations.
First, we use one fixed type assignment for each benchmark.
Typed Racket accepts a range of type annotations for any expression.
These type annotations determine the runtime constraints on untyped code, therefore the choice of types can drastically affect performance.
Except for the @bm[quadBG] and @bm[quadMB] benchmarks, which differ only in type annotations, we have not rigorously studied how the choice of types affects overall performance.
Then again, it is still a failure of gradual typing if a programmer must divine the best possible choice of type annotations to obtain reasonable performance.

Second, our conclusions are based on Typed Racket's implementation technology.
Typed Racket re-uses Racket's runtime, a conventional JIT technology.
In particular, the JIT makes no attempt to reduce the overhead of contracts.
Contract-aware implementation techniques such @emph{soft contracts} (@exact{@|PHIL|} @|etal| 2014) @; TODO HACK @elem{@~cite[nthvh-icfp-2014]}
 or the @emph{Pycket} tracing JIT compiler@~cite[bauman-et-al-icfp-2015]
 may significantly reduce the overhead of gradual typing.

Finally, when the Racket contract system discoveres a type boundary error, type soundness guarantees that programmers receive the exact type boundary, type annotation, and incompatible value in the error message.
This blame assignment has practical value for developers@~cite[dthf-esop-2012], but the runtime system must dynamically track contextual information to implement it.
On one hand, there may be inefficiencies in Racket's implementation of this runtime monitoring.
On the other hand, a different gradual type system could offer a weaker soundness guarantee and circumvent the need for this runtime accounting altogether.
For example, Reticulated Python's transient semantics checks the type of a mutable data structure when typed code reads from the structure, but not when untyped code writes to it, avoiding the need to proxy such data structures@~cite[vksb-dls-2014].
The question is whether this alternative soundness guarantee is sufficiently practical.

