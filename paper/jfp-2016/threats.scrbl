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
Only a small proportion of configurations in our benchmark suite run with low overhead (@|lo-prop| of all configurations are @deliverable[lo] on v@|version|), and many demonstrate over @|hi|x overhead (@|hi-prop| on v@|version|).
While we are confident that the method addresses the goals of gradual typing, our particular application has some threats to validity.
In particular, both our @emph{experimental protocol} and the @emph{conclusions} we draw have limitations.

@; -- small bm, some aggressively modularized
The first @emph{experimental} threat is that the benchmark programs are relatively small.
Gradual typing offers the most promise for maintainers of large projects,@note{For example, the Typed Racket @library{plot} library has over 80 modules.} but benchmarking even a 20-module application with our method is prohibitively time-consuming.
@; just compiling the 16-module quad sequentially took over a week
@Secref{sec:scale} offers suggestions for scaling the method.

The second experimental threat is that we collected a low number of samples (less than 10) for a few benchmarks due to time limitations.
Instead of running a larger number of iterations in parallel,@note{In fact, we experimented with computing clusters but found the scheduler unreliable and the data prone to outliers.}
 we ran at most two configurations simultaneously on a reserved desktop machine.
This protocol yielded low-variance samples, but it is possible that some data is not truly representative.

The third experimental threat is that configurations running in parallel referenced the same Racket executable and external libraries.
This is a potential source of bias, though we did not detect any abnormalities when running two configurations simultaneously.

The final experimental threat is that we did not rigorously control for heuristics in the Racket compiler or JIT.
We compiled once before collecting one iteration for a configuration using Racket's default options.
Nevertheless, the overheads we recorded are generally larger than those attributed to systematic biases in the literature@~cite[mdhs-asplos-2009 cb-asplos-2013].

@; ===

The first limitation of our @emph{conclusions} is that we use one fixed type assignment for each benchmark.
Typed Racket accepts a range of type annotations for any untyped expression;
 because these types become dynamic assertions, the choice of types can drastically affect performance.
Except for the @bm[quadBG] and @bm[quadMB] benchmarks, which differ only in type annotations, we have not rigorously studied how the choice of types affects overall performance.
Then again, it is still a failure of gradual typing if a programmer must divine the best possible choice of type annotations to obtain reasonable performance.

Second, our conclusions are based on Typed Racket's implementation technology.
Typed Racket re-users Racket's runtime, a conventional JIT technology.
In particular, the JIT makes no attempt to reduce the overhead of contracts.
Contract-aware implementation techniques such @emph{soft contracts} (@exact{@|PHIL|} @|etal| 2014) @; TODO HACK @elem{@~cite[nthvh-icfp-2014]}
 and the Pycket tracing JIT compiler@~cite[bauman-et-al-icfp-2015]
 may significantly reduce the overhead of gradual typing.

Finally, Typed Racket's type soundness theorem requires that every type at a boundary is translated to a contract.
When the Racket contract system discoveres a type boundary error, it alerts the programmer with the exact type boundary, type annotation, and incompatible value.
This blame assignment@~cite[dthf-esop-2012] has practical value for developers, but the runtime system must dynamically track contextual information to implement it.
On one hand, there may be inefficiencies in Racket's implementation.
On the other hand, a different gradual type system could offer a weaker soundness guarantee and circumvent the need for this runtime accounting altogether.
For example, Reticulated Python's transient semantics checks the type of a mutable data structure when typed code accesses the structure, but not when untyped code writes to it@~cite[vksb-dls-2014].
The question is whether this alternative soundness guarantee is sufficiently practical.

