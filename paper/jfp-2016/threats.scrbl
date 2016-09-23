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
We have identified four threats to our protocol.
First, the benchmark programs are relatively small.
Larger programs might avoid the pathological overheads in our benchmarks,
 though our results for @bm[quadMB] and @bm[synth] are evidence to the contrary.

Second, a few benchmarks have little data (less than 6 samples per configuration) due to time limitations.
It is therefore possible that some samples are not truly representative.
Running more configurations in parallel could have yielded more samples, but would have introduced additional confounding variables in the measurements.

Third, our configurations running in parallel reference the same Racket executable and external libraries.
This cross-reference is a potential source of bias.
We did not detect any anomalies when running two configurations simultaneously, but the shared executable was an issue when we tried running experiments on a computing cluster.

Fourth, the Racket compiler and JIT include heuristic optimizations.
Our protocol of compiling @emph{once} before collecting one sample does not control for these heuristics.
Nevertheless, we report overheads much larger than those attributed to systematic biases in the literature@~cite[mdhs-asplos-2009 gvg-siu-2005 cb-asplos-2013].

@; ===

Our conclusions suffer from three limitations.
First, we use one fixed type assignment for each benchmark.
Typed Racket accepts a range of type annotations for any expression.
These type annotations determine the runtime constraints on untyped code, therefore the choice of types can drastically affect performance.
Except for the @bm[quadBG] and @bm[quadMB] benchmarks, which differ only in type annotations, we have no other paired benchmark of this kind.
Then again, it is still a failure of gradual typing if a programmer must divine the best possible choice of type annotations to obtain reasonable performance.

Second, our conclusions rely on Typed Racket's implementation technology.
Typed Racket re-uses Racket's runtime, a conventional JIT technology.
In particular, the JIT makes no attempt to reduce the overhead of contracts.
Contract-aware implementation techniques such @emph{soft contracts} (@exact{@|PHIL|} @|etal| 2014) @; TODO HACK @elem{@~cite[nthvh-icfp-2014]}
 or the @emph{Pycket} tracing JIT compiler@~cite[bauman-et-al-icfp-2015]
 may significantly reduce the overhead of gradual typing.

Finally, when the Racket contract system discovers a type boundary error, type soundness guarantees that programmers receive the exact type boundary, type annotation, and incompatible value in the error message.
This blame assignment has practical value for developers@~cite[dthf-esop-2012], but the runtime system must dynamically track contextual information to implement it.
On one hand, there may be inefficiencies in Racket's implementation of this runtime monitoring.
On the other hand, a different gradual type system could offer a different soundness guarantee and circumvent the need for this runtime accounting altogether.
For example, Reticulated Python's transient semantics checks the type of a mutable data structure when typed code reads from the structure, but not when untyped code writes to it, avoiding the need to proxy such data structures@~cite[vksb-dls-2014].
StrongScript provides only nominal subtyping for objects, largely because structural subtyping incurs a higher runtime cost@~cite[rnv-ecoop-2015].
    @; SS supports both @emph{optional} types and @emph{concrete} types, only the latter require dynamic enforcement.
The question is whether these alternatives are sufficiently practical.

