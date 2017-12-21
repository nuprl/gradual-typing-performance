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

Although the evaluation method addresses the goals of gradual typing, its application in @secref{sec:tr} manifests some threats to validity.
In particular, both the @emph{experimental protocol} and the @emph{conclusions} have limitations.

@; -- small bm, some aggressively modularized
There are four significant threats to the protocol.
First, the benchmark programs are relatively small.
Larger programs might avoid the pathological overheads in the benchmarks,
 though the results for @bm[quadMB] and @bm[synth] are evidence to the contrary.

Second, a few benchmarks have little data (less than 6 samples per configuration, details in the appendix) due to time limitations.@note{Parallelizing the experiment would yield more samples, but would also add confounding variables to the measurements. See @url{http://prl.ccs.neu.edu/blog/2016/08/03/a-few-cores-too-many/} for one relevant anecdote.}
It is therefore possible that some samples are not truly representative.

@; TODO say this better? Remove?
Third, the configurations running in parallel reference the same Racket executable and external libraries.
This cross-reference is a potential source of bias.

Fourth, the Racket just-in-time compiler includes heuristic optimizations.
The protocol of compiling @emph{once} before collecting one sample does not control for these heuristics.
Nevertheless, the overheads evident in the results are much larger than those attributed to systematic biases in the literature@~cite[mdhs-asplos-2009 gvg-siu-2005 cb-asplos-2013].

@; ===

The conclusions have three limitations.
First, the evaluation does not systematically measure the effects of annotating the same code with different types.
This is an issue because type annotations determine the runtime constraints on untyped code.
Therefore if two programmers give the same code different type annotations, they may experience different performance.
For example, @bm[quadBG] and @bm[quadMB] describe the same code with different types and have very different performance characteristics.
Whereas all configurations of the former are @deliverable{6}, only a small fraction of @bm[quadMB] configurations are @deliverable{20}.

Second, the conclusions rely on Typed Racket's implementation technology and do not necessarily generalize to other implementations of gradual typing.
Typed Racket re-uses Racket's runtime, a conventional JIT technology.
In particular, the JIT makes no attempt to reduce the overhead of contracts.
Contract-aware implementation techniques such soft contracts (@exact{@|PHIL|} @|etal| 2014) @; HACK @elem{@~cite[nthvh-icfp-2014]}
 or the Pycket tracing JIT compiler@~cite[bauman-et-al-icfp-2015]
 may significantly reduce the overhead of gradual typing.

Finally, when the Racket contract system discovers a type boundary error, its type soundness theorem guarantees that programmers receive the exact type boundary, type annotation, and incompatible value in the error message.
This blame assignment has practical value for developers@~cite[dthf-esop-2012], but the runtime system must dynamically track contextual information to implement it.
On one hand, there may be inefficiencies in Racket's implementation of this runtime monitoring.
On the other hand, a different gradual type system could offer a different soundness guarantee and circumvent the need for this runtime accounting altogether.
For example, Reticulated Python's transient semantics checks the type of a mutable data structure when typed code reads from the structure, but not when untyped code writes to it, avoiding the need to proxy such data structures@~cite[vksb-dls-2014].
StrongScript provides only nominal subtyping for objects, largely because structural subtyping incurs a higher runtime cost@~cite[rnv-ecoop-2015].
    @; SS supports both @emph{optional} types and @emph{concrete} types, only the latter require dynamic enforcement.
The question is whether these alternatives are sufficiently practical.
