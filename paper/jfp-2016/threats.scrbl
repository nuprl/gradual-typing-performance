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

Although the evaluation method addresses the goals of gradual typing, its application in @secref{sec:tr} manifests a few threats to validity.
In particular, both the @emph{experimental protocol} and the @emph{conclusions} have limitations.

There are three significant threats to the protocol.
First, some benchmarks have minor interactions with the filesystem.
The following benchmarks read from a configuration file: @bm[forth], @bm[zordoz], @bm[lnm], @bm[suffixtree], @bm[snake], and @bm[tetris].
The following benchmarks write output to a file: @bm[sieve], @bm[quadBG], and @bm[quadMB].
Removing these I/O actions does not change the overhead presented in @secref{sec:plots}, thus we consider our results representative.

Second, the @bm[quadBG] and @bm[quadMB] configurations that ran in parallel referenced the same Racket executable and external libraries.
This cross-reference is a potential source of bias, but we have been unable to detect adverse effects.

Third, the protocol of compiling and running @emph{once} before collecting data is a simplistic way to control for the effects of JIT compilation.
Nevertheless, the overheads evident in the results are much larger than those attributed to systematic biases in the literature@~cite[mdhs-asplos-2009 gvg-siu-2005 cb-asplos-2013].

@; ===

The conclusions have five limitations.
First, the evaluation does not systematically measure the effects of annotating the same code with different types.
This is an issue because type annotations determine the runtime constraints on untyped code.
Therefore if two programmers give the same code different type annotations, they may experience different performance problems.
For example, @bm[quadBG] and @bm[quadMB] describe similar code with different types and have extremely different performance characteristics.
Whereas all configurations of the former are @deliverable{6}, only a small fraction of @bm[quadMB] configurations are @deliverable{20}.

Second, the benchmark programs are relatively small.
Larger programs might avoid the pathological overheads in the benchmarks,
 though the results for @bm[quadMB] and @bm[synth] constitute evidence to the contrary.

Third, the evaluation does not vary the inputs to the benchmark programs; the
 conclusions are therefore based on one trace through each program.
We consider these traces representative, but some users may observe different
 traces that stress a different set of type boundaries.

Fourth, the conclusions rely on Typed Racket's implementation technology and do not necessarily generalize to other implementations of gradual typing.
Typed Racket re-uses Racket's runtime, a conventional JIT technology.
In particular, the JIT makes no attempt to reduce the overhead of contracts.
Contract-aware implementation techniques such soft contracts @elem{@~cite[nthvh-icfp-2014]}
 or the Pycket tracing JIT compiler@~cite[bauman-et-al-icfp-2015 btsb-oopsla-2017]
 may significantly reduce the overhead of gradual typing.

Finally, Typed Racket relies on the @emph{complete monitoring} property@~cite[dthf-esop-2012] of the Racket contract system to provide a strong form of type soundness@~cite[tfffgksst-snapl-2017].
When a type boundary error occurs, Racket produces the original type annotation and the dynamically-typed value that violates the annotation.
This protocol generates extremely precise error messages, but the runtime system must dynamically track contextual information to implement it.
On one hand, there may be inefficiencies in Racket's implementation of this runtime monitoring.
On the other hand, a different gradual type system could offer a different soundness guarantee and circumvent the need for this runtime accounting altogether.
For example, Thorn@~cite[bfnorsvw-oopsla-2009] and StrongScript@~cite[rnv-ecoop-2015] let the programmer chose whether to enforce a type annotation at runtime.
Reticulated Python provides an alternative, type-tag notion of soundness@~cite[vksb-dls-2014 vss-popl-2017]
 that may impose less overhead@~cite[gm-pepm-2018], but gives programmers far less precise information than Typed Racket for debugging type boundary errors.
