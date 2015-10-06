#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:fut"]{Long Live Sound Gradual Typing}

In the context of current implementation technology, sound gradual typing
 is dead. We support this thesis with benchmarking results for @emph{all
 possible gradual typing scenarios} for a dozen Racket/Typed Racket
 benchmarks of various sizes and complexities. Even under rather liberal
 considerations, few of these scenarios end up in deliverable or usable
 system configurations. Even allowing for additional conversions of untyped
 portions of the program does not yield much of an improvement. 

Our result calls for three orthogonal research efforts. First, Typed Racket
 is only one implementation of sound gradual typing, and it supports only
 macro-level gradual typing. Before we declare gradual typing completely
 dead, we must apply our framework to other implementations. The question
 is whether doing so will yield equally negative results.
 Safe TypeScript@~cite[rsfbv-popl-2015] appears to be one natural candidate for
 such an effort. At the same time, we are also challenged to explore how
 our evaluation framework can be adapted to the world of micro-level
 gradual typing, where programmers can equip even the smallest expression
 with a type annotation and leave the surrounding context untyped.  We
 conjecture that annotating complete functions or classes 
 is an appropriate starting point for such an adaptation experiment.

Second, Typed Racket's implementation may not support run-time checks as
 well as modern JIT compilers. Typed Racket elaborates into plain Racket,
 type-checks the result, inserts contracts between typed and untyped
 modules, and then uses Racket to compile the
 result@~cite[thscff-pldi-2011]. The latter implements a
 JIT compiler that open-codes primitive functions.
 One implication is that code from contracts does not get eliminated
 even if it is re-evaluated for the same value in a plain loop.
 A more sophisticated JIT compiler may eliminate some of the contract overhead
 in such cases, but we conjecture that performance pathologies will still remain.
 Applying our method to an implementation with a more sophisticated compiler,
 e.g., Pycket@~cite[fbpsth-dyla-2014], may let us validate this conjecture.

Third, the acceptance of Typed Racket in the commercial and open-source
 Racket community suggests that (some) programmers find a way around the
 performance bottlenecks of sound gradual typing. To expand this community
 will take the development of both guidelines on how to go about annotating
 a large system and performance measurement tools that help programmers
 discover how to identify those components of a gradually-typed
 configuration that yield the most benefit (per time
 investment). St-Amour's feature-specific profiler@~cite[saf-cc-2015] and
 optimization coaches@~cite[stf-optimization-coaching] look promising; we
 used both kinds of tools to find the reason for some of the most curious
 performance bottlenecks in our measurements.

In sum, while we accept that the current implementation technology for
 gradually-typed programming languages falls short of its promises, we also
 conjecture that a rigorous performance evaluation framework will provide
 guidance for future research. Above we have spelled out practical
 directions but even theoretical ideas---such as Henglein's optimal
 coercion insertion@~cite[hr-fpca-1995] and the collapsing of chains of
 contracts@~cite[sw-popl-2010]---may take inspiration from the application
 of our framework.

@;{section[#:style 'unnumbered]{Data and Code}

We will make our complete benchmark suite, all configurations, and the
measurements available jointly with our publication.}

@section[#:style 'unnumbered]{Acknowledgments}

We thank Matthew Butterick, John Clements, Matthew Might,
 Phúc C. Nguyễn, Vincent St-Amour, Danny Yoo,
 and Jon Zeppieri for providing benchmark code bases.

