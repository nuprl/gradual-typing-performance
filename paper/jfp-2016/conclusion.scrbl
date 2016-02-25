#lang scribble/base

@require["common.rkt" "typed-racket.rkt"]

@title[#:tag "sec:fut"]{Long Live Sound Gradual Typing}

Sound gradual typing was proposed to solve the practical issue of
 safely combining dynamic and static typing.
In fact, fundamental research on gradual typing might serve the
 broader purpose of aiding safe program conversion as large codebases
 move to adopt new language technology.
We have found, however, that gradual typing must address serious performance
 issues to achieve these goals.
At present, the difference between ideal and actual performance seems
 insurmountable and we doubt whether the unconstrained freedom to mix
 typed and untyped code can be practically achieved.

We support our thesis with benchmarking results for @emph{all
 possible} gradual typing configurations for @id[NUM-BENCHMARKS] Typed Racket
 benchmarks of various sizes and complexities.
Even under liberal conditions, few of these configurations have only
 deliverable or usable performance overhead.
Relaxing our judgment to allow additional conversions of untyped code
 yields little improvement. @todo{quantify}

Our result calls for three orthogonal research efforts.
First, Typed Racket is only one implementation of sound gradual typing,
 and it supports only macro-level gradual typing.
Before we declare gradual typing completely dead, we must apply our method to
 other implementations.
The question is whether doing so will yield equally negative results.
Safe TypeScript@~cite[rsfbv-popl-2015] appears to be one natural candidate for
 such an effort.
At the same time, we are also challenged to explore how our evaluation method
 can be adapted to the world of micro-level gradual typing, where programmers
 can equip even the smallest expression with a type annotation and leave the
 surrounding context untouched.
We conjecture that annotating complete functions or methods
 is an appropriate starting point for such an adaptation experiment.

Second, Typed Racket's implementation can be improved on two levels.
For one, the high-level translation from types to contracts can
 generate more efficient checks.
We have already removed the cost of user-defined-type predicates but there
 are more bugs to stamp out.
There are other language design issues, such as encouraging abstract types
 or using type inference to shrink the total size of a type boundary.
The other direction is to explore a typed runtime system.
Typed Racket elaborates into plain Racket, type-checks the result,
 inserts contracts between typed and untyped modules, and then uses Racket
 to compile the result@~cite[thscff-pldi-2011].
The latter implements a JIT compiler that open-codes primitive functions.
One implication is that code from contracts does not get eliminated
 even if it is re-evaluated for the same value in a plain loop.
A sophisticated JIT compiler may eliminate some of the contract overhead
 in such cases, but we conjecture that performance pathologies will still remain.
Applying our method to an implementation with a more sophisticated compiler,
 e.g., Pycket@~cite[bauman-et-al-icfp-2015], may let us validate this conjecture.

Third, the acceptance of Typed Racket in the commercial and open-source
 Racket community suggests that (some) programmers find a way around the
 performance bottlenecks of sound gradual typing.
Expanding this community will take the development of both guidelines on how
 to go about annotating a large system and performance measurement tools that
 help programmers discover how to identify those components of a gradually-typed
 configuration that yield the most benefit (per time investment).
St-Amour's feature-specific profiler@~cite[saf-cc-2015] and optimization
 coaches@~cite[stf-optimization-coaching] look promising; we
 used both kinds of tools to find the reason for some of the most curious
 performance bottlenecks in our measurements.

In sum, while we accept that the current implementation technology for
 gradually-typed programming languages falls short of its promises, we also
 conjecture that the use of our method will yield useful performance
 evaluations to guide future research.
Above we have spelled out practical
 directions but even theoretical ideas---such as Henglein's optimal
 coercion insertion@~cite[hr-fpca-1995] and the collapsing of chains of
 contracts@~cite[sw-popl-2010]---may take inspiration from the application
 of our method.

@section[#:style 'unnumbered]{Data and Code}

Our benchmarks and measurements are available in
our artifact:
@todo{url}

@section[#:style 'unnumbered]{Acknowledgments}

The authors gratefully acknowledge support from the National Science
Foundation (SHF 1518844). They also thank Matthew Butterick, John Clements,
Matthew Might, Vincent St-Amour, Neil Toronto, David Van Horn, Danny Yoo, and Jon
Zeppieri for providing benchmark code bases. Brian LaChance and Sam
Tobin-Hochstadt provided valuable feedback on earlier drafts.

