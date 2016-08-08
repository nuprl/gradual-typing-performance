#lang scribble/base

@require["common.rkt" "benchmark.rkt" "typed-racket.rkt" "util.rkt"]

@profile-point{sec:conclusion}
@title[#:tag "sec:fut"]{Long Live Sound Gradual Typing}

@; - our goal
Our goal is to foster expressive, safe, and performant implementations of gradual typing.
Comprehensive performance evaluation has the potential to advance this goal
 by determining the magnitude of performance overhead in realistic programs,
 identifying bottlenecks in the implementation of gradually typed languages,
 quantifying the performance effect of changes to a language,
 and
 encouraging new implementation strategies to overcome performance barriers.
Because sound gradual typing promises safe execution for @emph{any}
 combination of static and dynamic typing, performance evaluation must consider
 all possible choices open to programmers.
@; - foundations of sound GT
This diversity is a challenge for implementors, but also the source
 of gradual typing's practical appeal and utility.

@; - our method
The method we advocate is to
 (0) fix an "atomic" unit for type annotations,
 (1) fully type-annotate a representative suite of benchmarks,
 (2) incrementally remove annotations, measure the performance of each configuration thus obtained
 (3) report the performance overhead of these configurations relative to the fully-untyped baseline.
To present this data in a concise and relevant manner, we continuously vary the
 @emph{overhead} users may tolerate against
 the number of configurations that run within the chosen overhead.

@; - TR
In the context of Typed Racket, where every module in a program is either
 typed or untyped, we find that gradual typing introduces significant performance
 overhead.
Users must tolerate more than 20x performance overhead before all the static/dynamic
 variations promised by gradual typing qualify as "performant".
For programmers with more stringent performance requirements, the challenge
 is now to navigate the lattice of possibilities to find e.g. one of the
 @deliverable{3} @bm[acquire] configurations and avoid the 256 other configurations.
With larger programs, the space of possibilities will be even more difficult to manage.
If gradual typing is to succeed, implementors must resolve this performance issue.

Our result calls for three orthogonal research efforts.
First, Typed Racket is only one implementation of sound gradual typing,
 and it supports only coarse-grained interoperability.
Applying our framework to other languages like Safe TypeScript@~cite[rsfbv-popl-2015]
 and Reticulated Python@~cite[vksb-dls-2014] may yield different results.
At the same time, we are also challenged to scale our evaluation method
 to so called @emph{micro-level} gradual typing, where programmers can equip any variable
 with a type annotation and leave the surrounding context untouched.
We conjecture that annotating complete functions or methods
 is an appropriate starting point for such an adaptation experiment.

Second, Typed Racket's implementation can be improved on two levels.
For one, the conversion from types to contracts could be tuned to
 generate more efficient checks.
There are other language design issues, such as encouraging abstract types
 or using type inference to shrink the total size of a type boundary.
The other level is to explore a typed runtime system or alternative JIT technology.
Typed Racket currently elaborates into plain Racket, type-checks the result,
 inserts contracts between typed and untyped modules, and then relies on
 the Racket compiler to convert the result to bytecode @~cite[thscff-pldi-2011].
The latter implements a JIT compiler that open-codes primitive functions.
One implication is that code from contracts does not get eliminated
 even if it is re-evaluated for the same value in a plain loop.
A sophisticated@~cite[bauman-et-al-icfp-2015] or type-aware JIT compiler may eliminate some of the
 contract overhead in such cases.

Third, the acceptance of Typed Racket in the commercial and open-source
 Racket community suggests that (some) programmers find a way around the
 performance bottlenecks of sound gradual typing.
Expanding this community will take the development of both guidelines on how
 to go about annotating a large system and performance measurement tools that
 help programmers discover how to identify those components of a gradually-typed
 configuration that yield the most benefit (per time investment).
St-Amour's feature-specific profiler@~cite[saf-cc-2015] and optimization
 coaches@~cite[stf-optimization-coaching] look promising; we
 used both kinds of tools to diagnose the pathologies in our benchmarks.

In closing, we emphasize that gradual typing is @emph{not} about converting
 untyped programs to typed programs.
@emph{Fully typed is not the goal}.
Both static and dynamic typing have their advantages; ultimately,
 the stable point for a large software project will lie between
 the two extremes.
The true goal is that gradual typing helps programmers adapt to changing
 requirements and statically reason about their programs.
Safe interoperability is the challenge, and sound gradual typing is only
 a first step in this direction.


@section[#:style 'unnumbered]{Data and Code}

Our benchmarks and measurements are available in
our artifact:
@todo{url}


@section[#:style 'unnumbered]{Acknowledgments}

The authors gratefully acknowledge support from the National Science
Foundation (SHF 1518844). They also thank Matthew Butterick, John Clements,
Matthew Might, Vincent St-Amour, Neil Toronto, David Van Horn, Danny Yoo, and Jon
Zeppieri for providing benchmark code bases. Brian LaChance and Sam
Tobin-Hochstadt provided feedback on earlier drafts.

@; - Stephen Chang, Ryan Culpepper for helping Max with the predictions impl.
@; - Milo + Zeina for comments

