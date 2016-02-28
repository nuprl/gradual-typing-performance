#lang scribble/base

@require["common.rkt" "typed-racket.rkt"]

@title[#:tag "sec:fut"]{Long Live Sound Gradual Typing}

Sound gradual typing was proposed to solve the practical issue of
 safely combining dynamic and static typing.
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
Applying our framework to other languages like Safe TypeScript@~cite[rsfbv-popl-2015]
 and Reticulated Python @todo{cite} may yield different results.
At the same time, we are also challenged to scale our evaluation method
 to micro-level gradual typing, where programmers can equip any variable
 with a type annotation and leave the surrounding context untouched.
We conjecture that annotating complete functions or methods
 is an appropriate starting point for such an adaptation experiment.

Second, Typed Racket's implementation can be improved on two levels.
For one, the conversion from types to contracts could be tuned to
 generate more efficient checks.
There are other language design issues, such as encouraging abstract types
 or using type inference to shrink the total size of a type boundary.
The other level is to explore a typed runtime system.
Typed Racket currently elaborates into plain Racket, type-checks the result,
 inserts contracts between typed and untyped modules, and then relies on
 the Racket compiler to convert the result to bytecode @~cite[thscff-pldi-2011].
The latter implements a JIT compiler that open-codes primitive functions.
One implication is that code from contracts does not get eliminated
 even if it is re-evaluated for the same value in a plain loop.
A sophisticated or type-aware JIT compiler may eliminate some of the
 contract overhead in such cases.
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
 used both kinds of tools to diagnose some of the most curious
 performance bottlenecks in our measurements.

@section{Beyond Typed Racket}

Gradual typing, in our mind, is not just about mixing typed and untyped code.
The deeper question is about safe language interoperability, especially between
 languages that offer different correctness guarantees.
Typed Racket is just one instance, where the type system
 is at odds with the freedom of the untyped programming language.
A similar problem concerns typed languages' interaction with their untyped
 runtime system, or any communication through an FFI.

How can we ensure safety when data flows across a boundary?
To date, the solution has been ``by assumption'': we assert that the boundary is correct.
For a runtime system, this approach is justifiable.
Checking every interaction between the runtime and the language would
 incur a large overhead, and besides the runtime is small enough to
 consider it a trusted computing base.
Assuming safety is less valid, however, when reasoning about FFI calls.
It is usually not reasonable to trust that every foreign function is correct.
And in the case of gradual typing, the untyped code is part of the very system
 we hope to debug and maintain by adding type safety.

Thus we think of gradual typing as one motivating example for research on
 language interoperability.
The question is how we can enforce and streamline the conditions necessary
 for correctness.
Once we do find successful techniques, we expect to apply them in other
 multi-language systems; in particular, between specialized and general-purpose
 type systems.

As a concrete example, the verification community is currently trying to
 bring proof assistants into common use.
This is happening gradually in the manner we outlined above.
High-profile software like C compilers,
 device drivers, OS kernels, and even a web browser shim have been implemented
 in Coq because they stand to benefit the most from formal verification.
Each conversion requires tremendous effort, so despite these successes we
 may never reach the point where Coq is the @emph{lingua franca} for verified
 software.
Therefore we need safe interoperability between verified software and the existing
 code that lives around it.
Until now, interactions have been dealt with case-by-case, but we hope for
 a unified theory and implementation.
Research on typed/untyped interaction should provide key insights for this problem.


In sum, while we accept that the current implementation technology for
 gradually-typed programming languages falls short of its promises, we also
 conjecture that the use of our method will yield useful performance
 evaluations to guide future research.
Above we have spelled out practical
 directions but even theoretical ideas---such as Henglein's optimal
 coercion insertion@~cite[hr-fpca-1995] and the collapsing of chains of
 contracts@~cite[sw-popl-2010]---may take inspiration from the application
 of our method.
@; Yo, what to say here?
@; what DID we learn?

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

