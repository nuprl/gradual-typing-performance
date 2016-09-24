#lang scribble/base

@require["common.rkt" "benchmark.rkt" "typed-racket.rkt" "util.rkt"]

@profile-point{sec:conclusion}
@title[#:tag "sec:fut"]{The Future of Gradual Typing}

@; TR/Kings is a rough shift

@; - our goal
Our goal is to foster expressive, safe, and performant implementations of gradual typing.
Comprehensive performance evaluation has the potential to advance this goal by
 @; determining the magnitude of performance overhead in realistic programs,
 identifying performance bottlenecks,
 quantifying the effect of changes to a gradual type system,
 and
 motivating new implementation technology.
The present work is but the first step in this direction.

@; - our method
The evaluation method we advocate is to fix a granularity for type annotations and exhaustively measure the full spectrum of programs mixing typed and untyped components.
The overhead plots introduced in @secref{sec:graphs} provide a high-level summary of such data by showing what @emph{proportion} of the full spectrum is usable given a performance requirement.
Overhead plots can furthermore compare the number of @deliverable{} configurations to the number of @step{} configurations, and summarize the delta between two implementations of a gradual type system.

For benchmark programs that are too large to exhaustively measure, we recommend measuring the first and last levels of their performance lattice and randomly sampling other mixed programs.
As @secref{sec:scale} demonstrates, repeated samples of size @math{O(N)} can reasonably approximate the number of @deliverable{} configurations in a program with @math{N} modules.
Sampling can therefore quickly describe the performance overhead of gradual typing, and may discover pathological outliers in the meantime.

Although we developed this evaluation method specifically for gradual type systems, the same method can rigorously evaluate any multi-language system.
For example, the SoftDev group at King's College has a JIT compiler that integrates PHP and Python@~cite[bbdt-ecoop-2016].
It would be useful to see how performance overhead varies across all the different interleavings their system allows.

@; - TR
In the context of Typed Racket, where every module in a program is either typed or untyped, we find that gradual typing introduces significant performance overhead in our benchmark programs.
These findings call for three orthogonal research efforts.

First, researchers must apply the method to other state-of-the-art gradual type systems.
Typed Racket is only one implementation of sound gradual typing, and it supports only coarse-grained interleaving of typed and untyped components.
Applying our method to other languages such as Reticulated Python@~cite[vksb-dls-2014] and Safe TypeScript@~cite[rsfbv-popl-2015] may yield different results.
These systems implement so-called @emph{micro-level} gradual typing, meaning programmers can equip any class field or function parameter with a type annotation.
Perhaps this fine granularity will make it easier to avoid pathological type boundaries.

Second, Typed Racket must address the issues outlined in @secref{sec:devils}.
To reduce the cost of high-frequency checks, the runtime system could cache the results of successful dynamic checks@~cite[rf-pldi-2016] or implement a tracing JIT compiler tailored to identify dynamic type assertions@~cite[bauman-et-al-icfp-2015].
High-cost types may be a symptom of inefficiencies in the translation from types to dynamic checks.
Lastly, Racket's chaperones need an effective way to identify redundant contracts on a value.

Third, the research community must explore tools to help navigate a performance lattice.
Such tools would include editor support for managing type annotations, a cost model for the price of enforcing a static type dynamically, and a @emph{boundary profiler} that attributes runtime overhead to type boundaries.

In closing, we emphasize that gradual typing is @emph{not} about converting untyped programs to typed programs.
@emph{Fully typed is not the goal}.
Both static and dynamic typing have their advantages; ultimately, the stable point for a large software project will lie between the two extremes.
The true goal is that gradual typing helps programmers adapt to changing requirements and incrementally add static guarantees.

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

