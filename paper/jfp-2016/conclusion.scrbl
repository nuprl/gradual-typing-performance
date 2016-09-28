#lang scribble/base

@require["common.rkt" "benchmark.rkt" "typed-racket.rkt" "util.rkt"]

@profile-point{sec:conclusion}
@title[#:tag "sec:fut"]{The Future of Gradual Typing}

@; - our goal
Ten years ago, gradual typing was a new research area.
It was unclear whether one could design a type-@emph{sound} language that integrated untyped components, but the first papers on gradual typing served as proof-of-concept@~cite[mf-toplas-2007 gktff-sfp-2006 thf-dls-2006 st-sfp-2006].
Eight years ago, gradual typing existed only in models and it was unclear whether the research could scale to @emph{express} programs written in dynamically typed languages.
Recent implementations of gradual type systems have addressed these concerns@~cite[acftd-scp-2013 rsfbv-popl-2015 vksb-dls-2014 thf-popl-2008].

Meanwhile, the growing popularity of dynamically typed languages motivated industry labs to build @emph{optionally typed} variants of some languages.
Microsoft developed TypeScript@note{@url{https://www.typescriptlang.org/}} for JavaScript, Facebook developed Hack@note{@url{http://hacklang.org/}} for PHP and Flow@note{@url{https://flowtype.org/}} for JavaScript, and Dropbox is funding the @tt{mypy}@note{@url{http://mypy-lang.org/}} project for Python.
These languages provide the static benefits of type annotations and impose @emph{zero} runtime overhead, but do not guarantee type soundness.
Programs written in optionally typed languages are therefore suceptible to the errors and inefficiencies described in @secref{sec:overhead}.

The next question for gradual typing is whether gradual type systems can be made @emph{performant} enough to replace optional typing.
The evaluation method of @secref{sec:method} provides the first step towards this goal of @emph{sound}, @emph{expressive}, and @emph{performant} gradual typing.
In particular, the method rigorously quantifies the performance of a gradual type system.
@;  @Secref{sec:tr} and @secref{sec:scale} demonstrate how to apply the method.
 @itemlist[#:style 'ordered
   @item{
     To @emph{measure} the performance of a gradual type system, first fix a granularity for adding or removing type annotations, then annotate a suite of representative benchmark programs, and finally measure the running time of all possible typed/untyped @emph{configurations} (subject to the granularity).@note{
       Although we developed this evaluation method specifically for gradual type systems, the same method can rigorously evaluate any multi-language system.
       For example, the SoftDev group at King's College has a JIT compiler that integrates PHP and Python@~cite[bbdt-ecoop-2016].
     }
   }
   @item{
     To express the @emph{absolute performance} of the gradual type system, report the proportion of configurations in each benchmark that are @step{} using @emph{overhead graphs}.
   }
   @item{
     To express the @emph{relative performance} of two gradual type systems, plot their overhead graphs on the same axis and test whether the difference is statistically significant.
   }
   @item{
     For benchmarks that have @math{N} typeable components such that @exact{$2^N$} is too large to measure exhaustively, use @emph{simple random sampling} with an @math{O(N)} sample size to estimate the true proportion of @deliverable{} configurations.
   }
 ]
Applying this method to Typed Racket has revealed significant performance issues.
These findings call for three orthogonal research directions.

@; In the context of Typed Racket, where every module in a program is either typed or untyped, we find that gradual typing introduces significant performance overhead in our benchmark programs.
@; These findings call for three orthogonal research efforts.
First, the maintainers of other gradual type systems must evaluate their systems' performance.
The overhead apparent in Typed Racket may be commonplace, or an outlier.

Second, Typed Racket must address the pathologies identified in @secref{sec:devils}.
Here are a few suggestions.
To reduce the cost of high-frequency checks, the runtime system could cache the results of successful dynamic checks@~cite[rf-pldi-2016] or implement a tracing JIT compiler tailored to identify dynamic type assertions@~cite[bauman-et-al-icfp-2015].
High-cost types may be a symptom of inefficiencies in the translation from types to dynamic checks.
Lastly, Racket's chaperones need an effective way to identify and eliminate redundant contracts on a value@~cite[stw-pldi-2015 g-popl-2015].

Third, the research community must explore tools to help navigate a performance lattice.
Such tools would include editor support for managing type annotations, a static model of the price for enforcing a type annotation dynamically, and a @emph{boundary profiler} that attributes runtime overhead to particular type boundaries@~cite[saf-cc-2015].

Gradual typing is about solving a practical engineering problem.
The solutions it provides must be practical.
Maybe will succeed, maybe will fail.
Performance matters!
The end.

@; In closing, we emphasize that gradual typing is @emph{not} about converting untyped programs to typed programs.
@; @emph{Fully typed is not the goal}.
@; Both static and dynamic typing have their advantages; ultimately, the stable point for a large software project will lie between the two extremes.
@; The true goal is that gradual typing helps programmers adapt to changing requirements and incrementally add static guarantees.

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

