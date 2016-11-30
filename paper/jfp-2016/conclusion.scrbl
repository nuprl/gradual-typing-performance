#lang scribble/base

@require["common.rkt" "benchmark.rkt" "typed-racket.rkt" "util.rkt"]

@profile-point{sec:conclusion}
@title[#:tag "sec:fut"]{The Future of Gradual Typing}

@; - our goal
Gradual typing emerged as a new research area ten years ago.
Researchers began by asking whether one could design a programming language with a sound type system that integrated untyped components@~cite[thf-dls-2006 st-sfp-2006].
The initial series of papers on gradual typing provided theoretical models that served as proofs of concept.
Eight years ago, @citet[thf-popl-2008] introduced Typed Racket, the first implementation of a gradual type system designed to accomodate existing, dynamically-typed programs.
At this point, the important research question changed to whether the models of gradual typing could scale to express the @emph{grown} idioms found in dynamic languages.
Others have since explored this and similar questions in languages ranging from Smalltalk to JavaScript@~cite[acftd-scp-2013 rsfbv-popl-2015 vksb-dls-2014 rnv-ecoop-2015].

From the beginning, researchers speculated that the cost of enforcing type soundness at runtime would be high.
@citet[thf-dls-2006] anticipated this cost and attempted to reduce it by permitting only module-level type boundaries.
@citet[htf-hosc-2010] and @citet[sw-popl-2010] developed calculi to remove the space-inefficiency apparent in models of gradual typing.
Industry labs went so far as to build @emph{optionally typed} languages@;
@note{@url{http://www.typescriptlang.org/}}
@note{@url{http://hacklang.org/}}
@note{@url{http://flowtype.org/}}
@note{@url{http://mypy-lang.org/}}
that provide static checks but sacrifice type soundness.
Programs written in such languages run with zero overhead, but are suceptible to the hard-to-trace bugs and silent failures explained in @secref{sec:overhead}.

As implementations of gradual typing matured, programmers using them had mixed experiences about the performance overhead of gradual typing.
Some programmers did not notice any significant overhead.
    @; note: able to avoid by typing more modules?
Others experienced orders-of-magnitude slowdowns.
The burning question thus became @emph{how to systematically measure} the performance overhead of a gradual type system.
This paper provides an answer:
 @itemlist[#:style 'ordered
   @item{
     To @emph{measure} the performance of a gradual type system, fully annotate a suite of representative benchmark programs and measure the running time of all typed/untyped configurations according to a fixed @emph{granularity}.
     In Typed Racket, the granularity is by-module.
     In a micro-level gradual type system such as Reticulated Python, experimenters have more choices (e.g., by module or by variable).
   }
   @item{
     To express the @emph{absolute performance} of the gradual type system, report the proportion of configurations in each benchmark that are @step{} using @emph{overhead graphs}.
     Ideally, many configurations should be @step["0" "1.1"].
   }
   @item{
     To express the @emph{relative performance} of two implementations of a gradual type system, plot two overhead graphs on the same axis and test whether the distance is statistically significant.
     Ideally, the curve for the improved system should demonstrate a complete and significant ``left shift.''
   }
 ]
Applying the evaluation method to Typed Racket has confirmed that the method works well to uncover performance issues and to quantify improvements between distinct implementations of the same gradual type system.
We therefore recommend the method to maintainers of other gradual type systems and to any researcher investigating the performance of a multi-language system@~cite[gff-oopsla-2005 bbdt-ecoop-2016].

The results of the evaluation in @secref{sec:tr} suggest three vectors of future research for gradual typing.
First, the maintainers of other gradual type systems must attempt similar performance evaluations.
Second, the sampling technique of @secref{sec:scale} must be validated on larger programs and in the context of micro-level gradual typing.
Third, the research community must explore tools to help developers navigate a performance lattice, such as the feature-specific profiler of @citet[saf-cc-2015].

Typed Racket in particular must address the pathologies identified in @secref{sec:devils}.
Here are a few suggestions.
To reduce the cost of high-frequency checks, the runtime system could cache the results of successful checks@~cite[rf-pldi-2016] or implement a tracing JIT compiler tailored to identify dynamic type assertions@~cite[bauman-et-al-icfp-2015].
High-cost types may be a symptom of inefficiencies in the translation from types to dynamic checks.
Recent calculi for space-efficient contracts@~cite[stw-pldi-2015 g-popl-2015] may provide insight for eliminating redundant dynamic checks.

Finally, researchers must ask whether the specific problems reported in this paper indicate a fundamental limitation of gradual typing.
The only way to know is through further systematic evaluation of gradual type systems.


@;@section[#:style 'unnumbered]{Data and Code}
@;
@;Our benchmarks and measurements are available in our online supplement.


@section[#:style 'unnumbered]{Acknowledgments}

The authors gratefully acknowledge support from the National Science
Foundation (SHF 1518844). They also thank Matthew Butterick, John Clements,
Matt Might, Vincent St-Amour, Neil Toronto, David Van Horn, Danny Yoo, and Jon
Zeppieri for providing benchmark code bases. Brian LaChance and Sam
Tobin-Hochstadt provided feedback on earlier drafts.

@; - Stephen Chang, Ryan Culpepper for helping Max with the predictions impl.
@; - Milo + Zeina for comments

