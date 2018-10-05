#lang scribble/base

@; We therefore recommend the method to maintainers of other gradual type systems and to any researcher investigating the performance of a multi-language system@~cite[gff-oopsla-2005 bbdt-ecoop-2016].

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
Industry labs instead built languages with @emph{optional} type annotations@~cite[moon-maclisp-1974 s-cl-1990]
that provide static checks but sacrifice type soundness (see
 @hyperlink["http://www.typescriptlang.org/"]{TypeScript},
 @hyperlink["http://hacklang.org/"]{Hack},
 @hyperlink["http://flowtype.org/"]{Flow}, and
 @hyperlink["http://mypy-lang.org/"]{mypy}).
Programs written in such languages run with zero overhead, but are suceptible to silent failures@~cite[tfffgksst-snapl-2017].
At least three research languages attempted to provide a middle ground@~cite[rnv-ecoop-2015 vss-popl-2017 bfnorsvw-oopsla-2009].

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
     In a micro-level gradual type system such as Reticulated Python, experimenters may choose by-module, by-variable, or any granularity in between.
     If an exhaustive evaluation is not feasible, the sampling technique of @secref{sec:scale} provides an approximate measure.
   }
   @item{
     To express the @emph{absolute performance} of the gradual type system, report the proportion of configurations in each benchmark that are @step{} using @emph{overhead plots}.
     Ideally, many configurations should be @step["0" "1.1"].
   }
   @item{
     To express the @emph{relative performance} of two implementations of a gradual type system, plot two overhead plots on the same axis and test whether the distance is statistically significant.
     Ideally, the curve for the improved system should demonstrate a complete and significant ``left shift.''
   }
 ]
Applying the evaluation method to Typed Racket has confirmed that the method works well to uncover performance issues in a gradual type system and to quantify improvements between distinct implementations of the same gradual type system.

The results of the evaluation in @secref{sec:tr} suggest three vectors of future research for gradual typing in general.
The first vector is to evaluate other gradual type systems.
The second is to apply the sampling technique to large applications and to micro-level gradual typing.
The third is to build tools that help developers navigate a performance lattice, such as the feature-specific profiler of @citet[saf-cc-2015].
@citet[gm-pepm-2018] have made progress on the first and second vectors with a performance evaluation of Reticulated; consequently, @citet[gf-icfp-2018] have confirmed that porting Reticulated's notion of soundness@~cite[vss-popl-2017] to Typed Racket can improve its performance at the cost of detailed error messages.
These are encouraging first steps.

Typed Racket in particular must address the pathologies identified in @secref{sec:devils}.
Here are a few suggestions.
To reduce the cost of high-frequency checks, the runtime system could cache the results of successful checks@~cite[rf-pldi-2016] or implement a tracing JIT compiler tailored to identify dynamic type assertions@~cite[bauman-et-al-icfp-2015 btsb-oopsla-2017].
High-cost types may be a symptom of inefficiencies in the translation from types to dynamic checks.
Recent calculi for space-efficient contracts@~cite[htf-hosc-2010 stw-pldi-2015 g-popl-2015 g-tfp-2016 g-icfp-2013] may provide insight for eliminating proxies.
Storing runtime type information in the heap may prove to be more efficient than encoding it with contracts@~cite[rsfbv-popl-2015 vcgts-esop-2015].
Lastly, there is a long history of related work on improving the performance of dynamically typed languages@~cite[h-lfp-1992 jw-sas-1995 sw-sas-1995 c-esop-1988]; some of it may apply here.

Finally, researchers must ask whether the specific problems reported in this paper indicate a fundamental limitation of gradual typing.
The only way to know is through further systematic evaluation of gradual type systems.


@;@section[#:style 'unnumbered]{Data and Code}
@;
@;Our benchmarks and measurements are available in our online supplement.


@section[#:style 'unnumbered]{Acknowledgments}

The authors gratefully acknowledge support from the National Science Foundation (SHF 1518844) and thank the anonymous JFP reviewers.
They owe the implementors of Typed Racket a large debt, especially Sam Tobin-Hochstadt.
They also thank Matthew Butterick, John Clements, Matt Might, Linh Chi Nguyen,Vincent St-Amour, Neil Toronto, David Van Horn, Danny Yoo, and Jon Zeppieri for providing programs that inspired the benchmarks.
Sam Tobin-Hochstadt and Brian LaChance provided feedback on early drafts.

@; - Stephen Chang, Ryan Culpepper for helping Max with the predictions impl.

