#lang scribble/base

@require["common.rkt" "benchmark.rkt" "typed-racket.rkt" "util.rkt"]

@profile-point{sec:conclusion}
@title[#:tag "sec:fut"]{The Future of Gradual Typing}

@; - our goal
Gradual typing emerged as a new research area ten years ago.
Researchers began by asking whether one could design a programming language with a sound type system that integrated untyped components@~cite[mf-toplas-2007 gktff-sfp-2006 thf-dls-2006 st-sfp-2006].
The first papers on gradual typing provided theoretical models that served as proof of concept.
Eight years ago, @citet[thf-popl-2008] introduced Typed Racket, the first implementation of gradual typing designed to accomodate existing, dynamically-typed programs.
At this point, the important research question changed; researchers now asked whether the models of gradual typing could scale to express the eclectic idioms found in pragmatic, dynamically typed languages.
Recent gradual type systems have explored this question in languages ranging from Smalltalk to JavaScript@~cite[acftd-scp-2013 rsfbv-popl-2015 vksb-dls-2014 thf-popl-2008].

Researchers speculated that the runtime cost of enforcing type soundness would be high in realistic applications.
Nevertheless, a number of industry labs found the idea of migrating dynamically typed code to a static type discipline compelling, and implemented @emph{optionally typed} variants of dynamically typed languages.
Microsoft developed TypeScript@note{@url{https://www.typescriptlang.org/}} for JavaScript, Facebook developed Hack@note{@url{http://hacklang.org/}} for PHP and Flow@note{@url{https://flowtype.org/}} for JavaScript, and Dropbox is now funding the @tt{mypy}@note{@url{http://mypy-lang.org/}} project for Python.
These languages provide the static benefits of type annotations and impose @emph{zero} runtime overhead, but do not deliver runtime guarantees.
Programs written in these optionally typed languages are therefore suceptible to the errors and inefficiencies described in @secref{sec:overhead}.

The burning question for the gradual typing research program is whether gradual type systems impose unacceptable performance overhead.
If so, the followup question is whether this overhead is fundamental.
This paper addresses the first question by proposing an evaluation method for gradual type systems:
 @itemlist[#:style 'ordered
   @item{
     To @emph{measure} the performance of a gradual type system, first fix a granularity for adding or removing type annotations.
     Then annotate a suite of representative benchmark programs
     Finally measure the running time of all possible typed/untyped @emph{configurations} subject to the granularity.
   }
   @item{
     To express the @emph{absolute performance} of the gradual type system, report the proportion of configurations in each benchmark that are @step{} using @emph{overhead graphs}.
     In the ideal world, all but a few configurations should be @step["0" "1.1"].
   }
   @item{
     To express the @emph{relative performance} of two gradual type systems, plot their overhead graphs on the same axis and test whether the difference is statistically significant.
     Ideally, the newer system should demonstrate a significant ``left shift''.
   }
 ]
Applying the evaluation method to Typed Racket has confirmed that the method works well to uncover performance issues and to quantify improvements between implementations of a gradual type system.
We therefore recommend the method to maintainers of other gradual type systems, and to any researcher investigating the performance of a multi-language system.
An overhead graph could show the performance tradeoff as one converts parts of a program into a different language.
For example, the SoftDev group at King's College has a JIT compiler that integrates PHP and Python@~cite[bbdt-ecoop-2016].

The results of the paper's particular evaluation suggest three vectors of future research for gradual typing.
First, the maintainers of other gradual type systems must evaluate their systems' performance.
Second, the empirical accuracy of simple random sampling reported in @secref{sec:scale} must be confirmed on larger programs and in the context of micro-level gradual typing.
Third, the research community must explore tools to help navigate a performance lattice.
If developers can quickly find performant configurations in a lattice, then it will not matter if large portions of the lattice are not @deliverable[].
@; Such tools would include editor support for managing type annotations, a static model of the price for enforcing a type annotation dynamically, and a @emph{boundary profiler} that attributes runtime overhead to particular type boundaries@~cite[saf-cc-2015].

Finally, Typed Racket must address the pathologies identified in @secref{sec:devils}.
Here are a few suggestions.
To reduce the cost of high-frequency checks, the runtime system could cache the results of successful checks@~cite[rf-pldi-2016] or implement a tracing JIT compiler tailored to identify dynamic type assertions@~cite[bauman-et-al-icfp-2015].
High-cost types may be a symptom of inefficiencies in the translation from types to dynamic checks.
Lastly, Racket's chaperones need an effective way to identify and eliminate redundant contracts on a value@~cite[stw-pldi-2015 g-popl-2015].


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

