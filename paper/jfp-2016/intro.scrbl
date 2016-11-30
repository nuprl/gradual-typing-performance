#lang scribble/base

@; "The first challenge for computer science is to discover how to maintain
@;  order in a finite, but very large, discrete universe that is intricately
@;  intertwined."  -- Dijkstra, 1979 (from Emina's thesis)

@require["common.rkt" "util.rkt" "benchmark.rkt"]

@profile-point{sec:intro}
@title[#:tag "sec:intro"]{The Gradual Typing Design Space}

Programmers use dynamically typed languages to build all kinds of applications.
Telecom companies have been running Erlang programs for years@~cite[armstrong-2007];
 Sweden's pension system is a Perl program@~cite[l-freenix-2006],
 and the @emph{lingua franca} of the Internet is JavaScript.
New companies frequently use languages such as Python, PHP, and Ruby in server-side applications;
 see for example, Dropbox, Facebook, and Twitter.
 @; TODO need citation for the companies

Regardless of why programmers choose dynamically typed languages,
 the maintainers of these applications inevitably find the lack of explicit type
 annotations an obstacle to their work.
Such explicit annotations communicate a programmer's intent to other human readers.
Furthermore, tools can check the annotations for logical inconsistencies
 and leverage types to improve the efficiency of compiled code.
Researchers have tried to overcome the lack of type annotations with inference
 algorithms@~cite[cf-pldi-1991 awl-popl-1994 hr-fpca-1995 agd-ecoop-2005 fafh-sac-2009 rch-popl-2012],
 but there is no substitute for programmer-supplied annotations.
     @; Confirming problem, and responses:
     @; - PEP type hints
     @; - pycharm parsing comments
     @; - type-testing JITS for php/js
     @; - typescript flow

One solution to the problem is to rewrite the entire application in a statically typed language.
This solution assumes that the application is small enough and the problem is recognized soon enough to make a wholesale migration feasible.
For example, Twitter was able to port their server-side code from Ruby to Scala because they understood the problem early on.@note{@url{http://www.artima.com/scalazine/articles/twitter_on_scala.html}}
In other cases, the codebase becomes too large for this approach.

@; Enter GT
Another solution to the problem is gradual typing@~cite[st-sfp-2006 thf-dls-2006],@note{The term @emph{migratory typing} more accurately describes this particular mode of gradual typing.}
 a linguistic approach.
 @; What problem? Is it really clear enough?
    @; NOTE: a GT "language" is ideally a "superset" of an existing lang,
    @;       but the cast calculus & gradualizer/foundations have their place
In a gradually typed language,
 programmers can incrementally add type annotations to dynamically typed code.
At the lexical boundaries between annotated code and dynamically
 typed code, the type system inserts runtime checks to guarantee the soundness
 of the type annotations.

From a syntactic perspective the interaction is seamless, but dynamic checks introduce runtime overhead.
During execution, if an untyped function flows into a variable @type{$f$} with
 type @type{$\tau_1 \rightarrow \tau_2$}, then a dynamic check must
 follow every subsequent call to @type{$f$}
 because typed code cannot assume
 that values produced by the untyped function have the syntactic type @type{$\tau_2$}.
Conversely, typed functions invoked by untyped code must dynamically check their argument values.
If functions or large data structures frequently cross these
 @emph{type boundaries},
 enforcing type soundness might impose a huge runtime cost.

Optimistically, researchers have continued to explore the theory and practice of sound
 gradual typing@~cite[ktgff-tech-2007
                      sgt-esop-2009
                      thf-popl-2008
                      wgta-ecoop-2011
                      acftd-scp-2013
                      rnv-ecoop-2015
                      vksb-dls-2014
                      rsfbv-popl-2015
                      gc-popl-2015].@note{See @url{https://github.com/samth/gradual-typing-bib} for a bibliography.}
Some research groups have invested significant resources implementing sound gradual type systems.
Suprisingly few groups have evaluated the performance of gradual typing.
Most acknowledge an issue with performance in passing.
Worse, others report only the performance ratio of fully typed programs relative to
 fully untyped programs, ironically ignoring the entire space of programs
 that mix typed and untyped components.

This paper presents a systematic method for evaluating the performance of a gradual type system on a suite of benchmark programs.
The method is useful for comprehensively evaluating both the absolute performance of a gradual type system and the relative performance of two implementations of the same gradual type system.
As validation, the paper applies the method to a suite of @integer->word[(*NUM-BENCHMARKS*)] Typed Racket programs (the @|GTP| benchmarks suite) and compares their performance across @integer->word[(length (*RKT-VERSIONS*))] versions of Typed Racket.
This paper furthermore demonstrates that simple random sampling can approximate the results of the comprehensive evaluation with asymptotically fewer measurements.
@; (from exponential to linear).
@; The success of the sampling protocol is evidence that the evaluation method is practical for large software applications.

A conference version of this paper introduced and validated the fundamental aspects of the method@~cite[tfgnvf-popl-2016].
This work extends the method to relative performance, introduces the sampling method, and evaluates @integer->word[(- (*NUM-BENCHMARKS*) NUM-POPL)] additional benchmark programs.

@bold{Outline}
  @Secref{sec:story} is an extended introduction to gradual typing in Typed Racket.
  @Secref{sec:method} derives the evaluation method from the lessons of @secref{sec:story}.
  The following three sections present the benchmark programs (@secref{sec:bm}), the results of the performance evaluation @secref{sec:tr}, and the sampling method (@secref{sec:scale}).
  @Secref{sec:threats} discusses threats to the validity of our experiment; @secref{sec:devils} describes the performance overheads in the benchmarks.
  Lastly, @secref{sec:fut} surveys the research landscape and suggests future directions. @;
@exact{\hfill$\blacksquare$}

@; TODO roadmap, big sections
@; - extended introduction/motivation, gradual typing in typed racket, philosophy
@; - landscape, metrics, visualization (scrape sec 3)
@; - benchmarks (sec 4) exhaustive results, exponential (sec 5) srs can approximate (sec 6)
@; - threats to validity, pathologies, conclusion = past and present of research landscape


@;@parag{Disclaimer:} 3x overhead is not "deliverable" performance.
@;We never claimed so in the conference version and our opinion certainly has not changed.
