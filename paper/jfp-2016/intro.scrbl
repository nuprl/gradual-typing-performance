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
The server-side applications of some modern companies (Dropbox, Facebook, Twitter) are written in dynamic languages (respectively Python, PHP and Ruby).

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
Worse, others report only the performance ratio of fully typed programs relative to fully untyped programs, ironically ignoring the entire space of programs that mix typed and untyped components.

This archival paper presents a systematic method for evaluating the performance of a gradual type system (@secref{sec:method}).
The fundamental aspects of the method were presented in an earlier conference paper@~cite[tfgnvf-popl-2016]; this paper contributes:
@itemlist[
@item{
  @integer->word[(- (length ALL-BENCHMARKS) NUM-POPL)] additional benchmark programs (@secref{sec:bm});
}
@item{
  validation that the method can express the relative performance between @integer->word[(length (*RKT-VERSIONS*))] implementations of Typed Racket (@secref{sec:compare});
}
@item{
  evidence that simple random sampling can accurately approximate the results of a comprehensive evaluation with asymptotically fewer measurements (@secref{sec:scale});
}
@item{
  and an in-depth discussion of the pathological performance overheads in the benchmark programs (@secref{sec:devils}).
}
]
The discussion is intended to guide implementors of gradual type systems toward promising future directions (@secref{sec:fut}).

This paper begins with an extended introduction to our philosophy towards gradual typing and the pragmatics of Typed Racket.
@Secref{sec:overhead} in particular argues that type soundness is an imperative, despite the performance cost of enforcing it.

@;Experience with Typed Racket provides the broader context for this work and motivates the design of the evaluation method.


@;@parag{Disclaimer:} 3x overhead is not "deliverable" performance.
@;We never claimed so in the conference version and our opinion certainly has not changed.
