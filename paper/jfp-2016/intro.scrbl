#lang scribble/base

@; "The first challenge for computer science is to discover how to maintain
@;  order in a finite, but very large, discrete universe that is intricately
@;  intertwined."  -- Dijkstra, 1979 (from Emina's thesis)

@;; More anecdotes
@;http://archive.oreilly.com/pub/a/oreilly/perl/news/swedishpension_0601.html
@;http://diyhpl.us/~bryan/papers2/paperbot/7a01e5a892a6d7a9f408df01905f9359.pdf
@;http://programmers.stackexchange.com/questions/221615/why-do-dynamic-languages-make-it-more-difficult-to-maintain-large-codebases
@;https://www.quora.com/What-language-is-Facebook-written-in
@;http://www.zdnet.com/article/why-facebook-hasnt-ditched-php

@require["common.rkt" "util.rkt" "benchmark.rkt"]

@profile-point{sec:intro}
@title[#:tag "sec:intro"]{The Gradual Typing Design Space}

Programmers use dynamically typed languages to build all kinds of applications.
Telecom companies have been running Erlang programs for years@~cite[armstrong-2007]; Sweden's pension system is a Perl program@~cite[l-freenix-2006], web developers build large programs with JavaScript and Node.js@~cite[wmwz-ecoop-2017], and the server-side applications of some contemporary companies (Dropbox, Facebook, Twitter) were originally written in dynamic languages (Python, PHP, and Ruby, respectively).

Regardless of why programmers choose dynamically typed languages, the maintainers of these applications inevitably find the lack of explicit type annotations an obstacle to their work.
Researchers have tried to overcome the lack of type annotations with inference algorithms@~cite[cf-pldi-1991 awl-popl-1994 hr-fpca-1995 agd-ecoop-2005 fafh-sac-2009 rch-popl-2012], but most have come to realize that there is no substitute for programmer-supplied annotations.
Explicit annotations communicate a programmer's intent to other human readers.
Furthermore, tools can check the annotations for logical inconsistencies and leverage types during compilation.
     @; Confirming problem, and responses:
     @; - PEP type hints
     @; - pycharm parsing comments
     @; - type-testing JITS for php/js
     @; - typescript flow

One solution is to rewrite the entire application in a statically typed language.
This solution assumes that the application is small enough to make a wholesale migration feasible.
For example, Twitter was able to port their server-side code from Ruby to Scala because they recognized the long-term issues with their Ruby code early on.@note{@url{http://www.artima.com/scalazine/articles/twitter_on_scala.html}}

@; Enter GT
Another solution to the problem is gradual typing@~cite[st-sfp-2006 thf-dls-2006], a linguistic approach.@note{@citet[tfffgksst-snapl-2017] refer to this use of gradual typing as @emph{migratory typing}.}
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
@;@~cite[acftd-scp-2013 rsfbv-popl-2015 rnv-ecoop-2015 vss-popl-2017 tfffgksst-snapl-2017].
But surprisingly few groups have evaluated the performance of gradual typing.
Many acknowledge an issue with performance in passing@~cite[thf-popl-2008 acftd-scp-2013 vksb-dls-2014 rnv-ecoop-2015].
Others report only the performance of fully typed programs relative to fully untyped programs@~cite[rsfbv-popl-2015 vss-popl-2017], ironically ignoring the entire space of programs that mix typed and untyped components.

This archival paper presents the first method for evaluating the performance of a gradual type system (@secref{sec:method}), integrating new results with an earlier conference version@~cite[tfgnvf-popl-2016].
Specifically, this paper contributes:
@itemlist[
@item{
  validation that the method can express the relative performance between @integer->word[(length (*RKT-VERSIONS*))] implementations of Typed Racket (@secref{sec:compare});
}
@item{
  evidence that simple random sampling can approximate the results of an exhaustive evaluation with asymptotically fewer measurements (@secref{sec:scale});
}
@item{
  @integer->word[(- (length ALL-BENCHMARKS) NUM-POPL)] additional benchmark programs (@secref{sec:bm}); and
}
@item{
  a discussion of the pathological overheads in the benchmark programs (@secref{sec:devils}).
}
]
The discussion in @secref{sec:devils} is intended to guide implementors of gradual type systems toward promising future directions (@secref{sec:fut}).
In particular, there are challenges to address both in the implementation of sound gradual type systems and in helping users find combinations of typed and untyped code that meet their performance requirements.

This paper begins with a brief introduction to Typed Racket (@secref{sec:story}) and the @emph{migratory} flavor of gradual typing that it supports.
