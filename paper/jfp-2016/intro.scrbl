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
Telecom companies have been running Erlang programs for years; Sweden's pension system is a Perl program@~cite[l-freenix-2006]; web developers build large programs with JavaScript and Node.js; and the server-side applications of Dropbox, Facebook, and Twitter were originally written in Python, PHP, and Ruby, respectively.

Regardless of why programmers choose dynamically typed languages, the maintainers of these applciations often face a problem; namely, the lack of explicit and reliable type annotations poses an obstacle to their work.
Researchers have tried to overcome the lack of type annotations with type inference for at least two decades@~cite[cf-pldi-1991 awl-popl-1994 hr-fpca-1995 agd-ecoop-2005 fafh-sac-2009 ffkwf-pldi-1996 suzuki-popl-1981], but most have come to realize that there is no substitute for programmer-supplied annotations.
Unlike types inferred from the structure of the code, explicit annotations communicate the original programmer's specification to other human readers and thereby serve as a guide when it comes time to repair or extend the code.
     @; Confirming problem, and responses:
     @; - PEP type hints
     @; - pycharm parsing comments
     @; - type-testing JITS for php/js
     @; - typescript flow

One approach to the maintenance problem is to rewrite the entire application in a statically typed language.
For example, engineers at Twitter ported their server-side code from Ruby to Scala because they decided the long-term performance and readability benefits outweighed the cost of the rewrite.@note{@url{http://www.artima.com/scalazine/articles/twitter_on_scala.html}}

@; Enter GT
Another approach is gradual typing@~cite[st-sfp-2006 thf-dls-2006].@note{@citet[tfffgksst-snapl-2017] refer to this use of gradual typing as @emph{migratory typing}.}
 @; What problem? Is it really clear enough?
    @; NOTE: a GT "language" is ideally a "superset" of an existing lang,
    @;       but the cast calculus & gradualizer/foundations have their place
In a sound gradually typed language,
 programmers can incrementally add type annotations to dynamically typed code.
At the lexical boundaries between annotated code and dynamically
 typed code, the type system inserts runtime checks to guarantee type soundness@~cite[tfffgksst-snapl-2017],
 which ensures that no typed operation is applied to arguments outside its domain.

From a syntactic perspective the interaction between annotated and unannotated code is seamless, but dynamic checks introduce runtime overhead.
If, during execution, an untyped function flows into a variable @type{$f$} with
 type @type{$\tau_1 \rightarrow \tau_2$}, then a dynamic check must
 guard every subsequent call to @type{$f$}
 because typed code cannot assume
 that values produced by the function have the syntactic type @type{$\tau_2$}.
Conversely, typed functions invoked by dynamically-typed code must check their argument values.
If functions or large data structures frequently cross these
 @emph{type boundaries},
 enforcing type soundness might impose a significant runtime cost and thus make the entire approach impractical.

Optimistically, researchers have continued to explore the theory of sound
 gradual typing@~cite[ktgff-tech-2007
                      sv-dls-2008
                      wgta-ecoop-2011
                      tsdthf-oopsla-2012
                      gc-popl-2015
                      gct-popl-2016].@note{See @url{https://github.com/samth/gradual-typing-bib} for a bibliography.}
Some research groups have invested significant resources implementing sound gradual type systems@~cite[acftd-scp-2013 rsfbv-popl-2015 vss-popl-2017 tfffgksst-snapl-2017].
But surprisingly few groups have evaluated the performance of gradual typing.
Many acknowledge an issue with performance in passing@~cite[thf-popl-2008 acftd-scp-2013 vksb-dls-2014].
Others report only the performance of fully annotated programs relative to fully unannotated programs@~cite[rsfbv-popl-2015 vss-popl-2017], ignoring the entire space of programs that mix typed and untyped components.

This archival paper presents the first method for evaluating the performance of a gradual type system (@secref{sec:method}), integrating new results with a conference version@~cite[tfgnvf-popl-2016].
Specifically, this paper contributes:
@itemlist[
@item{
  validation that the method can effectively compare the performance of @integer->word[(length (*RKT-VERSIONS*))] implementations of the same gradual type system (@secref{sec:compare});
}
@item{
  evidence that simple random sampling can approximate the results of an exhaustive evaluation with asymptotically fewer measurements (@secref{sec:scale});
}
@item{
  @integer->word[(- (length ALL-BENCHMARKS) NUM-POPL)] object-oriented benchmark programs to augment the functional benchmarks of @citet[tfgnvf-popl-2016] (@secref{sec:bm}); and
}
@item{
  a discussion of the pathological overheads in the benchmark programs (@secref{sec:devils}).
}
]
The discussion in @secref{sec:devils} is intended to guide implementors of gradual type systems toward promising future directions (@secref{sec:fut}).
In particular, there are challenges to address both in the implementation of sound gradual type systems and in helping users find combinations of typed and untyped code that meet their performance requirements.

@; This paper begins with a brief introduction to Typed Racket (@secref{sec:story}) and the migratory flavor of gradual typing that it supports.
