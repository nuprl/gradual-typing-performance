#lang scribble/base

@; "The first challenge for computer science is to discover how to maintain
@;  order in a finite, but very large, discrete universe that is intricately
@;  intertwined."  -- Dijkstra, 1979 (from Emina's thesis)

@require["common.rkt" "util.rkt" "benchmark.rkt"]

@profile-point{sec:intro}
@title[#:tag "sec:intro"]{The Gradual Typing Design Space}

Programmers use dynamically typed languages to build all kinds of applications.
Telecom companies have been running Erlang programs for years@~cite[armstrong-2007];
 Sweden's pension system is a Perl program@~cite[v-aplwa-2010];
 and the @emph{lingua franca} of the Internet is JavaScript.
New companies frequently use languages such as Python, PHP, and Ruby in server-side applications;
 see for example, Dropbox, Facebook, and Twitter.

Regardless of why programmers choose dynamically typed languages,
 the maintainers of these applications inevitably find the lack of explicit type
 annotations an obstacle to their work.
Explicit annotations communicate a programmer's intent to other human readers.
Furthermore, the toolchain can check the annotations for inconsistencies
 and leverage types to improve the efficiency of compiled code.
Researchers have tried to overcome the lack of type annotations with inference algorithms,
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
Another solution to the problem is gradual typing@~cite[st-sfp-2006 thf-dls-2006],@note{We prefer the more descriptive term @emph{incremental typing}, but defer to the established slogan.}
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
During execution, if an untyped function flows into a variable @racket[f] with
 type @type{(Real\,->\,Real)}, then a dynamic check must
 follow every subsequent call to @racket[f]
@;These checks are necessary 
 because typed code cannot trust
 that the untyped function produces values that have the syntactic type @racket[Real].
Conversely, typed functions invoked by untyped code must dynamically check their argument values.
If functions or large data structures frequently cross these
 @emph{type boundaries},
 enforcing type soundness might impose a huge runtime cost.

Optimistically, researchers have continued to explore the theory and practice of sound
 gradual typing@~cite[htf-tfp-2007
                      sgt-esop-2009
                      TypedRacket
                      wgta-ecoop-2011
                      acftd-scp-2013
                      rnv-ecoop-2015
                      vksb-dls-2014
                      rsfbv-popl-2015
                      gc-popl-2015].@note{See @url{https://github.com/samth/gradual-typing-bib} for a bibliography with over 75 entries on gradual typing as of August 2016.}
Some research groups have invested significant resources implementing sound gradual type systems.
Suprisingly few groups have rigourously evaluated the performance of gradual typing.
Most acknowledge an issue with performance in passing.
Worse, others report only the performance ratio of fully typed programs relative to
 fully untyped programs, ironically ignoring the entire space of programs
 that mix typed and untyped components.

This paper presents a method for the performance evaluation of gradual type systems:
 @itemlist[#:style 'ordered
   @item{
     fix a granularity for adding or removing type annotations;
   }
   @item{
     annotate a suite of representative benchmark programs as fully as possible;
   }
   @item{
     consider all possible ways of removing a subset of type annotations in each program, subject to the granularity as fixed in (1);
   }
   @item{
     report the overhead of these gradually typed @emph{configurations}
     relative to the baseline performance of the fully untyped program.
   }
 ]
We demonstrate that the method characterizes
 both the @emph{absolute} performance of a gradual type system and the @emph{relative}
 performance of two implementations of gradual typing for the same untyped language.
To validate the method, we evaluate the performance of @integer->word[(length (*RKT-VERSIONS*))]
 Typed Racket versions on a suite of @integer->word[(*NUM-BENCHMARKS*)] functional
 and object-oriented benchmark programs ranging in size and complexity.

Building on the evaluation method introduced in the conference version of this paper@~cite[tfgnvf-popl-2016],
 we contribute
 the first comparitive evaluation of different implementations of the same gradual type system;
 @integer->word[(- (*NUM-BENCHMARKS*) NUM-POPL)] additional benchmark programs;
 an in-depth analysis of the performance bottlenecks in these benchmarks;
 and general lessons for implementors of gradually typed languages.

@parag{Disclaimer:} 3x overhead is not "deliverable" performance.
We never claimed so in the conference version and our opinion certainly has not changed.
