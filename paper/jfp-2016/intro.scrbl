#lang scribble/base

@; "The first challenge for computer science is to discover how to maintain
@;  order in a finite, but very large, discrete universe that is intricately
@;  intertwined."  -- Dijkstra, 1979 (from Emina's thesis)

@require["common.rkt" "util.rkt" "benchmark.rkt"]

@profile-point{sec:intro}
@title[#:tag "sec:intro"]{The Gradual Typing Design Space}

Dynamically typed languages are a staple of the software engineering world.
Telecom companies have been running Erlang programs for years@~cite[armstrong-2007],
 Sweden's pension system is a Perl program@~cite[v-aplwa-2010],
 and the @emph{lingua franca} of the Internet is JavaScript.
New companies frequently choose languages like Python, PHP, and Ruby;
 see for example, Dropbox, Facebook, and Twitter.

Regardless of why programmers build applications in dynamically typed languages,
 the maintainers of these applications inevitably find the lack of explicit type
 annotations an obstacle to their work.
     @; Confirming problem, and responses:
     @; - PEP type hints
     @; - pycharm parsing comments
     @; - type-testing JITS for php/js
     @; - typescript flow
Explicit annotations clarify a programmer's intent to other human readers.
Furthermore, automated tools can use the annotations to detect inconsistencies
 and compile more efficient code.
Despite many attempts to infer type information from dynamically typed programs@~cite[cf-pldi-1991 mfsw-hosc-2005 acfh-popl-2011],
 there is no substitute for explicit annotations.

@; Enter GT
Gradual typing@~cite[st-sfp-2006 thf-dls-2006]@note{We prefer the more descriptive term @emph{incremental typing}, but defer to the better sloganeer.}
 is a linguistic solution to the problem. @; What problem? Is it really clear enough?
    @; NOTE: a GT "language" is ideally a "superset" of an existing lang,
    @;       but the cast calculus & gradualizer/foundations have their place
In a gradually typed language,
 programmers can incrementally add type annotations to dynamically typed code.
At the lexical boundaries between annotated (or type-inferred) code and dynamically
 typed code, the type system inserts runtime checks to guarantee the soundness
 of the type annotations.

From a syntactic perspective the interaction is seamless, but dynamic checks introduce runtime overhead.
During execution, if an untyped function flows into a variable @racket[f] with
 type @racket[(Int -> Int)] then a dynamic check must
 follow @emph{every} subsequent call to @racket[f].
The check must assert that the result each each call would have the syntactic type @racket[Int].
Conversely, typed functions flowing into untyped code must dynamically check
 their argument values.
If functions or large data structures frequently cross these
 @emph{type boundaries},
 the runtime cost of enforcing type soundness could be tremendous.

Optimistically, researchers continued to explore the theory and practice of sound
 gradual typing@~cite[htf-tfp-2007
                      sgt-esop-2009
                      TypedRacket
                      wgta-ecoop-2011
                      acftd-scp-2013
                      aft-dls-2013
                      rnv-ecoop-2015
                      vksb-dls-2014
                      rsfbv-popl-2015
                      gc-popl-2015].@note{See @url{https://github.com/samth/gradual-typing-bib} for a full bibliography.}
Some research groups invested significant resources implementing sound gradual type systems.
These implementations often revealed dynamically typed features that conventional
 type systems cannot express; research on types and dynamic enforcement for such features continues.
Suprisingly few groups evaluate the performance of gradual typing in their system.
Most acknowledge performance in passing, as subject for future work.
Worse yet, others report the performance ratio of fully typed programs relative to
 fully untyped programs, ironically ignoring the entire space of programs
 mixing typed and untyped components.

This paper offers a foundation for the comprehensive performance evaluation of gradual type systems.
The method is to
 (1) fix a granularity for adding or removing type annotations,
 (2) fully type a suite of representative benchmark programs,
 (3) consider all possible ways of removing a subset of type annotations in each program,
 (4) report the @emph{overhead} of these gradually typed @emph{configurations}
     relative to the baseline performance of the fully untyped program.
We demonstrate that the method characterizes
 both the @emph{absolute} performance of a gradual type system and the @emph{relative}
 performance of two implementations of gradual typing for the same untyped language.
In particular, we evaluate the performance of @integer->word[(length (*RKT-VERSIONS*))]
 Typed Racket versions on a suite of @integer->word[(*NUM-BENCHMARKS*)] functional
 and object-oriented benchmark programs ranging in size and complexity.

Building on the evaluation method introduced in an earlier conference version@~cite[tfgnvf-popl-2016],
 this paper contributes:
@itemlist[
  @item{
    data for @integer->word[(*NUM-OO-BENCHMARKS*)]
     object-oriented benchmark programs and
     @integer->word[(- (*NUM-BENCHMARKS*) (*NUM-OO-BENCHMARKS*) NUM-POPL)]
     additional functional programs;
  }
  @item{
    comparisons across @integer->word[(length (*RKT-VERSIONS*))] versions
     of Typed Racket;
  }
  @item{
    and in-depth discussions of performance bottlenecks in each benchmark,
     their resolution,
     and general lessons for implementors of gradually typed languages.
  }
  @;@item{
  @;  preliminary reports on a method for predicting the performance overhead for
  @;   any of a program's @exact{$2^N$} configurations after taking @exact{$O(N)$}
  @;   measurements.
  @;}
]

@parag{Disclaimer:} 3x overhead is not "deliverable" performance.
ae never claimed so in the conference version and our opinion certainly has not changed.
