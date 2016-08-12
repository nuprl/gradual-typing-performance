#lang scribble/base

@; "The first challenge for computer science is to discover how to maintain
@;  order in a finite, but very large, discrete universe that is intricately
@;  intertwined."  -- Dijkstra, 1979 (from Emina's thesis)

@require["common.rkt" "util.rkt" "benchmark.rkt"]

@profile-point{sec:intro}
@title[#:tag "sec:intro"]{The Gradual Typing Design Space}

Dynamically-typed languages are a staple of the software
 engineering world.
Programmers use these languages to build applications
 ranging from telecommunications software@~cite[armstrong-2007]
 to high-profile startups (Dropbox, Facebook, Twitter)
 and a country's pension system@~cite[v-aplwa-2010].
Statically-typed languages offer engineering benefits.
Eventually, the maintainers of dynamically-typed systems find themselves
 wishing for some benefit realizable through static typing@~cite[ls-ppdp-2006 l-stop-2015 aemopssy-oopsla-2014 t-artima-2009].

@; Enter GT
Gradual typing@~cite[st-sfp-2006 thf-dls-2006]@note{We prefer the more descriptive term @emph{incremental typing}, but defer to the better sloganeer.}
 lets programmers formalize
 properties of their code in explicit, machine-checked type annotations.
On one hand, these annotations convey the author's intent to future human readers.
Automated tools can also leverage the
 annotations to speed the development and running time of gradually typed programs.
At the lexical boundaries between annotated (or type-inferred) code and dynamically
 typed code, gradual type systems insert runtime checks to guarantee the soundness
 of type annotations.

From a syntactic perspective the interaction is seamless, but dynamic checks introduce runtime overhead.
If an untyped function @racket[f] flows into typed code at type @racket[(Int -> Int)]
 then every subsequent call to @racket[f] necessitates a runtime check ensuring
 the result value would have the syntactic type @racket[Int].
The aggregate cost of such checks depends on the frequency of interaction
 across so-called @emph{type boundaries}.
If large mutable data structures or first-class objects repeatedly cross such boundaries,
 the runtime cost of enforcing type soundness could be tremendous.

@(define gt-bib "https://github.com/samth/gradual-typing-bib")
@(define num-gt-bib "over 75")

Nevertheless, researchers have implemented sound gradual type systems for
 Racket@~cite[TypedRacket],
 Smalltalk@~cite[acftd-scp-2013],
 Python@~cite[vksb-dls-2014],
 and JavaScript@~cite[rnv-ecoop-2015 rsfbv-popl-2015];
 subsequently, these researchers and their colleagues have published a great
 many papers on the subject of sound gradual typing@~cite[htf-tfp-2007
                                                          sgt-esop-2009
                                                          wgta-ecoop-2011
                                                          aft-dls-2013
                                                          gc-popl-2015].@note{See
    @hyperlink[gt-bib]{@tt{@|gt-bib|}} for a full bibliography (@|num-gt-bib| entries).}
Few publications consider the performance implications of runtime type checking.
Some mention performance overhead in passing.
Worse, others report only the performance of fully typed programs relative
 to untyped programs, ignoring the entire space of gradually typed programs.

This paper offers a foundation for the comprehensive performance evaluation of gradual type systems.
The method is to
 (1) fix a granularity for adding or removing type annotations,
 (2) fully type a suite of representative benchmark programs,
 (3) consider all possible ways of removing a subset of type annotations in each program,
 (4) report the @emph{overhead} of these gradually typed @emph{configurations}
     relative to the baseline performance of the fully untyped program.
Through an evaluation of Typed Racket, we show that the method characterizes
 both the @emph{absolute} performance of a gradual type system and the @emph{relative}
 performance of two implementations of gradual typing for the same untyped language.
In particular, we evaluate the performance of @integer->word[(length (*RKT-VERSIONS*))]
 Typed Racket versions on a suite of @integer->word[(*NUM-BENCHMARKS*)] functional
 and object-oriented benchmark programs ranging in size and complexity.

Our method originally stems from Takikawa @|etal|, who present the first evaluation
 of a gradual type system that considers the entire lattice of typed/untyped
 configurations for two object-oriented games.
This paper also builds on a conference version that introduced our method for
 performance evaluation and applied it to measure the absolute performance of
 Typed Racket on a suite of @integer->word[NUM-POPL] benchmark programs@~cite[tfgnvf-popl-2016].
Relative to the conference version, this paper contributes:
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
We never claimed so in the conference version and our opinion certainly has not changed.
