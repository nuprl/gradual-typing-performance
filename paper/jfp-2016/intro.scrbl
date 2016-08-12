#lang scribble/base

@; "The first challenge for computer science is to discover how to maintain
@;  order in a finite, but very large, discrete universe that is intricately
@;  intertwined."  -- Dijkstra, 1979 (from Emina's thesis)

@require["common.rkt" "util.rkt" "benchmark.rkt"]

@profile-point{sec:intro}
@title[#:tag "sec:intro"]{The Gradual Typing Design Space}

@(define twitter1 "http://www.artima.com/scalazine/articles/twitter_on_scala.html")
@(define twitter2 "https://blog.twitter.com/2011/twitter-search-is-now-3x-faster")

Dynamically-typed languages have become a staple of the software
 engineering world.
Programmers use these languages to build applications
 ranging from telecommunications software@~cite[armstrong-2007]
 to high-profile startups (Dropbox, Facebook, Twitter)
 and a country's pension system@~cite[v-aplwa-2010].
These software systems often begin as prototypes in which the flexibility of
 dynamic typing speeds development.
As programs grow in size and complexity, however, software maintenance
 becomes a significant bottleneck. @; M: due to the lack of types
When this happens, the assurances of a static type system become increasingly desirable.
In fact Twitter recently migrated their codebase from Ruby to Scala because of
 the performance and reliability Scala's type system offers.@note{@hyperlink[twitter1 twitter1]}

@; Enter GT
Gradual typing@~cite[st-sfp-2006 thf-dls-2006] proposes a language-based
 solution resolve the tradeoffs of dynamic and static typing.
   @; it is NOT about software migration! NON ONONONONON because fully-typed isn't the goal
The idea is to extend an existing, dynamically-typed language to allow the incremental
 addition of static types.
Programmers enable typechecking by writing type annotations.
The compiler validates and enforces these annotations.
Unannotated parts of the program have no static guarantees but may freely
 interact with typed code across so-called @emph{type boundaries}.
At runtime, the gradual type system dynamically checks untyped values flowing
 into typed code and dynamically protects typed values entering untyped code.

From a syntactic perspective, the interaction is seamless; however,
 dynamic checks introduce runtime overhead.
When an untyped function @racket[f] flows into typed code at type @racket[(Int -> Int)],
 every subsequent call to @racket[f] triggers a runtime assertion that the value
 produced by @racket[f] actually has type @racket[Int].
In general, these "runtime type checks" can have an arbitrarily large cost if
 values with intricate types frequently cross type boundaries.
In practice, sound gradual type systems for Racket, Python, and JavaScript
 have demonstrated overheads of
 3.4x@~cite[tfdffthf-ecoop-2015],
 10x@~cite[vksb-dls-2014],
 and
 72x@~cite[rsfbv-popl-2015].
@; Allende (DLS'13) dont seem to report a slowdown. Just,
@;  "as type annotations are added to a library, performance tends to degrade"
@; Richards @~cite[rnv-ecoop-2015] only reports
@;  fully-typed vs. typescript. Also, "blame was intolerable"

In particular, Vitousek @|etal| report the 10x slowdown for a fully typed
 hash algorithm relative to Python's performance on the same program with all
 types removed.
This is the only overhead relative to Python reported in the paper.
The 72x figure for Safe TypeScript was the worst overhead the authors observed
 on six fully-untyped benchmarks.
The average overhead for their untyped benchmarks was 22x, and the minimum overhead was 2.4x.
When fully typed, the same benchmarks' average overhead fell to 0.065x.

Performance of gradual type systems is clearly an issue, but the @emph{meta-issue}
 belied by the reported overhead is that @emph{prior evaluations of gradual type systems
 fail to consider the performance of gradually-typed programs}.
Each of the Python and JavaScript benchmarks can mix typed and untyped code in
 exponentially many ways.
None of these mixed programs are represented in the evaluations.

Takikawa @|etal|, on the other hand, measure the entire lattice of typed/untyped
 configurations for two object-oriented games.
    @; FYI their 3.4x figure was the worst overhead relative to untyped Racket (on a pre-patch TR)
Starting with a fully typed program, they consider all ways of removing types
 from parts of the program.
Typed Racket types may be removed at a module-level granularity, so a program
 with @exact{$N$} modules determines a lattice of @exact{$2^N$} configurations
 where configurations at the @exact{$i^{{\footnotesize\rm th}}$} level of the lattice have
 exactly @exact{$i$} typed modules.
This paper builds on their technique.

This paper also builds on a conference version that introduced our method for
 performance evaluation and applied it to measure the absolute performance of
 Typed Racket on a suite of benchmark programs@~cite[tfgnvf-popl-2016].
Relative to the conference version, this paper contributes:
@itemlist[
  @item{
    data for @integer->word[(*NUM-OO-BENCHMARKS*)]
     object-oriented benchmark programs, augmenting our previous suite of
     @integer->word[(- (*NUM-BENCHMARKS*) (*NUM-OO-BENCHMARKS*))]
     primarily functional benchmark programs;
  }
  @item{
    a comparison of @integer->word[(length (*RKT-VERSIONS*))] versions
     of Typed Racket, demonstrating that our method captures the @emph{relative}
     performance of two gradual type systems for the same language;
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
