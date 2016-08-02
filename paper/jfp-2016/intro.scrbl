#lang scribble/base

@; "The first challenge for computer science is to discover how to maintain
@;  order in a finite, but very large, discrete universe that is intricately
@;  intertwined."  -- Dijkstra, 1979 (from Emina's thesis)

@require["common.rkt" "typed-racket.rkt"]

@profile-point{sec:intro}
@title[#:tag "sec:intro"]{The Gradual Typing Design Space}

Dynamically-typed languages have become a staple of the software
 engineering world.
Programmers use these languages to build applications
 ranging from telecommunications software@~cite[armstrong-2007]
 to social networking websites (Facebook, Twitter)
 to an entire country's pension system@~cite[v-aplwa-2010].
These software systems often begin as prototypes in which the flexibility of
 dynamic typing speeds development.
But as programs grow in size and complexity, software maintenance
 becomes the bottleneck.
When this happens, the assurance provided by a static type system becomes
 increasingly desirable; however, migrating an existing untyped project to a
 typed language is a prohibitively large engineering investment.
@; Twitter made the jump (TODO where to put this?)
@; ### https://blog.twitter.com/2011/twitter-search-is-now-3x-faster
@; - switched from ruby-on-rails to java "frontend" to the database
@; - changed database: mysql -> lucene
@; - better code: no thread blocks on IO
@; ### http://www.artima.com/scalazine/articles/twitter_on_scala.html
@; - rails great for front-end, had performance limitations in back-end
@; - "the Ruby language lacks some things that contribute to reliable, high performance code, which is something we’re very interested in as we’re growing as a business"
@; - Ruby is bad environment for long-lived processes
@; - Scala has non-painful static typingA
@; - "Sometimes it would be really nice in Ruby to say things like, here’s an optional type annotation"
@; - green threads aren't good enough
@; - GC not as good as Java's
@; - "As our system has grown, a lot of the logic in our Ruby system sort of replicates a type system, either in our unit tests or as validations on models"
@; - scala very performant and stable

@; Enter GT
Gradual typing@~cite[st-sfp-2006 thf-dls-2006] proposes a language-based
 solution to resolve the tradeoffs between dynamic and static typing.
The idea is to extend an existing, dynamically-typed language to allow the incremental
 addition of static types.
Programmers enable typechecking by writing type annotations.
These annotations are checked and enforced by the compiler.
Unannotated parts of the program have no static guarantees but may freely
 interact with typed code by sharing values across so-called @emph{type boundaries}.
At runtime, the gradual type system dynamically checks untyped values flowing
 into typed code and dynamically protects typed values entering untyped code.
From programmers' perspective, the interaction is seamless.

@; So-called @emph{macro}-level gradual type systems implement type boundaries
@;  as module boundaries.
@; That is, any module in the program is either fully typed or fully untyped.
@; @emph{Micro}-level gradual type systems allow type boundaries
@;  between expressions within any module.
@; For example, a function may require its arguments be typed but produce an
@;  untyped result.

In the decade since gradual typing was first proposed, research groups have
 equipped
 JavaScript@~cite[rnv-ecoop-2015 rsfbv-popl-2015],
 Python@~cite[vksb-dls-2014],
 Racket@~cite[TypedRacket],
 @; Ruby@~cite[furr-dissertation-2009],
 and
 Smalltalk@~cite[acftd-scp-2013]
 with gradual type systems.
Each new extension must address challenges unique to its base language,
 but in general these gradual type systems have three broad goals:
 @itemlist[
   @item{@emph{Expressiveness:} describe all untyped features with useful types}
   @item{@emph{Soundness:} enforce the semantics of types at run-time}
   @item{@emph{Performance:} leverage types in efficient compilers and IDE tools}
 ]
Soundness for gradual type systems is traditionally formulated as a
 type soundness theorem guaranteeing that typed parts of a program never
 commit runtime type errors@~cite[thf-dls-2006].
Typed code may raise a type error at runtime upon receiving untyped data
 that does not match the type checker's assumptions, but typed code will never
 execute a single instruction using invalid data.
Consequently, type errors in a gradually typed program always reference a
 boundary where an unexpected value flowed into typed code.
With this information, a programmer can determine whether the untyped value
 or static type annotation is at fault.

Gradual type systems enforce type soundness by inserting dynamic checks at
 type boundaries.
These systems compile a static type @exact|{$\RktMeta{T}$}| to an assertion
 @exact|{$\ctc{\RktMeta{T}}$}| that guarantees the run-time behavior of an
 untyped program component matches the component's type, as assigned by an
 inferred or explicit type annotation@~cite[aft-dls-2013 TypedRacket sw-popl-2010].
For example, if the type checker assumes that an untyped function has type
 @racket[(Int -> Int)] then every value returned by the function at run-time
 is dynamically checked against the specification @exact|{$\ctc{\RktMeta{Int}}$}|.

Dynamic checks, however, introduce performance overhead and slowdowns of
 4x@~cite[tfdffthf-ecoop-2015],
 10x@~cite[vksb-dls-2014],
 and
 72x@~cite[rsfbv-popl-2015]
 appear in the literature.
@; Allende (DLS'13) dont seem to report a slowdown. Just,
@;  "as type annotations are added to a library, performance tends to degrade"
These performance problems imply a steep tradeoff between preserving type
 soundness and maintaining performance when adding types to an untyped program.
The aim of this paper is to provide a foundation for measuring
 and understanding the tradeoff.
Given a fixed granularity for type boundaries and both fully-untyped and
 fully-typed versions for a representative suite of benchmarks, this paper
 recommends measuring the performance of all so-called @emph{configurations}
 obtained by annotating a subset of possible locations in each benchmark with types.
In other words, if a benchmark has @math{N} locations that may be type-annotated
 then there are @exact{$2^N$} configurations to measure.
The performance overhead of these configurations relative to the fully-untyped
 benchmark is, we claim, the most accurate measure for the performance of the
 whole gradual type system.

@; In general, the granularity of type boundaries is fixed by the gradual type system,
@;  but there are many valid typings for a single program.
@; Choosing a representative suite of benchmarks is, as always, a subjective matter,
@;  but otherwise the method offers a strict protocol.

We apply the method to Typed Racket, where type boundaries are module boundaries.
The evaluation affirms that Typed Racket programs may suffer order-of-magnitude
 overhead when gradually typed, but also suggests concrete improvements.
Furthermore, the method is useful for quantifying improvements and identifying
 regressions across different versions of Typed Racket.

The method was originally presented in a conference publication@~cite[tfgnvf-popl-2016].
This paper extends that prior work with:
@itemlist[
  @item{
    a comparative analysis of @integer->word[(length (*RKT-VERSIONS*))] versions
     of Typed Racket, using the method to measure differences between versions;
  }
  @item{
    results for @integer->word[(count-new-oo-benchmarks)]
     object-oriented benchmark programs, augmenting our previous suite of
     @id[(- (count-benchmarks) (count-new-oo-benchmarks))]
     mostly-functional benchmark programs;
  }
  @item{
    in-depth discussions of performance bottlenecks in each benchmark and,
     where applicable, their resolution;
  }
  @item{
    preliminary reports on a method for predicting the performance overhead for
     any of a program's @exact{$2^N$} configurations after taking @exact{$O(N)$}
     measurements.
  }
]

