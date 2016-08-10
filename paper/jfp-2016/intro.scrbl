#lang scribble/base

@; "The first challenge for computer science is to discover how to maintain
@;  order in a finite, but very large, discrete universe that is intricately
@;  intertwined."  -- Dijkstra, 1979 (from Emina's thesis)

@require["common.rkt" "util.rkt" "jfp-parameters.rkt"]

@profile-point{sec:intro}
@title[#:tag "sec:intro"]{The Gradual Typing Design Space}

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
In fact Twitter recently migrated their codebase from Ruby to Scala because
 of performance and reliability concerns,@note{http://www.artima.com/scalazine/articles/twitter_on_scala.html}
 despite the engineering and opportunity cost of the migration.
@; ### https://blog.twitter.com/2011/twitter-search-is-now-3x-faster

@; Enter GT
Gradual typing@~cite[st-sfp-2006 thf-dls-2006] proposes a language-based
 solution to assist developers with the migration from dynamically typed
 scripts to statically typed programs.
The idea is to extend an existing, dynamically-typed language to allow the incremental
 addition of static types.
Programmers enable typechecking by writing type annotations.
The compiler validates and enforces these annotations.
Unannotated parts of the program have no static guarantees but may freely
 interact with typed code across so-called @emph{type boundaries}.
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
In particular, typed code may signal a type error at runtime upon receiving untyped data
 that does not match the type checker's assumptions, but typed code will never
 execute a single instruction using invalid data.
Consequently, every runtime type error raised by a gradual type system references
 boundary where an unexpected value flowed into typed code.
With this information, a programmer can determine whether the untyped value
 or static type annotation is at fault and correct the impedence mismatch.

Gradual type systems enforce type soundness by inserting dynamic checks at
 type boundaries.
These systems compile a static type @exact|{$\RktMeta{T}$}| to an assertion
 @exact|{$\ctc{\RktMeta{T}}$}| that guarantees the run-time behavior of an
 untyped program component matches the component's type, as assigned by an
 inferred or explicit type annotation@~cite[aft-dls-2013 TypedRacket sw-popl-2010].
For example, if the type checker assumes that an untyped function has type
 @racket[(Int -> Int)] then every value returned by the function at run-time
 is dynamically checked against the specification @exact|{$\ctc{\RktMeta{Int}}$}|.

Dynamic checks introduce performance overhead.
Slowdowns of
 4x@~cite[tfdffthf-ecoop-2015],
 10x@~cite[vksb-dls-2014],
 and
 72x@~cite[rsfbv-popl-2015]
 appear in the literature.
@; Allende (DLS'13) dont seem to report a slowdown. Just,
@;  "as type annotations are added to a library, performance tends to degrade"
These figures imply a steep tradeoff between preserving type
 soundness and maintaining performance when adding types to an untyped program.
The aim of this paper is to provide a foundation for measuring
 and understanding the tradeoff.
@; BLAH
Given a fixed granularity for type boundaries and both fully-untyped and
 fully-typed versions for a representative suite of benchmarks, this paper
 recommends @; BLAH
 measuring the performance of all so-called @emph{configurations}
 obtained by annotating a subset of possible locations in each benchmark with types.
In other words, if a benchmark has @math{N} locations that may be type-annotated
 then there are @exact{$2^N$} configurations to measure.
The performance overhead of these configurations relative to the fully-untyped
configuration is, we claim, the most accurate measure for the performance of the
 whole gradual type system.

@; In general, the granularity of type boundaries is fixed by the gradual type system,
@;  but there are many valid typings for a single program.
@; Choosing a representative suite of benchmarks is, as always, a subjective matter,
@;  but otherwise the method offers a strict protocol.

We apply the method to Typed Racket, where type boundaries are module boundaries.
The evaluation affirms that Typed Racket programs may suffer from an order-of-magnitude
 overhead when gradually typed.
Conversely, the evaluation offers insights for reducing some
 pathologies.
Furthermore, the method is useful for quantifying improvements and identifying
 regressions across different versions of Typed Racket.

The method was introduced in a conference publication@~cite[tfgnvf-popl-2016].
This paper extends that prior work with:
@itemlist[
  @item{
    a comparative analysis of @integer->word[(length (*RKT-VERSIONS*))] versions
     of Typed Racket, using the method to measure differences between versions;
  }
  @item{
    results for @integer->word[(*NUM-OO-BENCHMARKS*)]
     object-oriented benchmark programs, augmenting our previous suite of
     @id[(- (*NUM-BENCHMARKS*) (*NUM-OO-BENCHMARKS*))]
     primarily functional benchmark programs; and
  }
  @item{
    in-depth discussions of performance bottlenecks in each benchmark and,
     where applicable, their resolution.
  }
  @;@item{
  @;  preliminary reports on a method for predicting the performance overhead for
  @;   any of a program's @exact{$2^N$} configurations after taking @exact{$O(N)$}
  @;   measurements.
  @;}
]

