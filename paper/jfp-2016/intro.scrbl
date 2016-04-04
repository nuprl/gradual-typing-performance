#lang scribble/base
@;TODO add roadmap refs

@; "The first challenge for computer science is to discover how to maintain
@;  order in a finite, but very large, discrete universe that is intricately
@;  intertwined."  -- Dijkstra, 1979 (from Emina's thesis)

@require["common.rkt" "typed-racket.rkt"]

@title[#:tag "sec:intro"]{The Gradual Typing Design Space}

@; Programmers use dynamic languages, dynamic languages don't scale super well
Dynamically-typed languages have become a staple of the software
 engineering world.
Programmers use these languages to build all kinds of software,
 ranging from telecommunications software@~cite[armstrong-2007]
 to social networking websites
 to an entire country's pension system@~cite[v-aplwa-2010].
These software systems often start as prototypes in which the flexibility of
 dynamic typing speeds development.
But as programs grow in size and complexity, software maintenance
 becomes the bottleneck.
When this happens, the assurance provided by a static type system becomes
 increasingly desirable.
Few teams, however, have the resources to move a successful project from a dynamic
 language to a statically-typed language.

@; Enter GT
Gradual typing@~cite[st-sfp-2006] proposes a language-based
 solution to resolve the tradeoffs between dynamic and static typing.
The idea is to extend an existing, dynamically-typed language to allow the incremental
 addition of static types.
So-called @emph{macro}-level gradual type systems support by-module increments:
 any untyped module may be converted to a fully-typed module.
In contrast, @emph{micro}-level gradual type systems allow the incremental
 conversion of any expression.
Common to both macro and micro gradual type systems is the concept of a
 @emph{type boundary} dividing typed and untyped @emph{components} of a program,
 across which arbitrary values may flow at run-time.
Whether boundaries appear only in the type checking environment or within
 type judgments is a matter of taste, but the success of a gradually typed
 language hinges on their correct and efficient implementation.

In the decade since gradual typing was first proposed, research groups have
 extended
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
   @item{@emph{Safety:} preserve the semantics of types at runtime}
   @item{@emph{Performance:} leverage type information in compiler optimizations}
 ]
Safety for gradual type systems is traditionally formulated as a
 type soundness theorem guaranteeing that typed program components are never
 blamed@~cite[wf-esop-2009] for run-time type errors.
In other words, typed code may raise a type error at run-time, but
 only as a consequence of receiving untyped data that did not match a
 static assumption made by the type system.
When this happens, the source of the type error is always traced back
 to the type boundary that produced it, thereby helping programmers
 debug the impedence mismatch.

Type soundness is enforced with dynamic assertions inserted at type boundaries.
Static types @exact|{$\RktMeta{T}$}| are compiled to casts and coercions
 @exact|{$\ctc{\RktMeta{T}}$}| that guarantee the run-time behavior of an
 untyped program component matches the component's static type@~cite[aft-dls-2013 TypedRacket sw-popl-2010].
For example, if the type checker assumes that an untyped function has type
 @racket[(Int -> Int)] then every value returned by the function at run-time will
 be dynamically checked against specification @exact|{$\ctc{\RktMeta{Int}}$}|.
Dynamic checks, however, introduce performance overhead.
Indeed, slowdowns of
 4x@~cite[tfdffthf-ecoop-2015],
 10x@~cite[vksb-dls-2014],
 and
 72x@~cite[rsfbv-popl-2015]
 have been reported in the literature.
@; Allende (DLS'13) dont seem to report a slowdown. Just,
@;  "as type annotations are added to a library, performance tends to degrade"

These preliminary figures imply a steep tradeoff between preserving type
 soundness and maintaining performance when adding types to an untyped program.
The aim of this paper is to provide a scientific foundation for measuring
 and understanding the tradeoff.


@section{Contributions}

This paper introduces a method for evaluating the performance of a gradual type
 system.
Given an untyped program and a fixed type assigment for all components in the
 program, the method considers the performance of each @emph{configuration}
 obtained by typing a subset of the components.
We apply our framework to Typed Racket, in which the program components are
 modules.
Hence a program with @exact{$N$} modules has @exact{$2^N$} configurations.
The evaluation affirms that Typed Racket programs may suffer
 order-of-magnitude overhead, but also suggests concrete improvements to the language
 and quantifies the effect of implementing these and other improvements.

The method was originally presented in a conference publication@~cite[tfgnvf-popl-2016].
We extend that prior work with:
@itemlist[
  @item{
    A comparitive analysis of three versions of Typed Racket,
     using the method to measure differences between versions.
  }
  @item{
    @id[(string-titlecase (integer->word NUM-NEW-OO))]
     object-oriented benchmark programs, augmenting our previous suite of
     @id[(- NUM-BENCHMARKS NUM-NEW-OO)] mostly-functional benchmark programs.
  }
  @item{
    In-depth discussions of performance bottlenecks in each benchmark and,
     where applicable, their resolution.
  }
  @item{
    Preliminary reports on a method for predicting the performance overhead for
     any of a program's @exact{$2^N$} configurations after taking @exact{$O(N)$}
     measurements.
  }
]

