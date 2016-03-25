#lang scribble/base

@require["common.rkt" "typed-racket.rkt"]

@title[#:tag "sec:intro"]{The Gradual Typing Design Space}

@; Programmers use dynamic languages
Dynamically-typed languages have become a staple of the software
 engineering world.
Programmers use these languages to build all kinds of software,
 from social networking websites@note{@url["http://www.artima.com/scalazine/articles/twitter_on_scala.html"]}
 to pension systems @~cite[v-aplwa-2010].
@; TODO innocent -> something else?
@; ... systems initially benefit ...
In many cases, the systems start as innocent prototypes in which the flexibility
 of dynamic typing speeds development.
Soon enough, though, small programs grow into complex,
 multi-module programs, and software maintenance becomes a bottleneck.
When this happens, the assurance provided by a static type system becomes
 increasingly valuable.

@; Enter GT
Gradual typing@~cite[st-sfp-2006] proposes a language-based
 solution to resolve the tradeoffs between dynamic and static typing.
The idea is to extend an existing, dynamically-typed language to allow the incremental
 addition of static types.
In so-called @emph{macro}-level gradual type systems, an untyped program may
 be given static types one module at a time; in contrast, @emph{micro}-level
 gradual type systems allow the incremental conversion of any expression.
Common to both macro and micro gradual type systems is the concept of a
 @emph{type boundary} dividing typed and untyped code.
Whether boundaries appear only in the type checking environment or within
 type judgments is a matter of taste, but the success of a gradual type
 system hinges on the correct and efficient implementation of type boundaries.

In the decade since gradual typing was first proposed, research groups have
 extended
 JavaScript@~cite[rnv-ecoop-2015 rsfbv-popl-2015],
 Python@~cite[vksb-dls-2014],
 Racket@~cite[TypedRacket], and
 @; Ruby@~cite[furr-dissertation-2009],
 Smalltalk@~cite[acftd-scp-2013]
 with gradual type systems.
Each new extension must address the unique challenges of its base language,
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
 only after receiving untyped data that did not match an
 assumption made by the type system.
When this happens, the source of the type error is always traced back
 to the type boundary that produced it, thereby helping programmers
 debug the impedence mismatch.

Type soundness is enforced with dynamic assertions inserted at type boundaries.
Static types @exact{$\tau$} are compiled to run-time casts and coercions
 @exact{$\ctc{\tau}$} that guarantee the run-time behavior of an
 untyped program component matches the component's static type.
For example, if typed code assumes that an untyped function has type
 @racket[(Int -> Int)] then every call to the function will
 be dynamically checked against specification @exact|{$\ctc{\RktMeta{Int}}$}|.

These dynamic checks, however, introduce performance overhead.
Slowdowns of 4x@~cite[tfdffthf-ecoop-2015], 10x@~cite[vksb-dls-2014], and 72x@~cite[rsfbv-popl-2015] have been reported in the literature.
@; Allende (DLS'13) dont seem to report a slowdown. Just "as type annotations
@;  are added to a library, performance tends to degrade"
Clearly, there is a tradeoff between preserving type soundness and
 maintaining performance when adding types to an untyped program.

Despite the preliminary evidence that all sound gradual type systems
 suffer order-of-magnitude overhead in some programs, there
 has not been a detailed performance evaluation of any existing gradual type system.
Part of the difficulty is in quantifying peformance.
Whether a 10% or 2x slowdown is acceptable depends on the requirements of
 a software project or development team.
Performance can also vary drastically with the specific type boundaries in a
 program.
Adding static types to remove a single type boundary can greatly improve performance
 if the boundary is crossed frequently.

This paper introduces a method for evaluating the performance of a gradual
 type system.
The method is based on exhaustively measuring all gradually-typed
 @emph{configurations} of a program.
A configuration assign types to a subset of components in a program; we generate
 and test all such subsets.
Overall performance is reported as the @emph{proportion} of gradual configurations
 that run within an overhead @emph{parameter}.
@; Using a proportion ???
Using a parameter captures the varying requirements of engineering teams.

Our target audience is language designers who build gradual type systems or
 their supporting runtime systems.
The method has proven useful for identifying bottlenecks in the implementation
 of Typed Racket and measuring net improvements as these inefficiencies are fixed.
We encourage implementors of other gradual type systems to apply our method
 and to report its shortcomings.


@section{Contributions}
Our essential contribution is a method for evaluating the
 performance of a gradual type system.
Additionally, we present:
@itemlist[
  @item{
    A comparitive analysis of three versions of Typed Racket,
     quantifying the performance improvements in each successive version.
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
    Preliminary reports on a method for predicting performance overhead.
    After testing a linear number of all gradual configurations, we estimate
     performance for any combination of typed modules in a program.
  }
]

This paper extends an earlier conference
 publication@~cite[tfgnvf-popl-2016] which presented our evaluation method
 and applied it to Typed Racket v6.2.
The above bullet points are novel to this work.
