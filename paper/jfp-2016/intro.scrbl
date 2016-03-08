#lang scribble/base

@require["common.rkt" "typed-racket.rkt"]

@title[#:tag "sec:intro"]{The Gradual Typing Design Space}

@; Programmers use dynamic languages
Dynamically-typed languages have become a staple of the software
 engineering world.
Programmers use these languages to build all kinds of software,
 from social networking websites@note{@url["http://www.artima.com/scalazine/articles/twitter_on_scala.html"]}
 to pension systems @~cite[v-aplwa-2010].
In many cases, the systems start as innocent prototypes in which the flexibility
 of dynamic typing speeds development.
Soon enough, though, small programs grow into complex,
 multi-module programs, and software maintenance becomes a bottleneck.
When this happens, the assurance provided by a static type system becomes
 increasingly valuable.

@; Enter GT
Gradual typing@~cite[st-sfp-2006] proposes a language-based
 solution to resolve the tradeoffs between dynamic and static typing.
The idea is to extend an existing language to allow the incremental
 addition of type annotations.
This gives the programmer fine-grained control to decide when the benefits
 of working with a type system outweigh the costs.
Exactly where the annotations may be placed depends on the type system:
 @emph{macro}-level gradual typing@~cite[thf-dls-2006]
 requires each module to be either typed or untyped
 whereas @emph{micro}-level gradual typing can be toggled for any expression@~cite[st-sfp-2006].

In the decade since gradual typing was first proposed, research groups have
 extended
 JavaScript@~cite[rnv-ecoop-2015 rsfbv-popl-2015],
 Python@~cite[vksb-dls-2014],
 Racket@~cite[TypedRacket], and
 @; Ruby@~cite[furr-dissertation-2009],
 Smalltalk@~cite[acftd-scp-2013]
 with gradual type systems.
Each new extension must address the unique challenges of its base language,
 but in general these gradual type systems have three main goals:
 @itemlist[
   @item{@emph{Expressiveness:} assign useful types to untyped features}
   @item{@emph{Safety:} preserve the semantics of types at runtime}
   @item{@emph{Performance:} leverage type information in compiler optimizations}
 ]
To date, no implementation excels along all dimensions.
Although much work has been done to safely express dynamic features like
 continuation-passing@~cite[tsth-esop-2013],
 object identity checks@~cite[vksb-dls-2014], and
 run-time evaluation@~cite[sfrbcsb-popl-2014],
 safety is typically guaranteed through the use of dynamic casts and type checks.
These casts and checks affect performance, sometimes drastically.
Slowdowns of 4x@~cite[tfdffthf-ecoop-2015], 10x@~cite[vksb-dls-2014], and 72x@~cite[rsfbv-popl-2015] have been reported in the literature.
@todo{allende}

Despite evidence of the tradeoff between safe, expressive gradual typing
 and run-time performance, there
 has not been a comprehensive performance evaluation of any gradual type system.
Part of the difficulty is in quantifying peformance.
Runtime type checks seem unavoidable in this setting, but whether a 2x or 20x
 slowdown is acceptable varies between projects and development teams.
Another question is how to summarize the many ways of adding types to a program.
Worst-case and average-case measures tend to be overly pessimistic.

This paper introduces a framework for measuring the performance of a gradual
 type system.
The framework is based on exhaustively testing all gradually-typed configurations
 obtained by enabling or disabling types in a program.
Overall performance is reported as the @emph{proportion} of gradual configuartions
 that run within a given overhead factor.
Our target audience is language designers.
The method has proven useful for identifying bottlenecks in the implementation
 of Typed Racket and measuring net improvements as these bottlenecks are fixed.
We encourage implementors of other gradual type systems to apply our framework
 and report its shortcomings.


@section{So, is Sound Gradual Typing Dead?}
This paper is an extended version of an earlier conference
 publication which presented our evaluation framework
 and applied it to Typed Racket@~cite[tfgnvf-popl-2016].
Relative to the conference version, this paper:
@itemlist[
  @item{Compares three versions of Typed Racket.}
  @item{Adds @id[NUM-NEW-OO] object-oriented benchmark programs to our previous suite of
        @id[(- NUM-BENCHMARKS NUM-NEW-OO)] mostly-functional benchmark programs.}
  @item{Gives in-depth discussions of performance bottlenecks in each program and,
        where applicable, their resolution.}
  @item{Describes a technique for predicting the
        performance characteristics of a fully-typed program's gradually-typed
        configurations.}
]
The essential contribution of this paper, as with the last, is our
 @emph{framework} for evaluating the performance of a gradual type system.
That being said, we stand by our earlier claim that ``if applying our framework
 to other gradual type system implementations yields similar results, then
 sound gradual typing is dead.''@~cite[tfgnvf-popl-2016].
Until we find an implementation technique that mitigates the performance
 cost of mixing typed and untyped code, gradual typing has failed to meet
 its practical motivations.


