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
 addition of static types.
This gives the programmer fine-grained control to decide when the benefits
 of working with a type system outweigh the costs.
Exactly where static typing may be enabled depends on the type system.
So-called @emph{macro}-level gradual typing@~cite[thf-dls-2006]
 gives programmers the choice of statically typing any module,
 whereas @emph{micro}-level gradual typing offers the same choice for any expression.

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
   @item{@emph{Expressiveness:} describe all untyped features with useful types}
   @item{@emph{Safety:} preserve the semantics of types at runtime}
   @item{@emph{Performance:} leverage type information in compiler optimizations}
 ]
To date, no implementation excels along all dimensions
 because the technology used to make gradual type systems safe also
 degrades performance.

Safety for gradual type systems is traditionally formulated as a
 type soundness theorem guaranteeing that typed program components are never
 blamed@~cite[wf-esop-2009] for run-time type errors.
In other words, typed code may raise a type error at run-time, but
 only after receiving bad input from untyped code.
When this happens, the source of the type error is always traced back
 to the untyped component that produced it, thereby helping programmers
 debug the impedence mismatch at a typed/untyped boundary.

Type soundness is enforced with
 behavioral software contracts@~cite[ff-icfp-2002].
At the boundaries between typed and untyped program components,
 static types are compiled to dynamic contracts; each contract ensures that
 the run-time behavior of an untyped program component matches the component's
 static type.
For example, whenever typed code calls an untyped function, the
 function's return value is checked against a statically-determined contract.

These checks, however, introduce performance overhead.
If the overhead were low this would not be an issue, but
 slowdowns of 4x@~cite[tfdffthf-ecoop-2015], 10x@~cite[vksb-dls-2014], and 72x@~cite[rsfbv-popl-2015] have been reported in the literature.
@; Allende (DLS'13) dont seem to report a slowdown. Just "as type annotations
@;  are added to a library, performance tends to degrade"
Clearly, there is a tradeoff between preserving type soundness and
 maintaining performance when adding types to an untyped program.

Despite the preliminary evidence that enforcing type soundness via contracts
 causes order-of-magnitude overhead in some programs, there
 has not been any detailed performance evaluation of an existing gradual type system.
Part of the difficulty is in quantifying peformance.
Whether a 10% or 2x slowdown is acceptable depends on the requirements of
 a software project or development team.
There is no authoritative measure.
Performance also varies drastically with the specific typed/untyped boundaries in a program.
Adding static types to remove a boundary can greatly improve performance.
Conversely, aggregate measures like taking the mean running time over all
 possible ways of gradually adding types tend to give overly pessimistic figures.

This paper introduces a method for evaluating the performance of a gradual
 type system.
The method is based on exhaustively measuring all gradually-typed
 @emph{configurations} of a program.
A configuration assign types to a subset of components in a program; we generate
 and test all such subsets.
Overall performance is reported as the @emph{proportion} of gradual configurations
 that run within an overhead @emph{parameter}.
Using a parameter captures the varying requirements of engineering teams.

Our target audience is language designers.
The method has proven useful for identifying bottlenecks in the implementation
 of Typed Racket and measuring net improvements as these bottlenecks are fixed.
We encourage implementors of other gradual type systems to apply our method
 and to report its shortcomings.


@section{So, is Sound Gradual Typing Dead?}
This paper is an extended version of an earlier conference
 publication which presented our evaluation method
 and applied it to Typed Racket@~cite[tfgnvf-popl-2016].
Relative to the conference version, this paper:
@itemlist[
  @item{
    Evaluates three versions of Typed Racket and quantifies the
     performance improvements in successive version.
  }
  @item{
    Adds @id[NUM-NEW-OO] object-oriented benchmark programs to our previous suite of
     @id[(- NUM-BENCHMARKS NUM-NEW-OO)] mostly-functional benchmark programs.
  }
  @item{
    Gives in-depth discussions of performance bottlenecks in each program and,
     where applicable, their resolution.
  }
  @item{
    Reports on a prediction method for estimating the performance of
     any configuration after testing a linear number of all configurations.
  }
]
The essential contribution of this paper is our
 @emph{method} for evaluating the performance of a gradual type system.
That being said, we stand by our earlier claim that ``if applying our method
 to other gradual type system implementations yields similar results, then
 sound gradual typing is dead.''@~cite[tfgnvf-popl-2016].
Given current implementation technologies, the runtime cost of mixing
 typed and untyped code renders gradual typing impractical.
