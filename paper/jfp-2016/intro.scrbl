#lang scribble/base

@require["common.rkt"]

@;@title[#:tag "sec:intro"]{Gradual Typing and Performance}
@;@title[#:tag "sec:intro"]{Types of Untyped Languages}
@title[#:tag "sec:intro"]{The Gradual Typing Design Space}

@; 0. Programmers use dynamic languages
Dynamically-typed languages have become a staple of the software
 engineering world.
Programmers use these languages to build all kinds of software, from
 from social networking websites
 to pension systems @todo{cite}.
In many cases, the systems start as innocent prototypes.
Soon enough, though, small programs grow into complex,
 multi-module programs, and software maintenance becomes a bottleneck.

@; 1. In our opinion, benefits of static + dynamic types
On one hand, dynamic languages offer unconstrained freedom to write
 reusable and flexible code.
The language will faithfully do nearly anything the programmer instructs it to---as
 C.A.R. Hoare wrote in his @emph{Hints on Programming Language Design}, this
 willingness of computer hardware to "accept almost any sequence of instructions"
 is "the secret of the power, flexibility, and simplicity ... of computer
 hardware, and should therefore be cherished" @todo{cite}.
That being said, a static type system guarantees protection from so-called
 type errors, provides a form of documentation via type signatures, and
 enables type-driven compiler optimizations @todo{cite}.
Given these benefits, needing to satisfy a type checker by giving some annotations
 or datatypes seems a small price to pay for the maintenance
 benefits.
@; Type systems also useful for writing new code, though popular
@;  opinion seems to like untyped better.

@; 2. Enter GT
Gradual typing@~cite[st-sfp-2006 thf-dls-2006] proposes a language-based
 solution to resolve the tradeoffs between dynamic and static typing.
The idea is to extend the language so that programmers can incrementally equip
 programs with types.
This gives the language user freedom to decide where the benefits
 of working with a type system outweigh the costs.

Many programming languages combine the benefits of static and dynamic typing;
 in the past five years, we have seen new ``gradual'' type systems for
 @todo{cite langs}.
Each system has unique goals and capabilities.
In @Secref{sec:flavors} we survey this research and implementation landscape,
 but despite the variety, there are three equally-important criteria for
 evaluating any typed/untyped@note{We frequently use
   ``typed'' and ``untyped'' as shorthand for ``statically typed'' and ``dynamically typed''.
   To be clear: our ``untyped'' code is memory-safe and will catch type errors at runtime.}
 language.
 @itemlist[
   @item{@emph{Expressiveness:} how many untyped features can the type system
         validate?}
   @item{@emph{Safety:} are the semantics of types preserved at runtime?}
   @item{@emph{Performance:} how do typed/untyped programs run, relative to
         the untyped baseline?}
 ]
To date, there is no language that excels along all three dimensions.
Therefore one would expect that expressiveness, safety, and performance
 are treated equally in the development of new gradually typed languages.
We have found, however, that commercial implementations of gradual typing
 ignore type safety and research languages with type soundness guarantees
 do not systematically evaluate performance.
Moreover, the few performance studies mentioned in the literature
 report order-of-magnitude slowdowns.

As a first step towards expressive, safe, and performant gradual typing, this
 article motivates (@Secref{sec:story}) and presents (@Secref{sec:framework})
 a framework for systematically evaluating the performance
 of a gradual type system.
Given a programming language @math{L}, the framework is suitable for comparing
 two different gradual type systems for @math{L} based on their performance
 characteristics.
In @Secref{sec:typed-racket} we apply the framework to Typed Racket, a
 mature implementation of macro-level gradual typing, and compare three
 versions of the implementation.
This comparison serves to evaluate our framework,
 identify serious performance issues (@Secref{sec:aftermath}),
 and demonstrate how recent changes to Typed Racket have
 significantly improved the performance of the system (@todo{secref}).
We conclude by assessing weaknesses of our framework in @Secref{sec:death}
 and suggesting future applications in @Secref{sec:conclusion}.

@section{So, is Sound Gradual Typing Dead?}
An earlier conference version of this article presented our evaluation framework
 and evaluated Typed Racket v6.2 @todo{cite}.
That paper made two claims regarding the evaluation:
@itemlist[
  @item{The performance cost of Typed Racket's gradual typing is not tolerable.}
  @item{If applying our framework to other gradual type system implementations
        yields similar results, then sound gradual typing is dead.}
]
We stand by these claims.
Even after the performance improvements discussed
 in @todo{secref}, freely mixing typed and untyped code remains impractical.
It remains to be seen whether a performant implementation of sound gradual
 typing is possible, or how the gradual typing promise should be amended
 to help developers avoid order-of-magnitude slowdowns.

