#lang scribble/base

@; 2016-04-15
@; Too staccato! Too dry! But otherwise ok and accurate.

@;@title[#:tag "sec:intro"]{Gradual Typing and Performance}
@;@title[#:tag "sec:intro"]{Types of Untyped Languages}
@title[#:tag "sec:intro"]{The Gradual Typing Design Space}

@; 0. Programmers use dynamic languages
Dynamically-typed languages have become a staple of the software
 engineering world.
Programmers use these languages to build all kinds of software, from
 data analysis libraries
 to social networking websites
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
Given these benefits, having to annotate some programs or redesign others
 to satisfy a type checker seems a small price to pay for the maintenance
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
 in the past five years, we have seen new gradual type systems for
 @todo{cite langs}.
Each system has diverse goals and capabilities;
 in fact, the term
 @emph{gradual typing} has become an umbrella term rather than a technical one,
 applied equally to dynamic languages where type hints are used as an excuse
 for unsafe compiler optimizations and theoretical calculi with correctness
 theorems about the interaction of typed and untyped@note{We frequently use
 ``typed'' and ``untyped'' as shorthand for ``statically typed'' and ``dynamically typed''.
 To be clear: our ``untyped'' code is memory-safe and will catch type errors at runtime.}
 components@todo{cite}
@; fair to compare industry / calculi and deduce "umbrella"?
@Secref{sec:flavors} surveys this research and implementation landscape.
Despite the variety, there are three equally-important criteria for
 evaluating any gradual type system:
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
In fact, the few performance studies mentioned in the literature
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
 in @Secref{sec:aftermath}, freely mixing typed and untyped code remains impractical.
It remains to be seen whether a performant implementation of sound gradual
 typing is possible or how the gradual typing promise should be amended
 to help developers avoid order-of-magnitude slowdowns.

