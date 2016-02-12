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
 @; TODO not the best example
 social networking websites @todo{cite} @;@~cite[(in-bib TODO)]
 to pension systems @todo{cite} @;@~cite[(in-bib pension)].
In many cases, the systems start as innocent prototypes.
Soon enough, though, small programs grow into complex,
 multi-module programs, at which point the engineers realize that they are
 facing a maintenance nightmare, mostly due to the lack of reliable type
 information.

@; 1. In our opinion, benefits of static + dynamic types
On one hand, dynamic languages offer unconstrained freedom to write
 reusable and flexible code.
The language will faithfully do anything the programmer instructs it to---as
 C.A.R. Hoare wrote in his @emph{Hints on Programming Language Design}, this
 willingness of computer hardware to "accept almost any sequence of instructions"
 is "the secret of the power, flexibility, and simplicity ... of computer
 hardware, and should therefore be cherished" @todo{cite}.
That being said, a static type system helps validate a design, quickly catches
 common errors, provides documentation, and enables new optimizations.
Sound type systems additionally protect against all type errors.
Working with a type system seems a small price to pay for such a powerful
 debugging aid.

@; 2. Enter GT
Gradual typing@~cite[st-sfp-2006 thf-dls-2006] proposes a language-based
 solution to resolve the tradeoffs of dynamic and static typing.
The idea is to extend the language so that programmers can incrementally equip
 programs with types.
This gives the language user freedom to @emph{locally} decide where the benefits
 of working with a type system outweigh the costs..

Many programming languages combine the benefits of static and dynamic typing;
 in the past five years, we have seen new gradual type systems for
 @todo{cite langs}.
Each system has diverse goals and capabilities;
 in fact, the term
 @emph{gradual typing} has become an umbrella term rather than a technical one,
 applied equally to dynamic languages where type annotations encourage unsafe
 compiler optimizations @todo{cite} and safe languages where
 omitted type annotations are compensated by runtime casts.
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
 ignore type safety and research languages with a type soundness guarantee
 do not systematically evaluate performance.
In fact, the few performance studies mentioned in the literature
 report order-of-magnitude slowdowns.

As a first step towards expressive, safe, and performant gradual typing,
 this article presents a framework for systematically evaluating the performance
 of a gradual type system.
Given a programming language @math{L}, the framework is suitable for comparing
 two different gradual type systems for @math{L} based on their performance
 characteristics.
In @Secref{sec:typed-racket} we apply the framework to Typed Racket, a
 mature implementation of macro-level gradual typing, and compare three
 different versions of the implementation.
This evaluation serves to evaluate our framework,
 identify serious performance issues,
 and demonstrate how recent changes to Typed Racket have
 significantly improved the performance of the system.

@section{Is Sound Gradual Typing Dead?}
An earlier conference version of this article presented our evaluation framework
 and evaluated Typed Racket version 6.2 @todo{cite}.
That paper made two claims:
@itemlist[
  @item{The performance cost of Typed Racket's gradual typing is not tolerable.}
  @item{If applying our framework to other gradual type system implementations
        yields similar results, then sound gradual typing is dead.}
]
We stand by these claims.
Even after the performance improvements discussed
 in Section @todo{ref}, freely mixing typed and untyped code remains impractical.
@;TODO impractical = ???
It remains to be seen whether a performant implementation of sound gradual
 typing is possible or how the gradual typing promise should be amended
 to help developers avoid pathological slowdowns.

@todo{road map?}
