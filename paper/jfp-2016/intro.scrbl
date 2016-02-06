#lang scribble/base

@; 2016-04-15
@; Too staccato! Too dry! But otherwise ok and accurate.

@;@title[#:tag "sec:intro"]{Gradual Typing and Performance}
@;@title[#:tag "sec:intro"]{Types of Untyped Languages}
@title[#:tag "sec:intro"]{The Gradual Typing Design Space}

Dynamically-typed languages have become a
 staple of the software engineering world.
Programmers use these languages to build all kinds of software, from
 @; TODO not the best example
 social networking websites @todo{cite} @;@~cite[(in-bib TODO)]
 to pension systems @todo{cite} @;@~cite[(in-bib pension)].
In many cases, the systems start as innocent prototypes.
Soon enough, though, they grow into complex,
 multi-module programs, at which point the engineers realize that they are
 facing a maintenance nightmare, mostly due to the lack of reliable type
 information.
@todo{nightmare?}

Gradual typing@~cite[st-sfp-2006 thf-dls-2006] proposes a language-based
 solution to this pressing software engineering problem.
 @todo{specific problem}
The idea is to extend the language so that programmers can incrementally equip
 programs with types.
@; How to mediate the development benefits of dynamics and the maintenance benefits of types
@; 'Course not this clear-cut,
@; - types great for dev, catch many typos
@; - untypes frequently better for highly reusable & concise code
 @todo{fix}
This gives the language user freedom to @emph{locally} decide where the benefits
 of having a type system outweigh the tradeoffs.
@todo{impedence mismatch}

Many programming languages combine the benefits of static and dynamic typing;
 in the past five years, we have seen new gradual type systems for
 @todo{langs}.
The systems have diverse goals and capabilities; we provide a survey in
 @secref{sec:flavors}.
In fact, the term
 @emph{gradual typing} has become an umbrella term rather than a technical one.
Nonetheless, there are three main criterion for evaluating gradual type systems:
 @itemlist[
   @item{@emph{Expressiveness:} how many untyped features can the type system
         validate?}
   @item{@emph{Soundness:} are the semantics of types preserved at runtime?}
   @item{@emph{Performance:} how do typed/untyped programs run, relative to
         the untyped baseline?}
 ]
In the context of sound gradual typing, the first two evaluation criteria
 are relatively well-studied.
 @; Note on expressiveness: we specifically did NOT ask i.e. "dep. types?"
 @; That's an indirect answer to the core question of "how many 'correct'
 @;  untyped lambda terms does the system validate / understand
The third has seen little attention; however, anecdotal evidence indicates
 that slowdowns from 2x to 10x are common.
 @todo{anecdote vs evaluation}.

This article presents a framework for systematically evaluating the performance
 of a gradual type system.
Given a programming language @math{L}, the method is suitable for comparing two
 different gradual type systems for @math{L} based on their performance
 characteristics.
In @Secref{sec:typed-racket} we apply the framework to Typed Racket, a
 mature implementation of macro-level gradual typing, and compare three
 different versions of the implementation.
This evaluation serves a three-way purpose of evaluating our framework,
 identifying serious performance issues with the current implementation of
 Typed Racket, and demonstrating that recent changes to Typed Racket have
 significantly improved.

@section{Is Sound Gradual Typing Dead?}
An earlier conference version of this article presented our evaluation framework
 and evaluated Typed Racket version 6.2 @todo{cite}.
That paper made two bold claims:
@itemlist[
  @item{The performance cost of Typed Racket's gradual typing is not tolerable.}
  @item{If applying our framework to other gradual type system implementations
        yields similar results, then sound gradual typing is dead.}
]
We stand by these claims.
It remains to be seen whether a performant implementation of sound gradual
 typing is possible.
Until then, these systems fail to solve the original software maintenance
 problem.

