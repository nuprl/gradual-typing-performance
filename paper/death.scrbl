#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:death"]{Sound Gradual Typing is Dead}

Type soundness has meaning. A typed program can't go wrong up to a
 well-defined set of run-time exceptions. When a typed program raises an
 exception, the exception message can (usually) pinpoint the location of
 the problem in the program source. As a result, types are also one of the
 best forms of documentation and specification around. In the context of a
 sound type system, the type of a well-named function or method often
 explains (almost) as much as an inspection of their code will
 do.@note{Regardless of soundness, type systems have other advantages. We
 ignore those here.}

From this description, it is clear why we wish to equip programs in untyped
 languages with types. When programmers maintain such systems, they wish to
 benefit from the advantages, too. Hence sound gradual typing seems to be
 such a panacea at first. The problem is, however, that the cost of
 enforcing soundness in hybrid configurations appears to be overwhelming,
 if the results of our measurements are representative. 

In general, the graphs in @figure-ref["fig:lnm1" "fig:lnm2"] clarify how
 few partially typed configurations are usable by developers or deliverable
 to customers. For almost all benchmarks, the lines are below the (red)
 horizontal line of acceptability. Even with extremely liberal
 settings for @math{N} and @math{M}, few configurations are
 @math{N}-deliverable or @math{N/M}-usable. Worse, investing more effort
 into type annotation does not seem to pay off much. In practice,
 converting a module takes a workday or more effort, meaning that setting
 @math{L} to @math{2} is again a liberal choice. But even allowing the
 conversion of two additional modules does not increase the number of
 acceptable configurations. Put differently, the number of @math{L}-step
 @math{N/M}-acceptable configurations remains small with liberal choices
 for the parameters. 

Like Brutus, we come here to bury sound gradual typing, not to praise
 it. Sound gradual typing is dead. 

@; -----------------------------------------------------------------------------
@section{Threats to Validity of Conclusion}

Our judgment is harsh and fails to acknowledge that our evaluation
 framework might suggest flawed conclusions. 

First, many of our benchmarks import some modules from Racket's suite of
 libraries that remain untyped throughout the process, including for the
 fully typed configuration. While some of these run-time libraries come in
 the trusted code base---meaning Typed Racket knows their types and the
 types are not compiled to contracts---especially third-party libraries
 impose a cost on all configurations. In principle, these interfaces might
 contribute to the large running-time overhead of the partially typed
 configurations of our benchmarks, but given the highly reasonable
 typed-untyped ratios, we consider this effect minor. 

Second, our framework images a particularly @emph{free} approach to
 annotating programs with types. With ``free'' we mean that we do not
 expect software engineers to add types to modules in any particular
 order. Although this freedom is representative of some kind of maintenance
 work---add types when bugs are repaired and only then---a team may decide
 to add types to an entire project all at once. In this case, they may come
 up with a particular kind of plan that avoids all of these performance
 traps. Roughly speaking, such a plan would correspond to a specific path
 from the bottom element of the performance lattice to the top element.  To
 determine whether such a path-based approach might have better chances
 than a free one, we will have to study the existence of performant paths
 through the lattice, not configurations.

As the @tt{suffixtree} example demonstrates, a path-based approach depends
 very much on the structure of the module graph.  We therefore conjecture
 that some of the ideas offered in the conclusion section may help such
 planned, path-based approaches.

Third, we state our judgment with respect to the current implementation
 technology. Typed Racket compiles to Racket, which uses rather
 conventional compilation technology. It makes no attempt to reduce the
 overhead of contracts or to exploit contracts for optimizations. It
 remains to be seen whether contract-aware compilers or less-expressive
 gradual typing systems can reduce the significant overhead that our
 evaluation shows. 
