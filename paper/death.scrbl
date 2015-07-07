#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:death"]{Sound Gradual Typing is Dead}

Unsound type systems are useful. They find bugs at compile-time, and an IDE can use them to
 assist programmers. Sound type systems are meaningful in addition. A
 soundly typed program can't go wrong, up to a well-defined set of run-time
 exceptions@~cite[type-soundness]. When a typed program raises an
 exception, the exception message can (usually) pinpoint the location of
 the problem in the program source. Hence sound types are one of the best
 forms of documentation and specification around. In the context of a sound
 type system, the type of a well-named function or method often explains
 (almost) as much as an inspection of their code will do.

From this description, it is clear why programmers eventually wish to
 annotate programs in untyped languages with types and, ideally, with sound
 types. Types increase a programmer's productivity, and sound types greatly
 help with testing, debugging, and other bug-related maintenance tasks.
 Hence sound gradual typing seems to be such a panacea. The problem is,
 however, that the cost of enforcing soundness appears to be overwhelming
 according to our measurements. 

In general, the graphs in @figure-ref["fig:lnm1" "fig:lnm2"] clarify how
 few partially typed configurations are usable by developers or deliverable
 to customers. For almost all benchmarks, the lines are below the (red)
 horizontal line of acceptability. Even with extremely liberal settings for
 @math{N} and @math{M}, few configurations are @math{N}-deliverable or
 @math{N/M}-usable. Worse, investing more effort into type annotation does
 not seem to pay off. In practice, converting a module takes a good portion
 of a workday, meaning that setting @math{L} to @math{2} is again a liberal
 choice. But even allowing the conversion of two additional modules does
 not increase the number of acceptable configurations by much. Put
 differently, the number of @math{L}-step @math{N/M}-acceptable
 configurations remains small with liberal choices for all three parameters.

Like Marc Antony, we come here to bury sound gradual typing, not to praise
 it. Sound gradual typing is dead.

@; -----------------------------------------------------------------------------
@section{Threats to Validity of Conclusion}

Our judgment is harsh and fails to acknowledge potential weaknesses in our
 evaluation framework and in our results.

First, our benchmarks are relatively small. The two largest ones consist of
 13 and 16 modules, respectively. Even these benchmarks pose serious
 challenges to our computing infrastructure because they require timing
 @math{2^13} and @math{2^16} configurations @math{30} times each.
 Running experiments with modules that consist of many
 more modules would be impractical. At the same time, our
 experience suggests that the results get worse as programs get larger in
 terms of number of modules. 

Second, many of our benchmarks import some modules from Racket's suite of
 libraries that remain untyped throughout the process, including for the
 fully typed configuration. While some of these run-time libraries come in
 the trusted code base---meaning Typed Racket knows their types and the
 types are not compiled to contracts---others are third-party libraries
 that impose a cost on all configurations. In principle, these interfaces
 might substantially contribute to the running-time overhead of
 partially typed configurations. But, given the low typed/untyped ratios, 
 these libraries are unlikely to affect our conclusions.

Third, our framework images a particularly @emph{free} approach to
 annotating programs with types. With ``free'' we mean that we do not expect
 software engineers to add types to modules in any particular
 order. Although this freedom is representative of some kind of maintenance
 work---add types when bugs are repaired and only then---a team may decide
 to add types to an entire project in a focused approach. In this case, they
 may come up with a particular kind of plan that avoids all of these
 performance traps. Roughly speaking, such a plan would correspond to a
 specific path from the bottom element of the performance lattice to the top
 element.  Sadly our current measurements suggest that almost all
 bottom-to-top paths in our performance lattices suffer go through
 performance bottlenecks.  As the  @tt{suffixtree} example
 demonstrates, a path-based approach depends very much on the structure of
 the module graph.  We therefore conjecture that some of the ideas offered
 in the conclusion section may help such planned, path-based approaches.

Fourth, we state our judgment with respect to the current implementation
 technology. Typed Racket compiles to Racket, which uses rather conventional
 compilation technology. It makes no attempt to reduce the overhead of
 contracts or to exploit contracts for optimizations. It remains to be seen
 whether contract-aware compilers or less-expressive gradual typing systems
 can reduce the significant overhead that our evaluation
 shows. Nevertheless, we are convinced that even if the magnitude of the
 slowdowns are reduced, some pathologies will remain.

Fifth, running all configurations of the benchmarks can take a very long
 wall-clock time. To make the experiment feasible, we run the larger
 benchmarks using multiple cores and divide up the configurations amongst
 the cores. Each configuration is run in a single process running a separate
 instance of the Racket VM pinned to a single core. However, this
 parallelism may introduce confounding variables due to, for example, shared
 caches or main memory. We have attempted to control for this case and as
 far as we can tell, executing on an unloaded machine does not make a
 significant difference to our results.  We believe that this is due to the 
 magnitude of the performance differentials between configurations.
