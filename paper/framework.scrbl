#lang scribble/base

@require["common.rkt"]

@; -----------------------------------------------------------------------------
@title[#:tag "sec:fwk"]{Benchmarking Software Evolution}

The inspiration for our performance evaluation approach is due to Takikawa
 @|etal|@~cite[tfdffthf-ecoop-2015]. For their extension of functional
 Typed Racket to the object-oriented aspects of Racket, they use a
 lattice-style approach for their
 @italic{formative}@note{Takikawa @|etal| borrow this term from the
 education community@~cite[scriven-chapter-1967]. A @italic{formative
 evaluation} informs the design process. In contrast, a @italic{summative
 evaluation} assesses the outcome of an intervention.}  evaluation. By
 inspecting the entire lattice of typed/untyped configurations of a small
 game system, they are able to identify and then eliminate a major
 performance bottleneck from their implementation. Takikawa @|etal| make no
 attempt to use the lattice of their two benchmarks for any summative
 evaluation.

Let us articulate Takikawa @|etal|'s landscape from our
perspective: 
@itemlist[

@item{A (@italic{software system}) @italic{configuration} is a sequence of
 @math{n} modules.}

@item{Each module in a software system configuration is either typed or
 untyped.}

@item{A configuration @math{c_t} is greater than a configuration @math{c_u}
 (or equal) if @math{c_t} uses a typed module for every position in the
 sequence for which @math{c_u} uses a typed module.}

@item{@bold{Proposition} The collection of all configurations of length
 @math{n} forms a complete lattice of size @math{2^n}. The bottom element
 is the completely untyped configuration; the top element is the completely
 typed one.}
]
 We speak of a @italic{performance lattice} to describe this idea. 
 

Our contribution is to exploit the lattice-oriented approach to benchmarking
 for a @emph{summative} evaluation. To this end, we
 imagine software engineers who are considering the use of gradual typing
 for some system and consider what kind of question may influence their
 decision.  Based on this first step, we formulate a small number of
 parameterized, quantitative measures that capture possible answers to these
 questions.

When the configuration consists of a small number of modules, the software engineers
 might be able to equip the entire system with type annotations in one fell
 swoop. Such a fully annotated system should perform as well as
 the original, untyped version---and if the gradual type system is
 integrated with the compiler, it may even run faster because the compiler
 can apply standard type-based optimization techniques.

@def[#:term "typed/untyped ratio"]{The typed/untyped ratio of a performance
 lattice is the time needed to run the top configuration divided by the
 time needed to run the bottom configuration.}

Unfortunately, this assumption overlooks the realities of implementations
 of gradual typing. Some modules simply cannot be equipped with types
 because they use linguistic constructs that cannot be
 annotated. Furthermore, completely typed configurations still use the
 run-time libraries of the underlying untyped language and, in particular
 Typed Racket's run-time system remains largely untyped. As a result, even
 the completely typed configurations of our benchmarks usually import
 constants, functions, and classes from an untyped module in the run-time
 system. When these values cross this boundary at run-time, the contract
 system performs checks, and that imposes additional costs. To address this
 issue, the kmplementors of Typed Racket have enlarged their trusted code
 base with unchecked type environments that cover frequently imported parts
 of the run-time system. The next
 section explains what ``completely typed'' means for the individual
 benchmarks. 

When the software system configuration consists of a reasonably large
 number of modules, no software engineering team can annotate the entire
 system with types all at once. Every effort is bound to leave the configuration
 in a state where some modules are typed and some others are untyped. As a
 result, the configuration is likely to suffer from the software contracts
 that the gradual type system injects at the boundary between the typed and
 the untyped portion. If the cost is tolerable, the configuration can be
 released and can replace the currently deployed version. The
 projects working on macro-level approaches to gradual typing seem to
 assume this kind of mode of operation. The run-time costs may not be tolerable,
 however, as Takikawa @|etal| observe. In that case, the question is how
 much more work the software engineers have to invest to reach a releasable
 configuration. That is, how many more modules must be converted before the
 performance is good enough for the new configuration to replace the
 running one.

To capture this idea, we formulate the following definition of
 ``deliverable configurations.''

@def[#:term @list{@deliverable{N}}]{A configuration in a performance
 lattice is @deliverable{N} if its performance is no worse than a N% slowdown
 compared to the completely untyped configuration.}

 We parameterize this definition over the slowdown percentage that a team
 may consider acceptable. One team may think of a 10% slowdown as barely
 acceptable, while another one may tolerate a slowdown of an order of
 magnitude@~cite[tfdffthf-ecoop-2015]. The performance evaluations below
 show @deliverable{N} for a range of values of @math{N} for all of our
 performance lattices. 

Even if a configuration is not deliverable, it might be suitably fast to
 run the test suites and the stress tests. A software engineering team can
 use such a configuration for development purposes, but it may not
 deliver it. The question is how many configurations of a performance
 lattice are usable in that sense. In order to formulate this criteria
 properly, we introduce the following definition of usable configurations. 

@def[#:term @list{@usable["N" "M"]}]{A configuration in a performance
 lattice is @usable["N" "M"] if its performance is worse than a
 N% slowdown and no worse than an M% slowdown compared to
 the completely untyped configuration.}

Like the preceding definition, this one is also parametrized. Using the
 first parameter, we exclude deliverable configurations from the count. The
 second parameter specifies the positive boundary, that is, the acceptable
 slowdown percentage for a usable configuration. The performance
 evaluations below report @usable["N" "M"] for two values of @math{M} per
 value of @math{N}.

@def[#:term "unacceptable"]{For any choice of @math{N} and @math{M}, a
 configuration is unacceptable if it is neither @deliverable{N} nor
 @usable["N" "M"].}

Finally, we can also ask the question how much work a team has to invest to
 turn unacceptable configurations into useful or even deliverable
 configurations. In the context of macro-level gradual typing, one easy way
 to measure this amount of work is to count the number of modules that have
 to be annotated with types before the resulting configuration becomes
 usable or deliverable. Here is the precise definition. 

@def[#:term @list{@step["L" "N" "M"]}]{A configuration is
 @step["L" "N" "M"] if it is unacceptable and at most @math{L} type
 conversion steps away from a @deliverable{N} or a @usable["N" "M"]
 configuration.}

Again, the performance reports below varies the @math{L} parameter.

This paper thus proposes an evaluation method based on an exhaustive exploration of
the performance lattice. The benefit of parameterized definitions is that
the same method applies in different contexts each with
its own performance envelope.

                                                  

                                    
