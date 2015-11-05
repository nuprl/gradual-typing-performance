#lang scribble/base

@require["common.rkt"]

@; -----------------------------------------------------------------------------
@title[#:tag "sec:fwk"]{Benchmarking Software Evolution}

Our evaluation method is inspired by our previous work on extending functional
 Typed Racket to the object-oriented aspects of Racket, in which we
 use a lattice-style approach for a preliminary performance evaluation@~cite[tfdffthf-ecoop-2015].
 By inspecting the entire lattices of typed/untyped configurations of two small
 game systems, we identified and then eliminated a major
 performance bottleneck from the implementation.
 Our previous performance evaluation was conducted in tandem with the
 design and implementation of Typed Racket, and thus the final results were
 relatively positive.
 In contrast, we conduct our current evaluation completely @emph{independently} of
 Typed Racket's implementation efforts.@note{In terminology
 borrowed from the
 education community@~cite[scriven-chapter-1967], we conducted a @italic{formative
 evaluation} while this paper conducts a @italic{summative
 evaluation} to assess the post-intervention state of the system.}

Let us re-articulate the salient points from our previous work:
@itemlist[

@item{A (@italic{software system}) @italic{configuration} is a sequence of
 @math{n} modules.}

@item{Each module in a software system configuration is either typed or
 untyped.}

@item{A configuration @math{c_t} is greater than a configuration @math{c_u}
 (or equal) if @math{c_t} uses a typed module for every position in the
 sequence for which @math{c_u} uses a typed module.}

@item{The collection of all configurations of length
 @math{n} forms a complete lattice of size @math{2^n}. The bottom element
 is the completely untyped configuration; the top element is the completely
 typed one.}
]
 We speak of a @italic{performance lattice} to describe this idea. 
 

Our contribution is to exploit the lattice-oriented approach to benchmarking
 for a @emph{summative} evaluation. To this end, we
 imagine software engineers who are considering the use of gradual typing
 for some program and consider what kinds of questions may influence their
 decision.  Based on this first step, we formulate a small number of
 parameterized, quantitative measures that capture possible answers to these
 questions.

When the configuration consists of a small number of modules, the software engineers
 might be able to equip the entire program with type annotations in one fell
 swoop. Such a fully annotated system should perform as well as
 the original, untyped version---and if the gradual type system is
 integrated with the compiler, it may even run faster because the compiler
 can apply standard type-based optimization techniques.

@def[#:term "typed/untyped ratio"]{The typed/untyped ratio of a performance
 lattice is the time needed to run the top configuration divided by the
 time needed to run the bottom configuration.}

Unfortunately, this assumption overlooks the realities of implementations
 of gradual typing. Some modules simply cannot be equipped with types
 because they use linguistic constructs that the type system does not support.
 Furthermore, completely typed configurations still use the
 run-time libraries of the underlying untyped language. In particular,
 Typed Racket's run-time system remains largely untyped. As a result, even
 the completely typed configurations of our benchmarks usually import
 constants, functions, and classes from an untyped module in the run-time
 system. When these values cross this boundary at run-time, the contract
 system performs checks, and that imposes additional costs. To address this
 issue, the implementors of Typed Racket have enlarged their trusted code
 base with unchecked type environments that cover frequently imported parts
 of the run-time system. The next
 section explains what ``completely typed'' means for the individual
 benchmarks. 

When the software system configuration consists of a reasonably large
 number of modules, no software engineering team can annotate the entire
 system with types all at once. Every effort is bound to leave the configuration
 in a state in which some modules are typed and some others are untyped. As a
 result, the configuration is likely to suffer from the software contracts
 that the gradual type system injects at the boundaries between the typed and
 the untyped portions. If the cost is tolerable, the configuration can be
 released and can replace the currently deployed version. 
 The run-time costs may not be tolerable,
 however, as our previous work observes. In that case, the question is how
 much more effort the software engineers have to invest to reach a releasable
 configuration. That is, how many more modules must be converted before the
 performance is good enough for the new configuration to replace the
 running one.

To capture this idea, we formulate the following definition of
 ``deliverable configurations.''

@def[#:term @list{@deliverable{N}}]{A configuration in a performance
 lattice is @deliverable{N} if its performance is no worse than an @math{N}x slowdown
 compared to the completely untyped configuration.}

 We parameterize this definition over the slowdown factor that a team
 may consider acceptable. One team may think of a 1.1x slowdown as barely
 acceptable, while another one may tolerate a slowdown of an order of
 magnitude@~cite[tfdffthf-ecoop-2015].

Even if a configuration is not deliverable, it might be suitably fast to
 run the test suites and the stress tests. A software engineering team can
 use such a configuration for development purposes, but it may not
 deliver it. The question is how many configurations of a performance
 lattice are usable in that sense. In order to formulate this criteria
 properly, we introduce the following definition of usable configurations. 

@def[#:term @list{@usable["N" "M"]}]{A configuration in a performance
 lattice is @usable["N" "M"] if its performance is worse than an
 @math{N}x slowdown and no worse than an @math{M}x slowdown compared to
 the completely untyped configuration.}

Using the first parameter, we exclude deliverable configurations from the count. The
 second parameter specifies the positive boundary, i.e., the acceptable
 slowdown factor for a usable configuration.

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

This paper thus proposes an evaluation method based on a systematic exploration of
the performance lattice. The benefit of parameterized metrics is that
every reader can interpret the raw data with his or her own choices for
@math{L}, @math{N}, and @math{M}.

                                                  

                                    
