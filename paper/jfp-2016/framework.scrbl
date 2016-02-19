#lang scribble/base

@; TODO lattice
@;@figure["fig:fsm-lattice" "FSM performance lattice (labels are speedup/slowdown factors)"
@;  @(let ([vec (file->value "src/fsm-lattice-data.rktd")])
@;     (make-performance-lattice vec))
@;]

@require["common.rkt"]

@; -----------------------------------------------------------------------------
@title[#:tag "sec:frameworkfwk"]{Evaluation Framework}

@; @section{Criteria for Performance Evaluation}
Our experience with Typed Racket demonstrates two essential points:
  @itemlist[
    @item{
      Racket programmers are not seeking "ideal" conversion paths
       from untyped to typed.
      More often, they create a mixed typed/untyped program through
       some combination of existing code, necessity, and convenience.
    }
    @item{
      Performance can fluctuate wildly depending on what subset of modules
       are typed due to the complex interaction between modules.
    }
  ]
These observations convince us that performance evaluation for gradual type
 systems needs to consider @emph{all possible} ways of gradually typing a
 program.
We use take these observations as guiding principles for a @emph{language agnostic}
 evaluation framework.

@; -----------------------------------------------------------------------------

@section{Performance Lattice}

Our evaluation framework is inspired by our previous work on extending functional
 Typed Racket to the object-oriented aspects of Racket, in which we
 use a lattice-style approach for a preliminary performance evaluation
 conducted in tandem with the design and implementation of Typed Racket@~cite[tfdffthf-ecoop-2015].
By inspecting all possible ways of typing two small
 game systems, we identified and then eliminated a major
 performance bottleneck from the implementation.
In contrast, we conduct our current evaluation completely @emph{independently} of
 Typed Racket's implementation efforts.@note{In terminology
  borrowed from the
  education community@~cite[scriven-chapter-1967], we conducted a @italic{formative
  evaluation} while this paper conducts a @italic{summative
  evaluation} to assess the post-intervention state of the system.}
Thus our framework is directly applicable to other sound gradual type systems.

Let us re-articulate the salient points from our previous work:
@itemlist[

@item{A (@italic{software system}) @italic{configuration} is a sequence of
 @math{n} modules.}

@item{Each module in a software system configuration is either typed or untyped.}

@item{For a fixed sequence of @math{n} modules there are @math{2^n} possible
 configurations.}

@item{
 Let @math{S} be the set of all configurations for a sequence of @math{n} modules.
 For @exact|{$c \in S$}| and @exact|{$i \in [0, n)$}|,
  let @exact|{$c(i) = 1 \mbox{ iff the } i^{\emph{th}}$}| module in the sequence is typed
  and
  let @exact|{$c(i) = 0 \mbox{ iff the } i^{\emph{th}}$}| module in the sequence is untyped.}

@item{Define @exact|{$\leq\,\subseteq S \times S$}| as:
 @exact|{$c_1 \leq c_2 \iff \forall i\,. (c_1(i) = 1) \Rightarrow c_2(i) = 1$}|}

@item{@emph{Proposition:} @exact|{$(S, \leq)$}| is a complete lattice.
 The fully untyped configuration is the
 bottom element and the fully-typed configuration is the top}
]
 We speak of a @italic{performance lattice} to describe a pair @exact|{$(S, \leq)$}|
  generated from a sequence of modules.

Our contribution is to exploit the lattice-oriented approach to benchmarking
 for a @emph{summative} evaluation.
To this end, we imagine software engineers who are considering the use of
 gradual typing for some program and consider what kinds of questions may
 influence their decision.
Based on this first step, we formulate a small number of parameterized,
 quantitative measures that capture possible answers to these questions.
The goal is that we as language designers evaluate performance using the
 same criteria that our users value.


@section{Measurements}

When the software system consists of a small number of modules, the engineers
 might be able to equip the entire program with type annotations in one fell
 swoop.
Such a fully annotated system should perform as well as
 the original, untyped program---and if the gradual type system is
 integrated with the compiler, it may even run faster because the compiler
 can apply type-based optimization techniques.

@def[#:term "typed/untyped ratio"]{The typed/untyped ratio of a performance
 lattice is the time needed to run the top configuration divided by the
 time needed to run the bottom configuration.}

Unfortunately, completely typed configurations may still rely on untyped
 libraries or core services.
In particular, Typed Racket relies on Racket's untyped runtime system, hence
 programs almost always use an untyped constant, function, or class across
 a type boundary.@note{To mitigate the cost of this boundary,
  part of Racket's runtime is put in an unchecked type environment.}
Thus improper typed/untyped ratios are possible even for optimizing gradual
 type systems.

Sufficiently large system cannot be fully converted all at
 once and may never even reach the top configuration.
In theory this is not a problem, as gradual typing promises to safely run
 any combination of typed and untyped modules.
But the cost of preserving type soundness may make some combinations impractical
 to run.

If the performance overhead is low, the configuration can be
 released and replace the currently deployed program.
To capture this idea, we formulate the following definition of
 ``deliverable configurations.''

@def[#:term @list{@deliverable{N}}]{A configuration in a performance
 lattice is @deliverable{N} if its performance is no worse than an
 @math{N}x slowdown compared to the completely untyped configuration.}

We parameterize this definition over the slowdown factor that a team
 may consider acceptable.
One team may think of a 1.1x slowdown as barely
 acceptable, while another one may tolerate a slowdown of an order of
 magnitude.

Even if a configuration is not deliverable, it might be suitably fast to
 run the test suites and prototype designs.
A software engineering team can
 use such a configuration for development purposes, but it may not
 deliver it.
In order to formulate this criteria
 properly, we introduce the following definition of usable configurations. 

@def[#:term @list{@usable["N" "M"]}]{A configuration in a performance
 lattice is @usable["N" "M"] if its performance is worse than an
 @math{N}x slowdown but no worse than an @math{M}x slowdown compared to
 the completely untyped configuration.}

Using the first parameter, we exclude deliverable configurations from the count.
The second parameter specifies the positive boundary, i.e., the acceptable
 slowdown factor for a usable configuration.
Conversely:

@def[#:term "unacceptable"]{A
 configuration is unacceptable if it is neither @deliverable{N} nor
 @usable["N" "M"].}

Finally, we can also ask the question how much work a team has to invest to
 turn unacceptable configurations into useful or even deliverable
 configurations.
In the context of macro-level gradual typing, one easy way
 to measure this amount of work is to count the number of modules that have
 to be annotated with types before the resulting configuration becomes
 usable or deliverable.

@def[#:term @list{@step["L" "N" "M"]}]{A configuration is
 @step["L" "N" "M"] if it is unacceptable and at most @math{L} type
 conversion steps away from a @deliverable{N} or a @usable["N" "M"]
 configuration.}

These four notions of the typed/untyped ration, @deliverable{N},
 @usable["N" "M"], and @step["L" "N" "M"] form the basis of our evaluation
 framework.
The benefit of the parameterized metrics is that
 every reader can interpret the raw data with his or her own choices for
 @math{L}, @math{N}, and @math{M}.


