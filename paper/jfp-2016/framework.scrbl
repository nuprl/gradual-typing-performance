#lang scribble/base

@; TODO lattice
@;@figure["fig:fsm-lattice" "FSM performance lattice (labels are speedup/slowdown factors)"
@;  @(let ([vec (file->value "src/fsm-lattice-data.rktd")])
@;     (make-performance-lattice vec))
@;]

@require["common.rkt"]

@title[#:tag "sec:framework"]{Evaluation Framework}

Performance evaluation for gradually typed languages must reflect how
 developers use such languages.
Migrating an entire project from untyped to typed is @emph{not} the initial goal,
 but rather a symptom of exceptional initiative or a need to remove type
 boundaries.
Consequently, the question of whether there exists a
 smooth conversion path from fully-untyped to fully-typed that avoids
 performance overhead is purely academic.
In practice, what happens is that a subset of modules are given types for
 reasons known only to the original developers.
The overall performance of the hybrid program is then compared to the previous
 version of the program.
If type-driven optimizations result in a performance improvement, all is well.
Otherwise, the developers may either accept the performance of the hybrid system
 or seek ways to reduce the cost of type boundaries.

We formalize these lessons in two stages: first by describing the @emph{space}
 over which a performance evaluation must take place and second by giving
 @emph{metrics} for judging the performance of a gradually typed program.


@; -----------------------------------------------------------------------------

@section{Performance Lattice}

The promise of macro gradual typing is that any subset of modules
 in an untyped program may be given types.@note{The promise of @emph{micro} gradual
   typing is that any expression may be given types.}
Performance evaluation must therefore consider the space of all @emph{configurations}
 of a program that can be obtained by adding types to a subset of the program's modules.
We describe this space as a static lattice representing all combinations of typed and
 untyped modules.
This lattice-based approach to performance measurements was introduced in our
 preliminary evaluation of Typed Racket's object-oriented
 features@~cite[tfdffthf-ecoop-2015]; here we summarize the key points:
@itemlist[
  @item{
    A (@emph{software system}) @emph{configuration} is a sequence of
     @math{N} modules where each module is either typed or untyped.
  }
  @item{
    For a fixed sequence of @math{N} modules there are @math{2^N} possible
     configurations.
  }
  @item{
    Let @math{S} be the set of all configurations for @math{N} modules.
    For @exact|{$c \in S$}| and @exact|{$i \in [0, N)$}|,
     let @exact|{$c(i) = 1 \mbox{ iff the } i^{\emph{th}}$}| module in the sequence is typed
     and
     let @exact|{$c(i) = 0 \mbox{ iff the } i^{\emph{th}}$}| module in the sequence is untyped.
  }

  @item{
    Define @exact|{$\leq\,\subseteq S \times S$}| as:
     @exact|{$c_1 \leq c_2 \iff \forall i\,. (c_1(i) = 1) \Rightarrow c_2(i) = 1$}|
  }

  @item{ @emph{Proposition:}
    @exact|{$(S, \leq)$}| is a complete lattice.
    The fully untyped configuration is the
     bottom element and the fully-typed configuration is the top
  }
]

A @italic{performance lattice} is a pair @exact|{$(S, \leq)$}|
 generated from a sequence of modules.
After framing the lattice for a given program, language evaluators must measure
 the performance of each configuration and generate a labeling @exact{$l$}
 such that @exact{$\forall c \in S$} the average running time of
 configuration @math{c} is @exact{$l(c)$}.
Once the data is aggregated, we can draw lessons and make comparisons
 using the lattice and labeling.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:measurements"]{Measuring Performance}

The most basic question one might ask about a gradually typed language is
 whether fully-typed programs can be faster than untyped programs.
In principle, static types enable optimizations and can serve in place of the
 run-time tags used in safe dynamic languages, so one would expect a speedup.
But the net effect of these improvements may be offset in programs
 that rely heavily on an untyped library.
Determining whether speedups or slowdowns are the norm for fully typed programs
 may influence a software team's decision to experiment with gradual typing.

    @def[#:term "typed/untyped ratio"]{
     The typed/untyped ratio of a performance
      lattice is the time needed to run the top configuration divided by the
      time needed to run the bottom configuration.
    }

For programmers already using gradual typing, the important performance
 question is how much overhead their current configuration suffers due
 to gradual typing.
If the performance overhead is low enough, the configuration can be
 released and used by clients.
Depending on the nature of the software and clients' expectations,
 the value of "low enough" might take any value between zero overhead
 and an order-of-magnitude slowdown.
To account for the varying requirements of software teams, we formulate
 the following parameterized definition of "deliverable configurations."

    @def[#:term @list{@deliverable{}}]{
     A configuration in a performance
      lattice is @deliverable{} if its performance is no worse than a
      @math{D}x slowdown compared to the untyped configuration.
    }

Even if a configuration is not deliverable, it might be suitably fast to
 run the test suites and prototype designs.
A software engineering team can
 use such a configuration for development purposes, but it may not
 release it to clients.
In order to formulate this criteria
 properly, we introduce the following definition of usable configurations.

    @def[#:term @list{@usable[]}]{
     A configuration in a performance
      lattice is @usable[] if its performance is worse than an
      @math{D}x slowdown but no worse than a @math{U}x slowdown compared to
      the untyped configuration.
    }

Using the first parameter, we exclude deliverable configurations from the count.
The second parameter specifies the positive boundary, i.e., the acceptable
 slowdown factor for a usable configuration.
In practice, usable configurations may act as checkpoints for a team to ensure
 the product is working correctly before implementing performance optimizations.

On the other hand, the performance overhead of gradual typing may render
 some configurations too slow even for development purposes.
These might be configurations where running the unit tests takes hours
 or days longer than normal.

    @def[#:term "unacceptable"]{
     A configuration is unacceptable if it is neither @deliverable{} nor
      @usable[].
    }

Finally, if a software project is currently in an unacceptable
 configuration we ask how much work a team needs to invest to reach a
 @deliverable{} or @usable{} configuration.
We propose as a coarse measure of "work" the number of modules that must be
 annotated with types before performance improves.
Using this metric, configurations one module away from a usable configuration
 are nearly usable themselves.

    @def[#:term @list{@step{}}]{
     A configuration is @step[] if it is unacceptable and at
      most @math{k} type conversion steps away from a @deliverable{}
      or a @usable[] configuration.
    }

These four notions of the typed/untyped ratio, @deliverable{},
 @usable[], and @step[] form the basis of our evaluation
 framework.
Practitioners curious about the feasability of gradual typing should
 replace these parameters with concrete values tailored to their needs.
If experimental results show that a large number of configurations are
 deliverable, etc. under the actual parameters, then the same results may
 well hold for other projects.
Language implementors should work to make more configurations deliverable
 or diagnose the cause of the worst performance overhead.
