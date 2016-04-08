#lang scribble/base

@; TODO lattice
@;@figure["fig:fsm-lattice" "FSM performance lattice (labels are speedup/slowdown factors)"
@;  @(let ([vec (file->value "src/fsm-lattice-data.rktd")])
@;     (make-performance-lattice vec))
@;]

@require["common.rkt" benchmark-util/data-lattice]

@title[#:tag "sec:framework"]{Evaluation Framework}

Performance evaluation for gradually typed languages must reflect how
 developers use such languages.
Migrating an entire project from untyped to typed is rarely the initial goal,
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

The promise of macro-level gradual typing is that any subset of modules
 in an untyped program may be given types.
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
Hence we characterize the relative performance of fully-typed programs
 using a ratio to capture the possibility of speedups and slowdowns.

    @def[#:term "typed/untyped ratio"]{
     The typed/untyped ratio of a performance
      lattice is the time needed to run the top configuration divided by the
      time needed to run the bottom configuration.
    }

Determining whether speedups or slowdowns are the norm for fully typed programs
 may influence a software team's decision to experiment with gradual typing.
Once a team is already using gradual typing, the important performance
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

@(define sample-data
  (let* ([mean+std* '#((2 . 0) (1.5 . 0) (3.5 . 0) (1 . 0))]
         [mean (lambda (i) (car (vector-ref mean+std* i)))])
    (lambda (tag)
      (case tag
       [(c00) (mean 0)]
       [(c01) (mean 1)]
       [(c10) (mean 2)]
       [(c11) (mean 3)]
       [(all) mean+std*]
       [else (raise-user-error 'sample-data "Invalid configuration '~a'. Use e.g. c00 for untyped." tag)]))))

These four notions of the typed/untyped ratio, @deliverable{},
 @usable[], and @step[] form the basis of our evaluation
 framework.
As an example of how they are used, suppose we have a project with
 two modules where the untyped configuration runs in @id[(sample-data 'c00)]
 seconds and the typed configuration runs in @id[(sample-data 'c11)] second.
Furthermore, suppose the gradually typed configurations run in
  @id[(sample-data 'c01)] and @id[(sample-data 'c10)] seconds.
Using white squares to represent untyped modules and black squares for typed
 modules, we can visualize this scenario in a performance lattice with
 configurations' overhead (relative to the untyped configuration) as labels.

@centered[@elem[
  (parameterize ([*LATTICE-HSPACE* 30]
                 [*LATTICE-VSPACE* 4])
    (make-performance-lattice (sample-data 'all)))
]]

In terms of our metrics, typed/untyped ratio is
 @id[(/ (sample-data 'c11) (sample-data 'c00))]
 indicating a performance improvement due to adding types.
The typed configuration is also @deliverable{1} because it runs within a 1x
 slowdown relative to the untyped configuration.
Both gradually typed configurations are @deliverable{2} because they run within 4 seconds.
These configurations are also @step["1" "1" "1"] because each is one conversion step
 from the fully-typed configuration.

Practitioners curious about the feasability of gradual typing should
 replace our parameters with concrete values tailored to their needs.
If experimental results show that a large number of configurations are
 deliverable, etc. under the actual parameters, then the same results may
 well hold for other projects.
Language implementors should work to make more configurations deliverable
 or diagnose the cause of the worst performance overhead.
