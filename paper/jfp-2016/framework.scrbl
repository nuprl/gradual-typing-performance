#lang scribble/base

@require["common.rkt" benchmark-util/data-lattice]

@profile-point{sec:framework}
@title[#:tag "sec:framework"]{Evaluation Framework}

Performance evaluation for gradually typed languages must reflect how
 programmers use such languages.
Migrating an entire project from untyped to typed is rarely the initial goal,
 but rather a symptom of an exceptional initiative or a need to remove type
 boundaries.
Consequently, the question of whether there exists a
 smooth conversion path from fully-untyped to fully-typed that avoids
 performance overhead is purely academic.
In practice, a subset of modules are given types for
 reasons known only to the original developers.
The programmers then compare the overall performance of the hybrid program to
 the performance of the previous version.
If type-driven optimizations result in a performance improvement, all is well.
Otherwise, the developers may either accept the performance of the hybrid system
 or seek ways to reduce the cost of type boundaries.
As the program evolves, this process repeats.

We formalize these lessons in two stages: first by describing the @emph{space}
 over which a performance evaluation must take place and second by giving
 @emph{metrics} for judging the performance of a gradually typed program.


@; -----------------------------------------------------------------------------

@section{Performance Lattice}

The promise of Typed Racket's macro-level gradual typing is that any subset of modules
 in an untyped program may be given types.
Performance evaluation must therefore consider the space of all program
 @emph{configurations} reachable by typing a subset of the program's modules.
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
  @item{ @emph{Proposition:}
    For a fixed sequence of @math{N} modules there are @exact{$2^N$} possible
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

  @item{
    Define an indexed @emph{type conversion} relation @exact{$\rightarrow_k\,\subset\,\leq$}
     such that @exact{$c_1 \rightarrow_k c_2$} if and only if @exact{$c_1 \leq c_2$}
     and the sum @exact{$\,\Sigma_i\,\big[c_2(i) - c_1(i)\big]$} is less than or equal to @exact{$k$}.
    If @exact|{$c_1 \rightarrow_k c_2$}| we say that @exact{$c_2$} is reachable from
     @exact{$c_1$} in at most @exact{$k$} type conversion steps.
  }
]

A @italic{performance lattice} is a pair @exact|{$(S, \leq)$}|
 generated from a sequence of modules.
After framing the lattice for a given program, language evaluators must measure
 the performance of each configuration and generate a labeling @exact{$l$}
 such that for all @exact{$c \in S$} the performance of
 configuration @math{c} is expressed by @exact{$l(c)$}.
Once this data is aggregated, researchers can draw lessons and make comparisons
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
But for users of gradual type systems, the important performance
 question is how much overhead their current configuration suffers due
 to gradual typing.
If the performance overhead is low enough, the configuration can be
 released to clients.
Depending on the nature of the software and clients' expectations,
 an appropriate substitute for "low enough" might take any value between zero overhead
 and an order-of-magnitude slowdown.
To account for the varying requirements of software teams, we formulate
 the following parameterized definition of deliverable configurations.

    @def[#:term @list{@deliverable{}}]{
     A configuration in a performance
      lattice is @deliverable{} if its performance is no worse than a
      @math{D}x slowdown compared to the untyped configuration.
    }

Even if a configuration is not deliverable, it might be suitably fast to
 run the test suites and prototype designs.
A software engineering team can
 use such a configuration for development purposes without releasing
 it to clients.
@;In practice, usable configurations may act as checkpoints for a team to ensure
@; the product is working correctly before implementing performance optimizations.
Using a second parameter to capture the meaning of "suitably fast",
 we define a notion of usable configurations.

    @def[#:term @list{@usable[]}]{
     A configuration in a performance
      lattice is @usable[] if its performance is worse than an
      @math{D}x slowdown but no worse than a @math{U}x slowdown compared to
      the untyped configuration.
    }

On the other hand, the performance overhead of gradual typing may render
 some configurations too slow even for development purposes.
These might be configurations where running the unit tests takes hours
 or days longer than normal.

    @def[#:term "unacceptable"]{
     A unacceptable configuration is neither @deliverable{} nor @usable[].
    }

Finally, if a software project is currently in an unacceptable
 configuration we ask how much work a team needs to invest to reach a
 deliverable or usable configuration.
We propose as a coarse measure of "work" the number of modules that must be
 annotated with types before performance improves.
Using this metric, configurations one type conversion step away from a usable
 configuration are recognized as nearly usable themselves.

    @def[#:term @list{@step{}}]{
     A configuration is @step[] if it is unacceptable and at most @math{k}
      type conversion steps from a @deliverable{}
      or a @usable[] configuration.
    }

@profile-point{sec:framework:example}
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
@(define (sample-overhead cfg)
  (ceiling (/ (sample-data cfg) (sample-data 'c00))))

The four notions of the typed/untyped ratio, @deliverable{},
 @usable[], and @step[] form the basis of our evaluation
 framework.
As an example of these terms' use, suppose we have a project of
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

@(let ([tu-ratio (/ (sample-data 'c11) (sample-data 'c00))]
       [t-str @id[(sample-overhead 'c11)]]
       [g-overhead (inexact->exact (max (sample-overhead 'c10) (sample-overhead 'c01)))])
  @elem{
    In terms of our metrics, typed/untyped ratio is @id[tu-ratio],
     indicating a performance improvement due to adding types.
    The typed configuration is also
      @deliverable[t-str]
      because it runs within a @elem[t-str]x
      slowdown relative to the untyped configuration.
    Both gradually typed configurations are
      @deliverable[@id[g-overhead]]
      because they run within @id[(* g-overhead (sample-data 'c00))] seconds.
    Lastly, these configurations are @step[t-str t-str t-str]
     because each is one conversion step
     from the fully-typed configuration.
  })

Practitioners curious about the feasibility of gradual typing should
 replace the parameters with concrete values tailored to their needs.
If our experimental results in @Secref{sec:plots} show that a large number of
 configurations are deliverable under the actual parameters,
 then the same results may hold for other projects.
Language implementors should work to make more configurations deliverable
 or diagnose the cause of the worst performance overhead.
