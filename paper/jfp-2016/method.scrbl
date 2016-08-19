#lang scribble/base

@require[ 
  "common.rkt"
  "util.rkt"
  "benchmark.rkt"
  benchmark-util/data-lattice
  "typed-racket.rkt"
  (only-in pict blank hc-append scale)
  (only-in with-cache *use-cache?*)
]

@profile-point{sec:method}
@title[#:tag "sec:method"]{Evaluation Method}

Performance evaluation for gradual type systems must reflect how
 programmers use such systems.
Migrating an entire project from untyped to typed is rare,
 while incremental transitions are common.
Programmers tend to add types to a few modules, then compare the performance
 of the new mixed program against the previous running version.
If type-driven optimizations result in a performance improvement, all is well.
Otherwise, the developers may seek ways to reduce the cost of type boundaries.
As the program evolves, this process repeats.

We turn this observation into an evaluation method in three stages:
 first by describing the @emph{space} over which a performance evaluation must take place,
 second by giving @emph{metrics} relevant to the performance of a gradually typed program,
 and third by introducing a graphical shorthand for concisely representing exponentially large datasets.


@; -----------------------------------------------------------------------------

@section{Performance Lattice}

The promise of Typed Racket's macro-level gradual typing is that programmers may
 add types to any subset of the modules in an untyped program.
Performance evaluation must therefore consider the space of all
 @emph{configurations} a programmer could possibly create.
We describe this space as a static lattice representing all combinations of typed and
 untyped modules:
@itemlist[
  @item{
    A (@emph{software system}) @emph{configuration} is a sequence of
     @math{N} modules. Each module is either typed or untyped.
  }
  @item{
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
     @exact|{$c_1 \leq c_2$}|
     if and only if
     @exact|{$c_1(i) = 1$}|
     implies
     @exact|{$c_2(i) = 1$}|
     for all @exact{$i$} such that @exact{$0 \le i < N$}.
  }

  @item{
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
  @item{
    A @italic{performance lattice} is a pair @exact|{$(S, \leq)$}|.
  }
]

@; TODO clarify that lattice is a static artifact
@;       doesn't say anything about performance per se
@;      after measuring have some data,
@;       next challenge is how to get relevant lessons from data

After framing the performance lattice for a given program, language evaluators must
 generate a labeling @exact{$l$} such that for all @exact{$c \in S$} the performance of
 configuration @math{c} is expressed by @exact{$l(c)$}.
Researchers can then draw lessons and make comparisons using the labeling.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:measurements"]{Measuring Performance}

The most basic question about a gradually typed language is
 how fast fully-typed programs are in comparison to their fully untyped relative.
In principle, static types enable optimizations and can serve in place of the
 runtime tags used in safe dynamic languages.
The net effect of such improvements may, however, be offset in programs
 that rely heavily on an untyped library.
Hence we characterize the relative performance of fully-typed programs
 using a ratio to capture the possibility of speedups and slowdowns.

@; Determining whether speedups or slowdowns are the norm for fully typed programs
@;  may influence a software team's decision to experiment with gradual typing.

    @def[#:term "typed/untyped ratio"]{
     The typed/untyped ratio of a performance
      lattice is the time needed to run the top configuration divided by the
      time needed to run the bottom configuration.
    }

For users of gradual type systems, the important performance
 question is how much overhead due to gradual typing their current configuration suffers
 relative to the original program.
If the performance overhead is low enough, programmers can release the
 configuration to clients.
Depending on the nature of the software and clients' expectations,
 an appropriate substitute for "low enough" might take any value between zero overhead
 and an order-of-magnitude slowdown.
To account for these varying requirements, we use
 the following parameterized definition of deliverable configurations.

    @def[#:term @list{@deliverable{}}]{
     A configuration
      is @deliverable{} if its performance is no worse than a
      @math{D}x slowdown compared to the untyped configuration.
    }

Finally, if a software project is currently in an unacceptable
 configuration we ask how much work a team needs to invest to reach a
 deliverable or usable configuration.
We propose as a coarse measure of "work" the number of modules that must be
 annotated with types before performance improves.

    @def[#:term @list{@step{}}]{
     A configuration is @step[] if it is at most @math{k}
      type conversion steps from a @deliverable{} configuration.
    }

@profile-point{sec:method:example}
@(define sample-data
  (let* ([mean+std* '#((20 . 0) (15 . 0) (35 . 0) (10 . 0))]
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

These four notions form the basis of our evaluation method.
Let us illustrate these terms with a concrete example.
Suppose we have a project with
 two modules where the untyped configuration runs in @id[(sample-data 'c00)]
 seconds and the typed configuration runs in @id[(sample-data 'c11)] second.
Furthermore, suppose the gradually typed configurations run in
  @id[(sample-data 'c01)] and @id[(sample-data 'c10)] seconds.
Using white squares to represent untyped modules and black squares for typed
 modules, we can visualize this scenario in a performance lattice with
 configurations' overhead (relative to the untyped configuration) as labels.

@centered[@elem[
  (parameterize ([*LATTICE-CONFIG-MARGIN* 30]
                 [*LATTICE-LEVEL-MARGIN* 4])
    (make-performance-lattice (sample-data 'all)))
]]

@(let ([tu-ratio (/ (sample-data 'c11) (sample-data 'c00))]
       [t-str @id[(sample-overhead 'c11)]]
       [g-overhead (inexact->exact (max (sample-overhead 'c10) (sample-overhead 'c01)))])
  @elem{
    The typed/untyped ratio is @id[tu-ratio],
     indicating a performance improvement due to adding types.
    The typed configuration is also
      @deliverable[t-str]
      because it runs within a @elem[t-str]x
      slowdown relative to the untyped configuration.
    Both gradually typed configurations are
      @deliverable[@id[g-overhead]]
      because they run within @id[(* g-overhead (sample-data 'c00))] seconds.
    Lastly, these configurations are @step[t-str t-str]
     because each is one conversion step
     from the fully-typed configuration.
  })

Practitioners curious about the feasibility of gradual typing in Racket must
 replace the parameters with concrete values tailored to their needs.
If our experimental results in @Secref{sec:plots} show that a large number of
 configurations are deliverable under the chosen parameters,
 then the same results may hold for other projects.
Language implementors are meanwhile working to
 diagnose the most severe overheads and to improve the average-case overheads
 introduced by gradual typing.

@subsection{Auxilliary Notions}

@emph{Note} usable was a core definition in POPL, but we find it confuses things.

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
      lattice is @usable[] if its performance is worse than a
      @math{D}x slowdown but no worse than a @math{U}x slowdown compared to
      the untyped configuration.
    }

On the other hand, the performance overhead of gradual typing may render
 some configurations too slow even for development purposes.
These might be configurations where running the unit tests takes hours
 or days longer than normal.

    @def[#:term "unacceptable"]{
     An unacceptable configuration is neither @deliverable{} nor @usable[].
    }



@; -----------------------------------------------------------------------------
@section[#:tag "sec:graphs"]{Overhead Graphs}

@todo{how to presetn this huge amount of data in a comprehensible fashion}

    @figure*["fig:suffixtree-lattice" "64"
      @(parameterize ([*LATTICE-CONFIG-MARGIN* 3]
                      [*LATTICE-LEVEL-MARGIN* 8]
                      [*LATTICE-FONT-SIZE* 9]
                      [*LATTICE-BOX-HEIGHT* 6]
                      [*LATTICE-BOX-WIDTH* 3]
                      [*LATTICE-BOX-SEP* 0]
                      [*LATTICE-BOX-TOP-MARGIN* 0]
                      [*LATTICE-TRUNCATE-DECIMALS?* #t]
                      [*LATTICE-BOX-BOT-MARGIN* 1])
        (render-data-lattice suffixtree "6.2"))
    ]

More and more

    @figure*["fig:fsm-lattice" "FSM"
      @(parameterize ([*LATTICE-BOX-SEP* 0])
         (hc-append
           (render-data-lattice fsm "6.2")
           (blank 24 0)
           (render-data-lattice fsm "6.4")))
    ]

