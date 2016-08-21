#lang scribble/base

@require[ 
  "common.rkt"
  "util.rkt"
  "benchmark.rkt"
  benchmark-util/data-lattice
  "typed-racket.rkt"
  (only-in pict blank hc-append scale)
  (only-in with-cache *use-cache?*)
  (only-in gtp-summarize/summary
    from-rktd
    get-num-modules
    get-num-iterations
    D-deliverable
    get-num-configurations)
  (only-in gtp-summarize/lnm-plot
    count-configurations/mean)
]

@profile-point{sec:method}
@title[#:tag "sec:method"]{Evaluation Method}

@todo{fixup}

Performance evaluation for gradual type systems must reflect how
 programmers use such systems.
Migrating an entire project from untyped to typed is rare,
 while incremental transition is common.
@; TODO still ifffy
Consequently, programmers tend to judge the performance of gradual typing
 by comparing the performance of a program mixing typed and untyped components
 against the program's previous running version.
If type-driven optimizations result in a performance improvement, all is well.
Otherwise, the programmer may seek ways to reduce the cost of type boundaries.
As the program evolves, this process repeats.

We turn this observation into an evaluation method in three stages:
 first by describing the @emph{space} over which a performance evaluation must take place,
 second by giving @emph{metrics} relevant to the performance of a gradually typed program,
 and third by introducing a graphical shorthand for concisely presenting the
 metrics on exponentially large datasets.


@; -----------------------------------------------------------------------------

@section{Performance Lattice}

The promise of Typed Racket's macro-level gradual typing is that programmers may
 add types to any subset of the modules in an untyped program.
Performance evaluation must therefore consider the space of program
 @emph{configurations} a programmer could possibly create.
That is, all ways of choosing a single module to type, all ways of choosing two
 modules to type, and so on to include every combination of typed and untyped modules.
We describe this space as a lattice.
@itemlist[
  @item{
    A (@emph{program}) @emph{configuration} is a sequence of
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
    Define @exact|{$\leq$}| (a subset of @exact|{$S \times S$}|) as:
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
     bottom element and the fully-typed configuration is the top element.
  }

  @item{
    Define an indexed @emph{type conversion} relation @exact{$\rightarrow_k$} (a subset of @exact{$\leq$}) as:
     @exact{$c_1 \rightarrow_k c_2$} if and only if @exact{$c_1 \leq c_2$}
     and the sum @exact{$\,\Sigma_i\,\big[c_2(i) - c_1(i)\big]$} is less than or equal to @exact{$k$}.
    If @exact|{$c_1 \rightarrow_k c_2$}| we say that @exact{$c_2$} is reachable from
     @exact{$c_1$} in at most @exact{$k$} type conversion steps.
  }
  @item{
    A @italic{performance lattice} is a pair @exact|{$(S, \leq)$}|.
  }
]

@(let* ([suffixtree-lattice-version "6.2"]
        [S (from-rktd (benchmark-rktd suffixtree suffixtree-lattice-version))]
        [suffixtree-num-modules (integer->word (get-num-modules S))]
        [suffixtree-num-configs (number->string (get-num-configurations S))]
        [suffixtree-num-iters   (number->string (get-num-iterations S))]
        [suffixtree-sample-D    2]
        [suffixtree-num-D       (integer->word ((D-deliverable suffixtree-sample-D) S))]
        [suffixtree-sample-k    1]
        [suffixtree-num-k       (integer->word ((count-configurations/mean S suffixtree-sample-k) suffixtree-sample-D))]
       )
  (list
    @elem{
      A performance lattice illustrates a program's configurations.
      Equipped with a labeling @exact{$l$} such that @exact{$l(c)$} characterizes the
       performance of configuration @exact{$c$}, a lattice is a full description of
       one program's performance under gradual typing.
      Creating a lattice is difficult (requires type annotations for every module in the program)
       and generating a labeling is time consuming (requires exponentially many measurements)
       but such is the design space.
      @; NOTE:
      @; - generating a lattice is not for users -- requires a full typing
      @; - generating a labeleing is exponentially time consuming.

      @Figure-ref{fig:suffixtree-lattice} is a labeled lattice for one mid-sized program from
       our benchmark suite.
      The program, @bm[suffixtree], has @|suffixtree-num-modules| modules.
      Thus the lattice has @|suffixtree-num-configs| configurations,
       rendered as @|suffixtree-num-modules|-segment rectangles.
      The bottom element in the lattice represents the untyped configuration.
      Directly above it, the first level of the lattice represents all configurations with one typed module;
       these configurations' rectangles have 1 filled segment.
      In general the @exact|{$i^{\emph{th}}$}| level of the lattice represents all configurations
       with @math{i} typed modules, and the top element is a black rectangle representing the fully typed configuration.

      The label below each rectangle is that configuration's overhead@note{Ratio of two means; each mean is over @|suffixtree-num-iters| samples.}
       relative to the untyped configuration.
      @;For instance, the fully typed configuration runs 40% faster than the untyped configuration
      @; and the slowest configuration is @id[@round[@max-overhead[@[benchmark-rktd suffixtree "6.2"]]]]x slower than untyped.
      With this data, one can answer nearly any question about @bm[suffixtree]'s performance overhead due to gradual typing.
      For instance, @|suffixtree-num-D|
       configurations run within a @id[suffixtree-sample-D]x overhead
       and @|suffixtree-num-k|
        configurations are at most @id[suffixtree-sample-k] type conversion step
        from a configuration with no more than @id[suffixtree-sample-D]x overhead.

      A lattice, however, is a poor visual aid for answering such questions.
      Images such as @figure-ref{fig:suffixtree-lattice} successfully
       convey the spectrum of migration paths available to programmers, but evaluating
       performance with a lattice is difficult at best.
      In the next subsection, we identify key questions that a performance lattice can answer.
      The following subsection introduces an alternative visualization tailored to these questions.
    }
    @figure*["fig:suffixtree-lattice" @elem{Performance overhead in @bm[suffixtree], on Racket v@|suffixtree-lattice-version|}
      @(parameterize ([*LATTICE-CONFIG-MARGIN* 3]
                      [*LATTICE-LEVEL-MARGIN* 8]
                      [*LATTICE-FONT-SIZE* 9]
                      [*LATTICE-BOX-HEIGHT* 6]
                      [*LATTICE-BOX-WIDTH* 3]
                      [*LATTICE-BOX-SEP* 0]
                      [*LATTICE-BOX-TOP-MARGIN* 0]
                      [*LATTICE-TRUNCATE-DECIMALS?* #t]
                      [*LATTICE-BOX-BOT-MARGIN* 1])
        (render-data-lattice suffixtree suffixtree-lattice-version))
    ]
))


@; -----------------------------------------------------------------------------
@section[#:tag "sec:measurements"]{Performance Metrics}

The most basic question about a gradually typed language is
 how fast fully-typed programs are in comparison to their fully untyped relative.
In principle, static types enable optimizations and can serve in place of the
 runtime tags used in safe dynamic languages.
The net effect of such improvements may, however, be offset by runtime type checks
 in programs that rely heavily on an untyped library.
Hence we characterize the relative performance of fully-typed programs
 using a ratio to capture the possibility of speedups and slowdowns.

@; Determining whether speedups or slowdowns are the norm for fully typed programs
@;  may influence a software team's decision to experiment with gradual typing.

    @def[#:term "typed/untyped ratio"]{
     The typed/untyped ratio of a performance
      lattice is the time needed to run the top configuration divided by the
      time needed to run the bottom configuration.
    }

For users of a gradual type system, the important performance
 question is how much overhead due to gradual typing their current configuration suffers
 relative to the original program.
If the performance overhead is low enough, programmers can release the
 configuration to clients.
Depending on the nature of the software and clients' expectations,
 an appropriate substitute for "low enough" might take any value between zero overhead
 and an order-of-magnitude slowdown.
To account for these varying requirements, we use
 the following parameterized definition of a deliverable configuration.

    @def[#:term @list{@deliverable{}}]{
     A configuration
      is @deliverable{} if its performance is no worse than a
      @math{D}x slowdown compared to the untyped configuration.
    }

    @; NOTE: really should be using %, but Sec 4 shows why we stick to x

If a software project is currently in a non-deliverable configuration,
 the next question is how much work a team must invest to reach a
 deliverable configuration.
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

Let us illustrate these terms with an example.
Suppose we have a project with
 two modules where the untyped configuration runs in @id[(sample-data 'c00)]
 seconds and the typed configuration runs in @id[(sample-data 'c11)] seconds.
Furthermore, suppose the gradually typed configurations run in
  @id[(sample-data 'c01)] and @id[(sample-data 'c10)] seconds.
@Figure-ref{fig:demo-lattice} is a performance lattice for this hypothetical program.
The label below each configuration is its overhead relative to the untyped configuration.

    @figure["fig:demo-lattice" "Sample performance lattice"
      (parameterize ([*LATTICE-CONFIG-MARGIN* 30]
                     [*LATTICE-LEVEL-MARGIN* 4]
                     [*LATTICE-BOX-SEP* 0])
        (make-performance-lattice (sample-data 'all)))
    ]

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

The ratio of @deliverable{D} configurations in such a lattice is a measure of
 the overall feasibility of gradual typing.
When the ratio is high, then no matter how the software evolves performance will
 likely remain acceptable.
Conversely, a low ratio means the team may struggle to retain performance after
 incrementally typing a few modules.
Practitioners with fixed performance requirements can thereby extrapolate
 the performance of a gradual type system based on the number of deliverable configurations.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:graphs"]{Overhead Graphs}

@todo{how to presetn this huge amount of data in a comprehensible fashion}

More and more

    @figure*["fig:fsm-lattice" "FSM"
      @(parameterize ([*LATTICE-BOX-SEP* 0])
         (hc-append
           (render-data-lattice fsm "6.2")
           (blank 24 0)
           (render-data-lattice fsm "6.4")))
    ]

