#lang scribble/base

@require[
  "common.rkt"
  "util.rkt"
  "benchmark.rkt"
  "typed-racket.rkt"
  benchmark-util/data-lattice
  (only-in racket/format ~r)
  (only-in gtp-summarize/summary
    from-rktd
    get-num-modules
    get-num-iterations
    D-deliverable
    get-num-configurations)
  (only-in gtp-summarize/lnm-plot
    count-configurations/mean)
  (except-in gtp-summarize/lnm-parameters defparam)
]

@profile-point{sec:method}
@title[#:tag "sec:method"]{Evaluation Method}

Performance evaluation for gradual type systems must reflect how programmers use such systems.
Experience with Typed Racket shows that programmers frequently combine typed and untyped code within an application.
These applications may undergo incremental transitions that add or remove some type annotations;
 however, it is rare that a programmer adds explicit annotations to every module in the program.

After incrementally typing part of an application, programmers compare the performance of the modified program with the previous version.
If type-driven optimizations result in a performance improvement, all is well.
Otherwise, the programmer may try to address the performance overhead.
As they continue to develop the program, programmers repeat this process.

The following subsections build an evaluation method from these observations in three steps.
First, @secref{sec:method:lattice} describes the @emph{space} over which a performance evaluation must take place.
Second, @secref{sec:measurements} defines @emph{metrics} relevant to the performance of a gradually typed program.
Third, @secref{sec:graphs} introduces a @emph{visualization} that concisely presents the metrics on exponentially large spaces.


@; -----------------------------------------------------------------------------

@section[#:tag "sec:method:lattice"]{Performance Lattice}

The promise of Typed Racket's macro-level gradual typing is that programmers may
 convert any subset of the modules in an untyped program to Typed Racket.
A comprehensive performance evaluation must therefore consider the space of
 typed/untyped @emph{configurations} a programmer could possibly create.
These configurations form a lattice.
@itemlist[
  @item{
    A (@emph{program}) @emph{configuration} is a sequence of
     @math{N} modules. Each module in a configuration is either typed or untyped.
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
     let @exact|{$c(i) = 0 \mbox{ iff the } i^{\emph{th}}$}| module in the sequence is untyped.@note{Representing configurations as @math{N}-bit binary numbers induces a total ordering on configurations. @Figure-ref{fig:exact-runtimes} re-uses this ordering.}
  }

  @item{
    Define @exact|{$\leq$}| (a subset of @exact|{$S \times S$}|) as:
     @exact|{$c_1 \leq c_2$}|
     if and only if
     @exact|{$c_1(i) \leq c_2(i)$}|
     for all @exact|{$i \in [0, N)$}|.
  }

  @item{
    @exact|{$(S, \leq)$}| is a complete lattice; henceforth, a @emph{configuration lattice}.
    The untyped configuration is the bottom element and the fully-typed configuration is the top element.
  }

  @item{
    Define an indexed @emph{type conversion} relation @exact{$\rightarrow_k$} (a subset of @exact{$\leq$}) as:
     @exact{$c_1 \rightarrow_k c_2$} if and only if @exact{$c_1 \leq c_2$}
     and the sum @exact{$\,\Sigma_i\,\big[c_2(i) - c_1(i)\big]$} is less than or equal to @exact{$k$}.
    If @exact|{$c_1 \rightarrow_k c_2$}|, then @exact{$c_2$} is reachable from
     @exact{$c_1$} in @exact{$k$} or fewer @emph{type conversion steps}.
  }
  @item{
    A @emph{performance lattice} is a configuration lattice @exact|{$(S, \leq)$}|
     equipped with a labeling @exact{$l$} such that
     @exact{$l(c)$} characterizes the performance of configuration @exact{$c$}
     for all @exact{$c \in S$}.
  }
]

@(define suffixtree-lattice-version "6.2")
@(define S (from-rktd (benchmark-rktd suffixtree suffixtree-lattice-version)))
@(define suffixtree-num-modules     (integer->word (get-num-modules S)))
@(define suffixtree-num-configs     (get-num-configurations S))
@(define suffixtree-num-configs-str (number->string suffixtree-num-configs))
@(define suffixtree-num-iters       (number->string (get-num-iterations S)))
@(define suffixtree-sample-D        2)
@(define suffixtree-num-D           ((D-deliverable suffixtree-sample-D) S))
@(define suffixtree-num-D-str       (integer->word suffixtree-num-D))
@(define suffixtree-sample-k        1)
@(define suffixtree-num-k           ((count-configurations/mean S suffixtree-sample-k) suffixtree-sample-D))
@(define suffixtree-num-k-str       (number->string suffixtree-num-k))
@(define suffixtree-tu-ratio        (format "~ax" (~r (typed/untyped-ratio S) #:precision '(= 1))))

    @figure*["fig:suffixtree-lattice" @elem{Performance overhead in @bm[suffixtree], on Racket v@|suffixtree-lattice-version|.}
      @(parameterize ([*LATTICE-CONFIG-MARGIN* 3]
                      [*LATTICE-LEVEL-MARGIN* 8]
                      [*LATTICE-FONT-SIZE* 9]
                      [*LATTICE-BOX-HEIGHT* 6]
                      [*LATTICE-BOX-WIDTH* 3]
                      [*LATTICE-BOX-SEP* 0]
                      [*LATTICE-BOX-TOP-MARGIN* 0]
                      [*LATTICE-TRUNCATE-DECIMALS?* #t]
                      [*LATTICE-BORDER-WIDTH* 0.5]
                      [*LATTICE-BOX-BOT-MARGIN* 1])
        (render-data-lattice suffixtree suffixtree-lattice-version))
    ]

@Figure-ref{fig:suffixtree-lattice} is a performance lattice for @bm[suffixtree], a mid-sized benchmark in the @|GTP| suite.
The benchmark consists of @|suffixtree-num-modules| modules, thus the lattice has @|suffixtree-num-configs-str| configurations.
Each configuration is rendered as a @|suffixtree-num-modules|-segment rectangle.
The bottom element in the lattice represents the untyped configuration.
The first level of the lattice represents all configurations with one typed module;
 these configurations' rectangles have one filled segment.
In general the @exact|{$i^{\emph{th}}$}| level of the lattice represents all configurations
 with @math{i} typed modules as rectangles with @math{i} filled segments.
The label below each rectangle is that configuration's overhead@note{Ratio of two means; each mean is over @|suffixtree-num-iters| samples.} relative to the untyped configuration.

With this data, a language implementor can answer nearly any question about the performance overhead in this benchmark program due to gradual typing.
For instance, @|suffixtree-num-D-str|
 configurations run within a @id[suffixtree-sample-D]x overhead
 and @|suffixtree-num-k-str|
 configurations are at most @id[suffixtree-sample-k] type conversion step
 from a configuration that runs within @id[suffixtree-sample-D]x overhead.
@(let* ([too-many-modules 8]
        [num-bm-with-too-many (integer->word (length (filter (lambda (b) (< too-many-modules (benchmark->num-modules b))) ALL-BENCHMARKS)))])
   @elem{
     Conversely, @figure-ref{fig:suffixtree-lattice} demonstrates that a graphical presentation of a performance lattice is a poor visual aid for answering such questions.
   })

@; -----------------------------------------------------------------------------
@section[#:tag "sec:measurements"]{Performance Metrics}

The most basic question about a gradually typed language is
 how fast fully-typed programs are in comparison to their fully untyped relative.
In principle and in Typed Racket, static types enable optimizations and can serve in place of runtime tag checks.
The net effect of such improvements may, however, be offset by runtime type checks
 in programs that rely heavily on an untyped library.
Relative performance is therefore best described as a ratio, to capture the
possibility of speedups and slowdowns.@;
@;
    @def[#:term "typed/untyped ratio"]{
     The typed/untyped ratio of a performance
      lattice is the time needed to run the top configuration divided by the
      time needed to run the bottom configuration.
    }@;
@;
For users of a gradual type system, the important performance
 question is how much overhead their @emph{current} configuration suffers.
 @; RELATIVE TO previous version (standardized as "untyped program")
If the performance overhead is low enough, programmers can release the
 configuration to clients.
Depending on the nature of the application,
 an appropriate substitute for ``low enough'' might take any value between zero overhead
 and an order-of-magnitude slowdown.
The following parameterized definition of a deliverable configuration accounts for these varying requirements.@;
@;
    @def[#:term @list{@deliverable{}}]{
     A configuration
      is @deliverable{} if its performance is no worse than a
      @math{D}x slowdown compared to the untyped configuration.
     @;A program is @deliverable{} if all its configurations are @deliverable{}.
    }@;
@;
@; NOTE: really should be using %, but Sec 4 shows why stick to x
@;
If an application is currently in a non-@deliverable[] configuration,
 the next question is how much work a team must invest to reach a
 @deliverable[] configuration.
One coarse measure of ``work'' is the number of modules that must be annotated with types before performance improves.@;
@;
    @def[#:term @list{@step{}}]{
     A configuration @exact{$c_1$} is @step[] if @exact{$c_1 \rightarrow_k c_2$}
      and @exact{$c_2$} is @deliverable{}.
    }@;
@; @profile-point{sec:method:example}
The number of @step[] configurations therefore captures the experience of a @emph{prescient} programmer that divines the @exact{$k$} modules
best-suited to improve performance.

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
Suppose there is a project with
 two modules where the untyped configuration runs in @id[(sample-data 'c00)]
 seconds and the typed configuration runs in @id[(sample-data 'c11)] seconds.
Furthermore, suppose the mixed configurations run in
  @id[(sample-data 'c01)] and @id[(sample-data 'c10)] seconds.
@Figure-ref{fig:demo-lattice} is a performance lattice for this hypothetical program.
The label below each configuration is its overhead relative to the untyped configuration.

    @figure["fig:demo-lattice" "Sample performance lattice"
      (parameterize ([*LATTICE-CONFIG-MARGIN* 30]
                     [*LATTICE-LEVEL-MARGIN* 4]
                     [*LATTICE-BOX-SEP* 0])
        (make-performance-lattice (sample-data 'all)))
    ]

@(let* ([tu-ratio (/ (sample-data 'c11) (sample-data 'c00))]
        [t-str @id[(sample-overhead 'c11)]]
        [g-overhead (inexact->exact (max (sample-overhead 'c10) (sample-overhead 'c01)))]
        [g-min-overhead (inexact->exact (min (sample-overhead 'c10) (sample-overhead 'c01)))]
        [g-overhead2 (~r (+ g-min-overhead (/ (- g-overhead g-min-overhead) 2)) #:precision '(= 1))])
  @elem{
    The typed/untyped ratio is @id[tu-ratio],
     indicating a performance improvement due to adding types.
    The typed configuration is also
      @deliverable[t-str]
      because it runs within a @elem[t-str]x
      slowdown relative to the untyped configuration.
    Both mixed configurations are
      @deliverable[@id[g-overhead]]
      because they run within @id[(* g-overhead (sample-data 'c00))] seconds,
      but only one is @deliverable[@id[g-overhead2]].
    Lastly, these configurations are @step[t-str t-str]
     because each is one conversion step
     from the typed configuration.
  })

The ratio of @deliverable{D} configurations in such a lattice is a measure of
 the overall feasibility of gradual typing.
When this ratio is high, then no matter how the application evolves, performance
 is likely to remain acceptable.
Conversely, a low ratio implies that a team may struggle to recover performance after
 incrementally typing a few modules.
Practitioners with a fixed performance requirement @math{D} can therefore use the number
 of @deliverable[] configurations to extrapolate the performance of a gradual type system.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:graphs"]{Overhead Graphs}

@(render-lnm-plot
  #:rktd*** (list (list (list (benchmark-rktd suffixtree suffixtree-lattice-version))))
  (lambda (pict*)
    (list
      @; less than half of all @bm[suffixtree] configurations run within a @id[(*MAX-OVERHEAD*)]x slowdown.
      @elem{
        The main lesson to extract from a performance lattice is the number of
         @deliverable{} configurations for various @math{D}.
        The plot on the left half of @figure-ref{fig:suffixtree-plot} presents this
         information for the @bm[suffixtree] benchmark.
        On the x-axis, possible values for @math{D} range continuously from one to @integer->word[(*MAX-OVERHEAD*)].
        Dashed lines to the left of the 2x tick pinpoint overheads of 1.2x, 1.4x, 1.6x, and 1.8x.
        To the right of the 2x tick, similar dashed lines pinpoint 4x, 6x, 8x, etc.
        The y-axis gives the proportion of configurations in the lattice that
         suffer at most @math{D}x performance overhead.
        Using this plot, one can confirm our earlier observation that @|suffixtree-num-D-str|
         of the @|suffixtree-num-configs-str| configurations
         (@(id (round (* 100 (/ suffixtree-num-D suffixtree-num-configs))))%)
         run within a @id[suffixtree-sample-D]x overhead.
        Additionally, the typed/untyped ratio (@|suffixtree-tu-ratio|) is above the plot.

        Viewed as a cumulative distribution function, the plot demonstrates how increasing @math{D} increases the number of @deliverable[] configurations.
        In this case, the shallow slope implies that few configurations become deliverable as the programmer accepts a larger performance overhead.
        The ideal slope would have a steep incline and a large y-intercept, meaning that few configurations suffer large overhead and many run faster than the untyped baseline.

        @;Nonetheless, the plot effectively summarizes this large dataset.
        @;Similar plots will handle arbitrarily large programs.

        The plot on the right half of @figure-ref{fig:suffixtree-plot} gives
         the number of @step{1} configurations.
        A point @math{(X,Y)} on this plot represents the percentage @math{Y}
         of configurations @exact{$c_1$} such that there exists a configuration
         @exact{$c_2$} where @exact{$c_1 \rightarrow_1 c_2$} and @exact{$c_2$}
         runs at most @math{X} times slower than the untyped configuration.@note{Note that @exact{$c_1$} and @exact{$c_2$} may be the same, for instance when @exact{$c_1$} is the fully-typed configuration.}
        Intuitively, this plot resembles the (0-step) @deliverable[] plot
         because accounting for one type conversion step does not change the overall
         characteristics of the benchmark, but only makes
         more configurations @deliverable[].
         @;@note{Plotting the @math{k=0} and @math{k=1} curves on the same axis would facilitate comparisons; however, the primary goal is to compare multiple implementations of a gradual type system on a fixed @math{k}.}

        These @emph{overhead plots} concisely summarize the @bm[suffixtree] dataset.
        The same presentation scales to arbitrarily large programs.
        Furthermore, one can make high-level comparisons between multiple implementations
         of a gradual type system by plotting their curves on the same axes.
      }
      @figure*["fig:suffixtree-plot" @elem{Overhead graphs for @bm[suffixtree], on Racket v@|suffixtree-lattice-version|.}
        (car pict*)
      ]
)))

@subsection{Assumptions and Limitations}

Plots in the style of @figure-ref{fig:suffixtree-plot} rest on two assumptions and have two significant limitations, which readers must keep in mind when interpreting our results.

@; - assn: log scale
The @emph{first assumption} is that configurations with less than 2x overhead are significantly
 more practical than configurations with over 10x overhead.
Hence the plots use a log-scaled x-axis
 to simultaneously encourage fine-grained comparison in the 20-60% overhead range
 and blur the distinction between, e.g., 14x and 18x slowdowns.

@; Zorn: 30% of execution time in storage management "represent the worst-case overhead that that might be expected to be associated with garbage collection."

@; - assn: 20x
The @emph{second assumption} is that configurations with more than 20x
 overhead are completely unusable in practice.
Pathologies like the 100x slowdowns in @figure-ref{fig:suffixtree-lattice}
 represent a challenge for implementors, but if these overheads suddenly
 dropped to 30x, the configurations would still be useless to developers.

@; - assn: ok to take means
@; meh whocares

@; - limit: no identity, no relation between configs (where is typed?)
The @emph{first limitation} of the overhead plots is that they hide a configuration's identity.
One cannot distinguish the fully-typed configuration; moreover,
 the @exact{$\leq$} and @exact{$\rightarrow_k$} relations are lost.
The typed/untyped ratio above the left plot partially compensates for the former.
Unfortunately, the other configurations remain anonymous.

@; - limit: angelic choice
The @emph{second limitation} is that the @step{1} plot optimistically chooses the
 best type conversion step.
In a program with @math{N} modules, a programmer has up to @math{N} type conversion steps to choose from,
 some of which may not lead to a @deliverable[] configuration.
For example, there are six configurations with exactly one typed module in @bm[suffixtree]
 but only one of these is @deliverable{2}.
