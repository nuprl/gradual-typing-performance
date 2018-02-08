#lang scribble/base

@require[
  "common.rkt"
  "util.rkt"
  "benchmark.rkt"
  "typed-racket.rkt"
  benchmark-util/data-lattice
  (only-in racket/format ~r)
  (only-in racket/list take)
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
@title[#:tag "sec:method"]{Evaluation Method, Part I}

A performance evaluation of gradual type systems must reflect how programmers use such systems.
Since experience with Typed Racket shows that programmers frequently combine typed and untyped code within an application, an evaluation method must account for the large space of code between untyped and fully-typed programs.
These applications may also undergo gradual transitions that add or remove some type annotations.
In a typical evolution, programmers compare the performance of the modified program with the previous version.
If type-driven optimizations result in a performance improvement, all is well.
Otherwise, the programmer may need to address the performance overhead.
As they continue to develop the application, programmers repeat this process.
Again, an evaluation method must take into account gradual evolution to reflect practice.

The following subsections build an evaluation method from these observations in three steps.
First, @secref{sec:method:lattice} describes the space over which a performance evaluation must take place.
Second, @secref{sec:measurements} defines metrics relevant to the performance of a gradually typed program.
Third, @secref{sec:graphs} introduces a visualization that concisely presents the metrics on exponentially large spaces.


@; -----------------------------------------------------------------------------

@section[#:tag "sec:method:lattice"]{Performance Lattice}

@(define suffixtree-lattice-version "6.2")
@(define S (from-rktd (benchmark-rktd suffixtree suffixtree-lattice-version)))
@(define suffixtree-num-modules     (integer->word (get-num-modules S)))
@(define suffixtree-num-configs     (get-num-configurations S))
@(define suffixtree-num-configs-str (number->string suffixtree-num-configs))
@(define suffixtree-num-iters       (number->string (get-num-iterations S)))
@(define suffixtree-sample-D        2)
@(define suffixtree-num-D           ((D-deliverable suffixtree-sample-D) S))
@(define suffixtree-num-D-max       ((D-deliverable MAX-OVERHEAD) S))
@(define suffixtree-num-D-str       (integer->word suffixtree-num-D))
@(define suffixtree-sample-k        1)
@(define suffixtree-num-k           ((count-configurations/mean S suffixtree-sample-k) suffixtree-sample-D))
@(define suffixtree-num-k-str       (number->string suffixtree-num-k))
@(define suffixtree-tu-ratio        (format "~ax" (~r (typed/untyped-ratio S) #:precision '(= 1))))
@(define suffixtree-max             (*MAX-OVERHEAD*))
@(define suffixtree-num-max         (- suffixtree-num-configs ((D-deliverable suffixtree-max) S)))

    @figure*["fig:suffixtree-lattice" @elem{Performance overhead in @bm[suffixtree], on Racket v@|suffixtree-lattice-version|.}
      @(parameterize ([*LATTICE-CONFIG-MARGIN* 3]
                      [*LATTICE-LEVEL-MARGIN* 8]
                      [*LATTICE-FONT-SIZE* 10]
                      [*LATTICE-BOX-HEIGHT* 6]
                      [*LATTICE-BOX-WIDTH* 3]
                      [*LATTICE-BOX-SEP* 0]
                      [*LATTICE-BOX-TOP-MARGIN* 0]
                      [*LATTICE-TRUNCATE-DECIMALS?* #t]
                      [*LATTICE-BORDER-WIDTH* 0]
                      [*LATTICE-BOX-BOT-MARGIN* 1])
        (render-data-lattice suffixtree suffixtree-lattice-version))
    ]

The promise of Typed Racket's macro-level gradual typing is that programmers can add types to any subset of the modules in an untyped program.
In principle, this promise extends to third-party libraries and modules from the Racket runtime system, but in practice a programmer has no control over such modules.
Thus we distinguish between two kinds of modules: the @emph{migratable} modules that a programmer may add types to, and the @emph{contextual} modules in the software ecosystem, which remain unchanged.
A comprehensive performance evaluation must therefore consider the @emph{configurations} a programmer could possibly create given type annotations for each migratable module.
These configurations form a lattice, ordered by the subset relation on the set of typed modules in a configuration.

@Figure-ref{fig:suffixtree-lattice} demonstrates one such lattice for a program with @|suffixtree-num-modules| migratable modules.
The black rectangle at the top of the lattice represents the configuration in which all @|suffixtree-num-modules| modules are typed.
The other @id[(sub1 suffixtree-num-configs)] rectangles represent configurations with some untyped modules.

A given row in the lattice groups configurations with the same number of typed modules (black squares).
For instance, configurations in the second row from the bottom contain two typed modules.
These represent all possible ways of converting two modules in the untyped configuration to Typed Racket.
Similarly, configurations in the third row represent all possible configurations a programmer might encounter after applying three such @emph{type conversion steps} to the untyped configuration.
In general, let the notation @exact{$c_1 \rightarrow_k c_2$} express the idea that a programmer starting from configuration @exact{$c_1$} (in row @exact{$i$}) could reach configuration @exact{$c_2$} (in row @exact{$j$}) after taking at most @exact{$k$} type conversion steps (@exact{$j - i \le k$}).

Configurations in @figure-ref{fig:suffixtree-lattice} are furthermore labeled with their performance overhead relative to the untyped configuration on Racket version 6.2.
With these labels, a language implementor can draw several conclusions about the performance overhead of gradual typing in this program.
For instance, @|suffixtree-num-D-str| configurations run within a @id[suffixtree-sample-D]x overhead and @|suffixtree-num-k-str| configurations are at most @integer->word[suffixtree-sample-k] type conversion step from a configuration that runs within a @id[suffixtree-sample-D]x overhead.
High overheads are common (@id[suffixtree-num-max] configurations have over @id[suffixtree-max]x overhead), but the fully typed configuration runs faster (0.7x overhead) than the untyped configuration because Typed Racket uses the type annotations to compile efficient bytecode.

A labeled lattice such as @figure-ref{fig:suffixtree-lattice} is a @emph{performance lattice}.
The same lattice without labels is a @emph{configuration lattice}.
Practically speaking, users of a gradual type system explore configuration lattices and maintainers of such systems may use performance lattices to evaluate overall performance.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:measurements"]{Performance Metrics}

The most basic question, and least important, about a gradually typed language is
 how fast fully-typed programs are in comparison to their fully untyped relative.
In principle and in Typed Racket, static types enable optimizations and can serve in place of runtime tag checks.
The net effect of such improvements may, however, be offset by the runtime cost of enforcing type soundness.
Relative performance is therefore best described as a ratio, to capture the possibility of speedups and slowdowns.@;
@;
    @def[#:term "typed/untyped ratio"]{
     The typed/untyped ratio of a performance
      lattice is the time needed to run the top (fully typed) configuration divided by the
      time needed to run the bottom (untyped) configuration.
    }@;
@;
For users of a gradual type system, the important performance
 question is how much overhead their current configuration suffers.
 @; RELATIVE TO previous version (standardized as "untyped program")
If the performance overhead is low enough, programmers can release the
 configuration to clients.
Depending on the nature of the application, some developers might not accept any performance overhead.
Others may be willing to tolerate an order-of-magnitude slowdown.
The following parameterized definition of a deliverable configuration accounts for these varying requirements.@;
@;
    @def[#:term @list{@deliverable{}}]{
     A configuration
      is @deliverable{} if its performance is no worse than a
      factor of @math{D} slowdown compared to the untyped configuration.
     @;A program is @deliverable{} if all its configurations are @deliverable{}.
    }@;
@;
@; NOTE: really should be using %, but Sec 4 shows why stick to x
@;
If an application is currently in a non-@deliverable[] configuration,
 the next question is how much work a team must invest to reach a
 @deliverable[] configuration.
One coarse measure of ``work'' is the number of additional modules that must be annotated with types before performance improves.@;
@;
    @def[#:term @list{@step{}}]{
     A configuration @exact{$c_1$} is @step[] if @exact{$c_1 \rightarrow_k c_2$}
      and @exact{$c_2$} is @deliverable{}.
    }@;
@; @profile-point{sec:method:example}
The number of @step[] configurations captures the experience of a prescient programmer that converts the @exact{$k$} modules that maximize the performance improvement.

@(define sample-data
  (let* ([rng (vector->pseudo-random-generator (vector 0 1 2 3 4 5))]
         [approx (lambda (n) (let ([offset (/ n (random 1 10 rng))]) (if (zero? (random 0 1 rng)) (+ n offset) (- n offset))))]
         [mean+std* `#((20 . 0)
                       (50 . 0) (,(approx 50) . 0) (,(approx 50) . 0)
                       (30 . 0) (,(approx 30) . 0) (,(approx 30) . 0)
                       (10 . 0))]
         [mean (lambda (i) (car (vector-ref mean+std* i)))])
    (lambda (tag)
      (case tag
       [(c000) (mean 0)]
       [(c001) (mean 1)]
       [(c010) (mean 2)]
       [(c100) (mean 3)]
       [(c011) (mean 4)]
       [(c101) (mean 5)]
       [(c110) (mean 6)]
       [(c111) (mean 7)]
       [(all) mean+std*]
       [else (raise-user-error 'sample-data "Invalid configuration '~a'. Use e.g. c000 for untyped." tag)]))))
@(define (sample-overhead cfg)
  (ceiling (/ (sample-data cfg) (sample-data 'c000))))

    @figure["fig:demo-lattice" "Sample performance lattice"
      (parameterize ([*LATTICE-CONFIG-MARGIN* 30]
                     [*LATTICE-LEVEL-MARGIN* 8]
                     [*LATTICE-BOX-SEP* 0]
                     [*LATTICE-LINE-ALPHA* 0.8])
        (make-performance-lattice (sample-data 'all)))
    ]


Let us illustrate these terms with an example.
Suppose there is a project with
 three modules where the untyped configuration runs in @id[(sample-data 'c000)]
 seconds and the typed configuration runs in @id[(sample-data 'c111)] seconds.
Furthermore, suppose half the mixed configurations run in
  approximately @id[(sample-data 'c001)] seconds and the other half run in approximately @id[(sample-data 'c011)] seconds.
@Figure-ref{fig:demo-lattice} is a performance lattice for this hypothetical program.
The label below each configuration is its overhead relative to the untyped configuration.

@(let* ([tu-ratio (/ (sample-data 'c111) (sample-data 'c000))]
        [t-str @id[(sample-overhead 'c111)]]
        [g-overheads (map sample-overhead '(c011 c101 c110 c001 c010 c100))]
        [min-g (inexact->exact (apply max (take g-overheads 3)))]
        [max-g (inexact->exact (apply max g-overheads))])
  @elem{
    The typed/untyped ratio is @id[tu-ratio],
     indicating a performance improvement due to adding types.
    The typed configuration is also
      @deliverable[t-str]
      because it runs within a @elem[t-str]x
      slowdown relative to the untyped configuration.
    All mixed configurations are
      @deliverable[@id[max-g]], but only three are, e.g.,
      @deliverable[@id[min-g]].
    Lastly, the mixed configurations are all @step["1" t-str]
     because they can reach the typed configuration in one type conversion step.
  })

The ratio of @deliverable{D} configurations in a performance lattice is a measure of
 the overall feasibility of gradual typing.
When this ratio is high, then no matter how the application evolves, performance
 is likely to remain acceptable.
Conversely, a low ratio implies that a team may struggle to recover performance after
 typing a few modules.
Practitioners with a fixed performance requirement @math{D} can therefore use the number
 of @deliverable[] configurations to extrapolate the performance of a gradual type system.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:graphs"]{Overhead Graphs}

@(render-lnm-plot
  #:index 0
  #:rktd*** (list (list (list (benchmark-rktd suffixtree suffixtree-lattice-version))))
  (lambda (pict*)
    (list
      @figure*["fig:suffixtree-plot" @elem{Overhead graphs for @bm[suffixtree], on Racket v@|suffixtree-lattice-version|.}
        (car pict*)
      ]
      @; less than half of all @bm[suffixtree] configurations run within a @id[(*MAX-OVERHEAD*)]x slowdown.
      @elem{
        Although a performance lattice contains a comprehensive description of performance overhead, it does not effectively communicate this information.
        It is difficult to tell, at a glance, whether a program has good or bad performance relative to its users' requirements.
        Comparing the relative performance of two or more lattices is also difficult and is in practice limited to programs with an extremely small number of modules.

        The main lesson to extract from a performance lattice is the proportion of @step{} configurations for various @math{k} and @math{D}.
        In other words, this proportion describes the number of configurations (out of the entire lattice) that are at most @math{k} upward steps from a @deliverable{D} configuration.
        One way to plot this information is to fix a value for @math{k}, say @math{k=0}, and consider a set of values @exact{$d_0,\ldots,d_{n-1}$} for @math{D}.
        The set of proportions of @step["0" "d_i"] configurations defines a histogram with the value of @math{D} on the independent axis and the proportion of configurations on the dependent axis.

        @Figure-ref{fig:suffixtree-plot} demonstrates two such @emph{overhead plots}, summarizing the data in @figure-ref{fig:suffixtree-lattice}.
        @; TODO awkward
        Specifically, each plots the @|suffixtree-num-configs-str| configurations of a program called @bm[suffixtree] using data measured on Racket v@|suffixtree-lattice-version|.
        The plot on the left fixes @math{k=0} and plots the proportion of @step["0" "D"] configurations.
        The plot on the right fixes @math{k=1} and plots the proportion of @step["1" "D"] configurations.
        Both plots consider @math{@id[ALOT]} values of @math{D} evenly spaced between 1x and 20x.
        The line on each plot traces the underlying histogram.
        The x-axis is log scaled to focus on low overheads.
        Vertical ticks pinpoint the following values of @math{D}: 1.2x, 1.4x, 1.6x, 1.8x, 2x, 4x, 6x, 8x, 10x, 12x, 14x, 16x, and 20x.

        The plot on the left, in which @math{k=0}, confirms the observation made in @secref{sec:method:lattice} that @(id (round (* 100 (/ suffixtree-num-D suffixtree-num-configs))))% of the @|suffixtree-num-configs-str| configurations (@|suffixtree-num-D-str| configurations) run within a @id[suffixtree-sample-D]x overhead.
        For values of @math{D} larger than 2x, the proportion of @deliverable{D} configurations is slightly larger, but even at a @id[MAX-OVERHEAD]x overhead, this proportion is only @(id (round (* 100 (/ suffixtree-num-D-max suffixtree-num-configs))))%.
        The plot on the right shows that the proportion of @step["1" "D"] is typically twice as high as the proportion of @deliverable{} configurations for this benchmark.

        Clearly, this presentation scales to arbitrarily large programs because the @math{y}-axis plots the proportion of @deliverable{D} configurations; in contrast, a performance lattice contains exponentially many nodes.
        Furthermore, plotting the overhead for multiple implementations of a gradual type system on the same set of axes conveys their relative performance.
      }
)))

@subsection{Assumptions and Limitations}

Plots in the style of @figure-ref{fig:suffixtree-plot} rest on two assumptions and have two significant limitations, which readers must keep in mind as they interpret the results.

@; - assn: log scale
The @emph{first assumption} is that configurations with less than 2x overhead are significantly
 more practical than configurations with a 10x overhead or more.
Hence the plots use a log-scaled x-axis
 to simultaneously encourage fine-grained comparison in the 1.2x to 1.6x overhead range
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
The @emph{first limitation} of the overhead plots is that they do not report the number of types in a configuration.
The one exception is the fully-typed configuration; its overhead is given explicitly through the typed/untyped ratio above the left plot.

@; - limit: angelic choice
The @emph{second limitation} is that the @step{1} plot optimistically chooses the
 best type conversion step.
In a program with @math{N} modules, a programmer has at most @math{N} type conversion steps to choose from,
 some of which may not lead to a @deliverable[] configuration.
For example, there are six configurations with exactly one typed module in @figure-ref{fig:suffixtree-lattice} but only one of these is @deliverable{1}.
