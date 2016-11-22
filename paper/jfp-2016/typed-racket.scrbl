#lang scribble/base

@require[
  "benchmark.rkt"
  "common.rkt"
  "typed-racket.rkt"
  "util.rkt"
  (only-in racket/list first last)
  (only-in gtp-summarize/path-util add-commas)
  (prefix-in gtp: (only-in gtp-summarize/summary from-rktd configuration->overhead))
  (only-in gtp-summarize/bitstring natural->bitstring)
  (only-in racket/string string-join)
  (only-in racket/format ~r)
  (except-in gtp-summarize/lnm-parameters defparam)
]

@profile-point{sec:tr}
@title[#:tag "sec:tr"]{Evaluating Typed Racket}

As validation of the evaluation method, this section presents the results of applying it to a suite of @integer->word[(count-benchmarks)] benchmark programs.
This section documents our protocol for collecting performance lattice data and presents the derived overhead graphs.


@; -----------------------------------------------------------------------------
@profile-point{sec:tr:protocol}
@section[#:tag "sec:protocol"]{Experimental Protocol}

@; TODO table of changes
@;      (or put in APPENDIX)

@(define MIN-ITERS-STR "3")
@(define MAX-ITERS-STR "30")
@(define FREQ-STR "1.40 GHz")

To generate all configurations in a benchmark, we usually began with untyped code, manually created the typed configuration, and then automatically generated all mixed configurations from these versions.
There were five complications.

First, the addition of types occasionally required type casts or small refactorings.
For example, the expression @racket[(string->number s)] had type @racket[(U Complex #f)] even when the programmer informally knew the expression would never produce a complex number.
To avoid similar losses of precision, we added appropriate runtime checks.
As another example, the @bm[quad] program called a library function to partition a @racket[(Listof (U A B))]
 into a @racket[(Listof A)] and a @racket[(Listof B)] using a predicate for values of type @racket[A].
Typed Racket could not prove that values which failed the predicate had type @racket[B], thus we refactored the call site.

Second, the @bm[fsm], @bm[synth], and @bm[quad] benchmarks were originally typed programs.
The benchmarks re-use many of the original type annotations, but Typed Racket could not enforce some of these annotations across a type boundary.
For example, the core datatypes in the @bm[synth] benchmark are monomorphic because Typed Racket could not dynamically enforce parametric polymorphism on instances of an untyped structure.

Third, any contracts in untyped programs are represented as type annotations and in-line assertions in the derived benchmarks.@note{At the time, Typed Racket could not express contracts.}
The @bm[acquire] program in particular used contracts to ensure ordered lists with unique elements.
Such checks are explicit pre and post-conditions on functions in the @bm[acquire] benchmark.

Fourth, we inserted @emph{adaptor modules} between modules exporting a struct and their typed clients.
Adaptor modules contained only type annotations and ensured a canonical set of types in configurations where the underlying server module was untyped.
The assignment of canonical types was necessary because in Typed Racket each import of an untyped struct generates a unique datatype.
Two typed modules could not share instances of such an untyped struct definition unless they referenced a common import annotation.
It was possible to implement this sharing without the layer of indirection, but adaptor modules provided a uniform solution and did not introduce noticeable overhead.

Fifth, half the configurations in @bm[dungeon] do not run on versions 6.2 and 6.3 due to a defect in the way these versions proxy first-class classes.
For the purpose of our experiment, these configurations have ``infinite'' performance overhead.

All our measurements were taken on a Linux machine with two physical AMD Opteron 6376 2.3GHz processors and 128GB RAM.@note{The Opteron is a NUMA architecture.}
With the exception of @bm[quadBG] and @bm[quadMB],@note{The six datasets for @bm[quad] were collected using 30 cores, but consequently exhibit greater variation between trials. See @secref{sec:compare}.} the machine collected data with at most two of its 32 cores.
Each core ran at minimum frequency as determined by the @tt{powersave} CPU governor (approximately @|FREQ-STR|).

For every benchmark, our driver script spawned one green thread for each available core.
These green threads worked together to collect one running time for each of the benchmark's configurations.
To collect a single running time, a green thread selected a configuration at random, spawned a new subprocess to compile the configuration once and run it twice,@note{Threads used the Linux @tt{taskset} command to pin subprocesses to a dedicated core. The subprocesses invoked the Racket compiler and runtime using the standard settings.} and recorded the time for the second run.
Depending on the overall time needed to collect data for a benchmark, we repeated this process between @|MIN-ITERS-STR| and @|MAX-ITERS-STR| times per benchmark.
The overhead graphs in the next section use the mean of these iterations.

Our results are consistent with the data reported for Racket v6.2 using three different machines@~cite[tfgnvf-popl-2016].
We are thereby encouraged that the observed performance overhead is reproducible across machines and computing environments.
@Secref{sec:threats} reports threats to validity regarding our experimental protocol.
The online supplement to this paper contains both our experimental scripts and the full datasets.


@; -----------------------------------------------------------------------------
@profile-point{sec:plots}
@section[#:tag "sec:plots"]{Evaluating Performance}

@(render-lnm-plot
  (lambda (pict*)
    (define name*
      (for/list ([p (in-list pict*)]
                 [i (in-naturals)])
        (format "fig:lnm:~a" i)))
    (define get-caption
      (let ([N (length name*)])
        (lambda (i) @elem{@|GTP| overhead graphs (@id[i]/@id[N])})))
    (define NUMV (integer->word (length (*RKT-VERSIONS*))))
    (cons
      @elem{
        @(apply Figure-ref name*) present our results in a series of overhead graphs.
        As in @figure-ref{fig:suffixtree-plot}, the left column of figures are cumulative distribution functions for @deliverable[] configurations and the right column are cumulative distribution functions for @step["1" "D"] configurations.
        These plots additionally give data for three versions of Racket released between June 2015 and February 2016.
        Data for version 6.2 are thin red curves with short dashes.
        Data for version 6.3 are mid-sized green curves with long dashes.
        Data for version 6.4 are thick, solid, blue curves.
        The typed/untyped ratio for each version appears above each plot in the left column.

        @; -- overall, bleak picture
        @(let* ([num-bm (length ALL-BENCHMARKS)]
                [num-bm-str (integer->word num-bm)]
                [max-str (format "~a" (*MAX-OVERHEAD*))]
                [suffixtree-num-modules (integer->word 6)]
                [num-max-deliverable 9] ; TODO, use *MAX-OVERHEAD*
                [num-max-deliverable-str (integer->word num-max-deliverable)]
                [num-mostly-2-deliverable 6]
                [num-mostly-2-deliverable-str (integer->word num-mostly-2-deliverable)]
                [num-good-slope 8]
                [num-good-slope-str (integer->word num-good-slope)]
                [v-min (first (*RKT-VERSIONS*))]
                [v-max (last (*RKT-VERSIONS*))]
                [absolute-min-overhead "1.2"]
                [absolute-min-overhead-bm "synth"]
               )
          @elem{
            Many curves are quite flat; they demonstrate that gradual typing introduces large and widespread performance overhead in the corresponding benchmarks.
            Among benchmarks with fewer than @|suffixtree-num-modules| modules, the most common shape is a flat line near the 50% mark.
            Such lines imply that the performance of a family of configurations is dominated by a single type boundary.
            @; For instance, there is one type boundary in @bm[fsm] that adds overwhelming slowdown when present; all eight configurations with this boundary have over @|max-str| overhead.
            Benchmarks with @|suffixtree-num-modules| or more modules generally have smoother slopes, but five such benchmarks have essentially flat curves.
            The underlying message is that for many values of @math{D} between 1 and @|max-str|, few configurations are @deliverable{}.

            For example, in @integer->word[(- num-bm num-mostly-2-deliverable)] of the @|num-bm-str| benchmark programs, @emph{at most} half the configurations are @deliverable{2}.
            The situation is worse for lower (more realistic) overheads, and does not improve much for higher overheads.
            Similarly, there are ten benchmarks in which at most half the configurations are @deliverable{10}.

            The curves' endpoints describe the extremes of gradual typing.
            The left endpoint gives the percentage of configurations that run at least as quickly as the untyped configuration.
            Except for @bm[lnm], such configurations are a low proportion of the total.@note{@bm[sieve] is a degenerate case. Only its untyped and fully-typed configurations are @deliverable{1}.}
            The right endpoint shows how many configurations suffer over 20x performance overhead.
            @string-titlecase[num-max-deliverable-str] benchmarks have at least one such configuration.

            Moving from @math{k=0} to @math{k=1} in a fixed version of Racket does little to improve the number of @deliverable{} configurations.
            Given the slopes of the @math{k=0} graphs, this result is not surprising.
            One type conversion step can eliminate a pathological boundary, such as those in @bm[fsm] and @bm[zombie], but the overhead in larger benchmarks comes from a variety of type boundaries.
            Except in configurations with many typed modules, adding types to one additional module is not likely to improve performance.

            On the other hand, the newer versions of Typed Racket do exhibit less overhead.
            The three slopes for each benchmark are similar, but later versions typically have more @deliverable{} configurations for any fixed @math{D}.
            In particular @bm[kcfa], @bm[snake], and @bm[tetris] doubled the number of @deliverable{8} configurations between versions 6.2 and 6.4.

            The exception to this rule is @bm[forth], which suffers a performance regression between versions 6.2 and 6.3.
            This regression is due to an inaccurate optimization for class contracts that was fixed for version 6.3.

         @;   Finally, note that every benchmark has at least a handful of @deliverable{1.8} configurations.@note{The largest x-intercept is @|absolute-min-overhead|, in @|absolute-min-overhead-bm|.}
         @;   These configurations are a small percentage of the total, but perhaps developers or an automated tool can locate them and avoid the many high-overhead configurations.
          })
      }
      (for/list ([p (in-list pict*)]
                 [name (in-list name*)]
                 [i (in-naturals 1)])
        (figure name (get-caption i) p)))))


@; -----------------------------------------------------------------------------
@section[#:tag "sec:compare"]{Comparing Gradual Type Systems}

@; uncertain runtimes, uncertain proportions
@(let* ([S-6.2 (gtp:from-rktd (benchmark-rktd morsecode "6.2"))]
        [S-6.4 (gtp:from-rktd (benchmark-rktd morsecode "6.4"))]
        [num-mod (benchmark->num-modules morsecode)]
        [interesting-config (natural->bitstring 13 #:pad num-mod)]
        [c6.2 (gtp:configuration->overhead S-6.2 interesting-config)]
        [c6.4 (gtp:configuration->overhead S-6.4 interesting-config)]
        [sample-D-str (let ([diff (abs (- c6.4 c6.2))])
                        (~r (+ (min c6.2 c6.4) (/ diff 2)) #:precision '(= 1)))])
@list[@elem{
  Overhead plots summarize the high-level performance of gradual type systems, but do not quantify the uncertainty in measurements.
  @; @~cite[kj-ismm-2013].
  @Figure-ref{fig:exact-runtimes} demonstrates why uncertainty might be an issue.
  It plots our entire dataset for the @bm[morsecode] benchmark.
  The @math{x}-axis has sixteen discrete intervals, one for each @bm[morsecode] configuration.@note{Configuration 0 is untyped and configuration 15 is fully typed. The mapping from @exact{$i \in [1,15]$} to configurations is in the appendix.}
  The @math{y} axis measures running time in milliseconds.
  Each data point is one running time for one configuration.
  These runnings times are coded by the version of Racket that produced them;
   times for v6.2 are red triangles,
   times for v6.3 are green circles,
   and times for v6.4 are blue squares.
  The left-to-right order of points for each configuration and version of Racket is the order in which we obtained the measurements.
  Any trend in these points would imply that the measurements are @emph{not} independent.

  Compare @figure-ref{fig:exact-runtimes} with the overhead graph for @bm[morsecode] in @figure-ref{fig:lnm:1}.
  Both graphs reach the same conclusion; namely, runtimes for version 6.2 are typically the slowest, but overall performance between all three versions is similar.
  The overhead graph, however, summarizes each configuration by its mean runtime without communicating the variance between iterations evident in @figure-ref{fig:exact-runtimes}.
  Fortunately, this variance is low.
  Therefore conclusions drawn from the overhead plot are likely to hold in practice.

  @; "exact" lessons
  @; - clustered points, unimodal, left-to-right independent
  @; - triangles highest
  @; - have some uncertainty
  }

  @figure["fig:exact-runtimes" "Exact running times"
    @render-exact-plot[morsecode]
  ]
])
    @(define sample-D 2)
    @(define sample-confidence 98)

The challenge for performance evaluation is to quantify variance and significance in datasets with exponentially many configurations.
Equipping the performance metrics with confidence intervals provides a solution@~cite[n-ptrs-1937].
In essence, a confidence interval is a probable bound for the true value of an unknown parameter.
For our purposes, non-overlapping confidence intervals for two unknown parameters serve as evidence that the parameters are truly different.

@Figure-ref{fig:uncertainty} quantifies the uncertainty in the typed/untyped ratio (top plot) and the number of @deliverable[@id[sample-D]] configurations (bottom plot) for each of our benchmark programs.
The @math{x}-axis of both plots represents the @integer->word[(*NUM-BENCHMARKS*)] benchmarks, arranged left-to-right from smallest to largest performance lattice.
As before, each benchmark has data for @integer->word[(length (*RKT-VERSIONS*))] versions of Racket.
The @math{y}-axis of the top plot is the inverse of the typed/untyped ratio; that is, the performance of the untyped configuration divided by the performance of the typed configuration.
Higher @math{y}-values indicate better performance.
The @math{y}-axis of the bottom plot is the percent of configurations that are @deliverable[@id[sample-D]]; again, a larger percentage is better.

Every series of points in the top graph and every bar in the bottom graph is enclosed in an @tt{I}-shaped interval.
On the top graph, these intervals quantify variation among repeated samples of the typed/untyped ratio.
To be precise, each point in the top graph is the untyped runtime from one iteration of the benchmark divided by the typed runtime from the same iteration.
Ratios for subsequent iterations are arranged left-to-right within a column.
The confidence intervals are the @id[sample-confidence]% confidence intervals for each sequence of ratios;
 repeating this experiment and re-computing confidence intervals would, with high probability,
 produce intervals containing the true ratio.@note{``High probability'' is a far better mental model for interpreting @figure-ref{fig:uncertainty} than ``@id[sample-confidence]% confidence''.
  We say @id[sample-confidence]% because we use a critical value of 2.326, but these are not rigorous statistics.
  First, the so-called index method@~cite[f-arxiv-2006] we use to compute confidence intervals for ratios is intuitive, but ad-hoc compared to methods such as Fieller's@~cite[f-rss-1957].
  Second, a small overlap of (precise) @id[sample-confidence]% confidence intervals does not necessarily imply a lack of significant difference at the @~r[(* 0.01 (- 100 sample-confidence)) #:precision '(= 2)] confidence level@~cite[bfwc-pm-2005].}

On the bottom graph, the intervals quantify uncertainty in the number of @deliverable[@id[sample-D]] configurations.
Recall that the overhead graphs in @secref{sec:plots} count the number of configurations with mean runtime at most @math{D} times slower than the mean runtime of the untyped configuration.
The rectangles in @figure-ref{fig:uncertainty} give the same, mean-derived counts for @math{D=@id[sample-D]}.
These counts have some uncertainty because the true overhead of a configuration might lie above or below the overhead determined by its mean running time.@note{The slope of the overhead plots correlates to the risk of confusing true and mean overheads; a steep slope is a higher risk.}
Therefore, if a configuration's @emph{mean overhead} were less than @id[sample-D] but its @emph{true overhead} were greater than @id[sample-D], the mean-based count would over-approximate the number of @deliverable[@id[sample-D]] configurations.
Conversely, under-approximations are possible if the mean overhead exceeds the true overhead.
The error bars around these rectangles therefore count @deliverable[@id[sample-D]] configurations based on the upper and lower bounds of a @id[sample-confidence]% confidence interval derived from our sample overheads.
If the lower confidence bound for one configuration's sequence of ratios was @deliverable[@id[sample-D]] but the same configuration's mean overhead was not @deliverable[@id[sample-D]], the upper error bar would be an additional 1% higher.

As it turns out, there is only one configuration with mean overhead that wavers near the @id[sample-D]x mark.
Every other configuration is stable; furthermore, all configurations are stable for overheads of 1.2x, 1.6x, 8x, and 10x.
These results imply that our mean overheads have relatively small error bounds, such that no configuration's mean overhead is within the noise of the above-mentioned values of @math{D}.

These graphs provide evidence for stronger statements about the relative performance of the three implementations of Typed Racket.
For example:
@itemlist[
  @item{
    Version 6.4 has significantly lower (improved) typed/untyped ratios than the earlier versions on five benchmarks: @bm[mbta], @bm[zombie], @bm[dungeon], @bm[zordoz], and @bm[acquire].
  }
  @item{
    Version 6.4 has significantly more @deliverable[@id[sample-D]] configurations than version 6.2 on ten benchmarks, and significantly fewer on @bm[quadBG] benchmarks.
    The drastic improvements in the @bm[mbta] and @bm[gregor] benchmarks are due to recent enhancements to Racket's contracts.
    In fact, all benchmarks are improved by these enhancements, but many configurions still suffer more than 20x overhead.
  }
]
Similar graphs can assess the performance of any fixed configuration or value for @math{D}.

    @figure["fig:uncertainty" "Measuring Uncertainty"
      @(parameterize([*CONFIDENCE-LEVEL* sample-confidence])
         (render-uncertainty sample-D ALL-BENCHMARKS))
    ]

