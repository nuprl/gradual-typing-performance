#lang scribble/base

@require[
  "common.rkt"
  "util.rkt"
  "benchmark.rkt"
  "typed-racket.rkt"
  (except-in gtp-summarize/lnm-parameters defparam)
]

@profile-point{sec:scale}
@title[#:tag "sec:scale"]{Scaling the Method}

@(define srs-samples 4)
@(define sample-size-factor 10)

  @; plot library ~ 80 modules
  @; math library ~ 197 modules
The evaluation method proposed in @secref{sec:method} does not scale to benchmarks with a large number of typeable components.
Benchmarking a full performance lattice for a program with @math{N} such components requires an exponential number of measurements.
Even with access to a server farm, exhaustively measuring programs with as few as 60 components is impractical.
We clearly need to simplify the method to feasibly determine the overhead of gradual typing in such programs.

This section shows that simple random sampling can approximate the ground truth presented in @secref{sec:tr}.
Specifically, it demonstrates that a linear number of samples generates, in practice, a good approximation to the presented overhead plots.
Overhead plots are intuitively a statement about expected values.
If 10% of the benchmarks are @deliverable{5}, then one in ten randomly sampled configurations will be @deliverable{5}.
The same holds for any value of @math{D}, so an overhead plot generated from one sample population should approximate the true overhead plot.

@(define tetris-sample-size (* sample-size-factor (benchmark->num-modules tetris)))

To demonstrate, @figure-ref{fig:scale:srs-overhead} plots the true overhead for the @id[(benchmark->num-configurations tetris)] @bm[tetris] configurations against overhead in samples of @id[tetris-sample-size] configurations.@note{The sample size is @id[sample-size-factor] times the number of modules in the @bm[tetris] benchmark. All plots in this section use analogous sampling rates.}
The dark red line is from @secref{sec:plots}; it plots the true overhead in @bm[tetris] on Racket v6.2.
The @integer->word[srs-samples] faint red lines plot the overhead in @integer->word[srs-samples] distinct samples of @id[tetris-sample-size] configurations each.
In particular, a single faint red line plots the proportion of @deliverable{} configurations in one group of @id[tetris-sample-size] configurations, selected without replacement.@note{By @emph{without replacement}, we mean that each sample has @id[tetris-sample-size] unique configurations; however, there may be duplicates between samples. At any rate, choosing truly random samples (with replacement) yields similar results.}
Similarly, the dark and faint blue lines plot true and sample overheads in Racket v6.4.

@Figure-ref{fig:scale:srs-overhead} thus shows that small, random populations can approximate the true overhead in a large performance lattice.
There is noticeable difference between the sample overhead and true overhead, but overall the samples @emph{tend towards} the true overhead.
To make this notion of a @emph{trend} in samples precise, we can plot the average number of @deliverable{} configurations across the samples along with a confidence interval showing the variation between samples.
As stated in @secref{sec:compare}, such a confidence interval estimates the @emph{likely} true overhead based on the sample measurements.
The bounds in @bm[tetris] are tight, and we have recorded similar bounds in all our benchmarks with at least eight modules (see @figure-ref{fig:scale:srs-precise-all}).

In fact, the bounds are tight enough to reflect the @emph{difference} in performance between two versions of Racket.
@Figure-ref{fig:scale:srs-precise} presents such data for the @bm[tetris] benchmark.
The dark purple line is the delta between the true overhead on v6.4 and the true overhead on v6.2.
The dashed brown line plots a sample delta; in particular, the brown line shows the average difference between v6.4 and v6.2 across @integer->word[srs-samples] sample populations.
The interval around the brown line shows the best and worst differences we can expect based on a 98% confidence interval.
Specifically, the upper end of the shaded interval is the difference between the upper confidence limit on our samples for v6.4 and the lower confidence limit on our samples for v6.2.
The lower end of the interval is the smallest probable difference between the versions, based on our lower limit for v6.4 and upper limit for v6.2.

@Figure-ref{fig:scale:srs-precise-all} plots similar data for benchmarks in the @|GTP| suite with at least eight modules.
In all cases, the mean difference among the @integer->word[srs-samples] sample populations is close to the true difference between the performance of the two Racket versions.
Furthermore, the confidence bounds around these means are tight, even for the largest benchmarks, and increasing the number of samples produces even tighter bounds.

@; == Caveats / Ideas
@; - at least measure top/bottom of lattice
@; - only tested 20x overhead, estimates for larger (rarer) overheads may be worse
@; - haven't tried guessing worst-case overhead

  @(parameterize ([*RKT-VERSIONS* '("6.2" "6.4")]
                  [*NUM-SIMPLE-RANDOM-SAMPLES* srs-samples]
                  [*PLOT-HEIGHT* 140]
                  [*PLOT-WIDTH* 430]
                  [*PLOT-FONT-SCALE* 0.02]
                  [*X-TICK-LINES?* #t]
                  [*LNM-WIDTH* (+ 3 (*LNM-WIDTH*))])
   (list
    @figure["fig:scale:srs-overhead" @elem{Approximating true overhead}
        (render-srs-sound (list tetris) (list sample-size-factor))]

    @figure["fig:scale:srs-precise" @elem{Approximating improvement of v6.4 over v6.2 (@id[(*NUM-SIMPLE-RANDOM-SAMPLES*)] samples)}
      (render-srs-precise (list tetris) (list sample-size-factor))
    ]
    (parameterize ([*PLOT-HEIGHT* 100]
                   [*PLOT-WIDTH* 220]
                   [*PLOT-FONT-SCALE* 0.04]
                   [*X-TICK-LINES?* #f]
                   [*LNM-WIDTH* (- (*LNM-WIDTH*) 3)])
      @figure["fig:scale:srs-precise-all"  @elem{Approximating improvement in the largest benchmarks (@id[(*NUM-SIMPLE-RANDOM-SAMPLES*)] samples)}
        (render-srs-precise (list snake acquire synth gregor) (list sample-size-factor))
      ]
    )))


