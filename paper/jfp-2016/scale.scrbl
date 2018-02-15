#lang scribble/base

@; == Caveats / Ideas
@; - at least measure top/bottom of lattice
@; - only tested 20x overhead, estimates for larger (rarer) overheads may be worse
@; - haven't tried guessing worst-case overhead

@require[
  "common.rkt"
  "util.rkt"
  "benchmark.rkt"
  "typed-racket.rkt"
  (except-in gtp-summarize/lnm-parameters defparam)
  (only-in gtp-summarize/path-util add-commas)
]

@profile-point{sec:scale}
@title[#:tag "sec:scale"]{Evaluation Method, Part II}

@(define srs-samples 5)
@(define sample-size-factor 10)
@(define snake-sample-size (* sample-size-factor (benchmark->num-modules snake)))
@(define large-bm* (for/list ([bm (in-list ALL-BENCHMARKS)]
                              #:when (< 5 (benchmark->num-modules bm)))
                     bm))

  @; plot library ~ 80 modules
  @; math library ~ 197 modules
The evaluation method of @secref{sec:method} does not scale to benchmarks with a large number of migratable modules.
Benchmarking a full performance lattice for a program with @math{N} such components requires @exact{$2^N$} measurements.
In practice, this limits an exhaustive evaluation of Typed Racket to programs with approximately 20 migratable modules.
An evaluation of micro-level gradual typing would be severly limited; depending on the definition of a migratable component, such an evaluation might be limited to programs with 20 functions.

Fortunately, simple random sampling can approximate the ground truth presented in @secref{sec:tr}.
Instead of measuring every configuration in a benchmark, it suffices to randomly sample a linear number of configurations and plot the overhead apparent in the sample.

@Figure-ref{fig:scale:srs-snake} plots the true performance of the @bm[snake] benchmark against confidence intervals@~cite[n-ptrs-1937] generated from random samples.
The plot on the left shows the absolute performance of @bm[snake] on version 6.2 (dashed red line) and version 6.4 (solid blue line).
The plot on the right shows the improvement of version 6.4  relative to version 6.2 (solid purple line).
Each line is surrounded by a thin interval generated from @integer->word[srs-samples] samples of @id[snake-sample-size] configurations each.

The plots in @figure-ref{fig:scale:srs-snake} suggest that the intervals provide a reasonable approximation of the performance of the @bm[snake] benchmark.
These intervals capture both the absolute performance (left plot) and relative performance (right plot) of @bm[snake].

@Figure-ref{fig:scale:delta-interval} provides evidence for the linear sampling suggestion of @figure-ref{fig:scale:srs-snake} using data for the @integer->word[(length large-bm*)] largest benchmarks in the @|GTP| suite.
The solid purple lines from @figure-ref{fig:scale:delta} alongside confidence intervals generated from a small number of samples.
Specifically, the interval for a benchmark with @math{N} modules is generated from @integer->word[srs-samples] samples of @exact{$@id[sample-size-factor]N$} configurations.
Hence the samples for @bm[lnm] use @id[(* 10 (benchmark->num-modules lnm))] configurations and the samples for @bm[quadMB] use @id[(* 10 (benchmark->num-modules quadMB))] configurations.
For every benchmark, the true relative performance (solid purple line) lies within the corresponding interval.
In conclusion, a language designer can quickly approximate performance by computing a similar interval.


@section{Statistical Protocol}

For readers interested in reproducing the above results, this section describes the protocol that generated @figure-ref{fig:scale:srs-snake}.
The details for @figure-ref{fig:scale:delta-interval} are analogous:

@itemlist[
@item{
  To generate one random sample, select @id[snake-sample-size] configurations (10 times the number of modules) without replacement and associate each configuration with its overhead from the exhaustive performance evaluation reported in @secref{sec:tr}.
  @; Sampling with replacement yielded similar results.
}
@item{
  To generate a confidence interval for the number of @deliverable{D} configurations based on @integer->word[srs-samples] such samples, calculate the proportion of @deliverable{D} configurations in each sample and generate a 95% confidence interval from the proportions.
  This is the so-called @emph{index method}@~cite[f-arxiv-2006] for computing a confidence interval from a sequence of ratios.
  This method is intuitive, but admittedly less precise than a method such as Fieller's@~cite[f-rss-1957].
  The two intervals in the left half of @figure-ref{fig:scale:srs-snake} are a sequence of such confidence intervals.
}
@item{
  To generate an interval for the difference between the number of @deliverable{D} configurations on version 6.4 and the number of @deliverable{D} configurations on version 6.2, compute two confidence intervals as described in the previous step and plot the largest and smallest difference between these intervals.

  In terms of @figure-ref{fig:scale:delta-interval} the upper bound for the number of @deliverable{D} configurations on the right half of @figure-ref{fig:scale:srs-snake} is the difference between the upper confidence limit on the number of @deliverable{D} configurations in version 6.4 and the lower confidence limit on the number of @deliverable{D} configurations in version 6.2.
  The corresponding lower bound is the difference between the lower confidence limit on version 6.4 and the upper confidence limit on version 6.2.
}
]

@(parameterize ([*NUM-SIMPLE-RANDOM-SAMPLES* srs-samples]
                [*COLOR-OFFSET* 3]
                [*RKT-VERSIONS* '("6.2" "6.4")])
  @figure["fig:scale:srs-snake" @elem{Approximating absolute performance}
    (render-srs-single snake sample-size-factor)
  ]
)

@(parameterize ([*RKT-VERSIONS* '("6.2" "6.4")]
                [*PLOT-HEIGHT* 140]
                [*PLOT-WIDTH* 440]
                [*PLOT-FONT-SCALE* 0.02]
                [*DELTA-SECTION-ALPHA* 0.6]
                [*NUM-SIMPLE-RANDOM-SAMPLES* srs-samples]
                [*TICKS-START-FROM* (- (length ALL-BENCHMARKS) (length large-bm*))])
 (list
  @figure["fig:scale:delta-interval" @elem{Approximating relative performance}
      (render-delta large-bm* #:sample-factor sample-size-factor #:sample-style 'interval)
  ]
 ))

