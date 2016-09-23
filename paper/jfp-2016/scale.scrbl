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
@(define srs-samples-str (integer->word srs-samples))


The fundamental limitation of our evaluation method is that it requires an exponential number of measurements.
Given one fixed type assignment for a program of @math{N} modules, there are @exact{$2^{N}$} configurations.
In general, if there are @math{M} ways to type each module, then there are @exact{$M^N$} configurations.

To date, we have explored three alternatives for scaling the method.
@; Our success with these alternatives has been mixed.
We invite other researchers in the field, especially those working with micro-level gradual type systems, to further pursue these alternatives or propose other ways to scale.

@; - srs pretty effective
@; - great for streaming results, at least getting warning signs
@; - not great for definitive measurements, but like, you can get "definitive" envo prettyfast, mabe

Simple random sampling is an effective technique for approximating the number of @deliverable{} configurations in our larger benchmarks.
@Figure-ref{fig:scale:srs}, for example, approximates the number of @deliverable{} configurations with @math{N} (left column) and @math{3N} (right column) samples, where @math{N} is the number of modules in the benchmark.

Specifically, each plot has @integer->word[(+ 1 2 srs-samples)] lines.
The solid blue line is from @secref{sec:plots}; that is, the blue line plots the number of @deliverable{} configurations on Racket v6.4.@note{The blue line in the left column is identical to the blue line in the right column. The right column does @emph{not} plot @step[] configurations.}
The other lines plot the overhead in uniformly-sized samples.
For these lines, the @math{y}-axis is the proportion @emph{of the sample} that is @deliverable{} relative to the untyped configuration.
The dashed orange and black lines are not random samples; these lines are the best possible (orange) and worst possible (black) samples one could take.
The @|srs-samples-str| faint red lines are random samples taken without replacement.@note{Sampling with replacement yields similar results, but makes less sense if the experimenter's goal is to eventually measure all configurations.}

Despite the wide theoretical bounds (dashed lines) on the space of random samples, in practice random sampling yields data close to our measured overhead.
The samples in the left column generally follow the blue lines.
The samples in the right column are very close to the blue lines, so close that we would reach the same conclusions about the performance of gradual typing in Typed Racket if our measurements produced a red line instead of the blue ones.

Na@exact{\"i}ve random sampling is no substitute for actually measuring performance.
We absolutely @emph{do not} recommend language evaluators use such data to make claims about the fate of gradual typing.
Rather, we belive random sampling will help language evaluators quickly identify problematic type boundaries in a benchmark.
Our recommendation is to seek out pathologies by first measuring the the very top and bottom of a large performance lattice, then randomly sampling the middle.

  @(parameterize ([*RKT-VERSIONS* '("6.2" "6.4")]
                  [*NUM-SIMPLE-RANDOM-SAMPLES* srs-samples]
                  [*PLOT-HEIGHT* 140]
                  [*PLOT-WIDTH* 430]
                  [*PLOT-FONT-SCALE* 0.02]
                  [*LNM-WIDTH* (+ 3 (*LNM-WIDTH*))])
   (list
    @figure["fig:scale:srs-overhead" @elem{Approximating true overhead}
        (render-srs-sound (list tetris) (list sample-size-factor))]

    @figure["fig:scale:srs-precise" @elem{Approximating improvement of v6.4 over v6.2 (@id[(*NUM-SIMPLE-RANDOM-SAMPLES*)] samples)}
      (render-srs-precise (list tetris) (list sample-size-factor))
    ]
    (parameterize ([*PLOT-HEIGHT* 100]
                   [*PLOT-WIDTH* 210]
                   [*PLOT-FONT-SCALE* 0.04]
                   [*LNM-WIDTH* (- (*LNM-WIDTH*) 3)])
      @figure["fig:scale:srs-precise-all"  @elem{Approximating improvement (@id[(*NUM-SIMPLE-RANDOM-SAMPLES*)] samples)}
        (render-srs-precise (list snake acquire synth gregor) (list sample-size-factor))
      ]
    )))


