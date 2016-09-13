#lang scribble/base

@require[
  "common.rkt"
  "util.rkt"
  "benchmark.rkt"
  "typed-racket.rkt"
  (only-in gtp-summarize/lnm-parameters *COLOR-OFFSET*)
]

@profile-point{sec:scale}
@title[#:tag "sec:scale"]{Scaling the Method}

@; - take 1/8, try the optimistic/pessimistic sampling
@; - predictions? so far not working
@;   - at least profile the untyped
@; - prune the lattice, like

@; - buy more machines, use a cluster

The fundamental limitation of our evaluation method is that it requires an exponential number of measurements.
Given one fixed type assignment for a program of @math{N} modules, there are @exact{$2^{N}$} configurations.
In general, if there are @math{M} ways to type each module, then there are @exact{$M^N$} configurations.

To date, we have explored three alternatives for scaling the method.
@; Our success with these alternatives has been mixed.
We invite other researchers in the field, especially those working with micro-level gradual type systems, to further pursue the alternatives described here or to suggest novel techniques for evaluating gradual type systems at scale.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:scale:par"]{Parallelizing the Experiment}
@; - embarrasingly parallel
@; - only delays the problem
@; - memory contention locally
@; - outliers on karst

A benchmark's configurations are independent from one another, so the simplest way to measure larger benchmarks is to parallelize the experiment.
Additional processors will not make our method scalable, but will enable benchmarks of approximately twenty modules.

We explored parallel measurements for this paper, but opted for a simple two-processor experimental protocol.
In support of our decision, we offer two cautionary datasets.
First is @figure-ref{fig:scale:morsecode}, exact running times for the @bm[morsecode] benchmark measured with sixteen cores running in parallel.
Each core measured ten runs of one configuration.
    @; TODO recompute data with the powersave governor
    @;      also use the "barrier" protocol 
Compared to the data in @figure-ref{fig:exact-runtimes}, there is significantly more variation between configurations.
Furthermore, consecutive iterations yield faster runtimes in many configurations.

These unstable measurements are due to memory contention.
It turns out, the Racket runtime does not fit in the non-shared space available to our machine's cores.
As faster configurations finished, the remaining configurations demonstrated a ``speedup'' because there was less chance of a cache conflict.

    @figure["fig:scale:morsecode" @elem{@bm[morsecode] dataset, collected by 16 parallel cores.}
      @(let* ([empty-rktd "./src/empty-dataset.rktd"]
              [mc25-rktd "./src/morsecode-6.4-25core.rktd"]
              [mc25 (benchmark 'mc25 "" 0 "" "" #f '() `(("6.4" . ,mc25-rktd)))])
         (parameterize ([*RKT-VERSIONS* '("6.4")]
                        [*COLOR-OFFSET* 3])
           (render-exact-plot mc25)))
    ]

@; cluster, conclusions
@; - not all nodes uniform
@; - even on one node, not getting perfectly stable measurements
@; (could explain this was purpose of experiment, but need to explain graph either way so IDK)

@; TODO recollect for v6.4
Second, @figure-ref{fig:scale:karst} presents data collected on Indiana University's high-throughput computing cluster.@note{@url{https://kb.iu.edu/d/bezu}}
Each column shows exact running times for one arbitrary configuration in a benchmark, normalized to each series' mean runtime.
Within a column, the three lines of vertically-arranged points are data from separate jobs on the cluster.
These jobs
 created a new folder on the cluster's filesystem;
 copied a Racket v6.6 executable to the new folder;
 copied and compiled code for each configuration to the new folder;
 and ran each configuration through twenty consecutive iterations.
Furthermore, each job executed on a reserved node in the cluster.

@Figure-ref{fig:scale:karst} demonstrates that collecting the same data with different cluster nodes may yield different results.
Moreover, data from a single cluster node contains more outliers than the @figure-ref{fig:uncertainty} data we collected with a desktop machine.
Neither observation invalidates the data, but together they convinced us to prefer running experiments on a reserved desktop.

@; more colors, more symbols, unique from v6.2--v6.4
@; Do same graph as "exact" plots, but colors for each node
@;  colors really don't matter, just use horizontal position
@; - wait wtf about exact runtimes? y-axis gonna be tall

  @figure["fig:scale:karst" @elem{Cluster data, Racket v6.4. See @figure-ref{fig:uncertainty} for @math{x}-axis legend.}
    @render-karst["./src/karst-cputime.csv"]
  ]


@; -----------------------------------------------------------------------------
@section[#:tag "sec:scale:rnd"]{Sampling the Lattice}
@; - optimistic, pessimistic, 5 randoms
@; - sample size 1/8 1/16


@; -----------------------------------------------------------------------------
@section[#:tag "sec:scale:pre"]{Predicting Performance}
@; - software product lines
@; - maxnew method, verymixed success
@;   - might as well use mean, or use untyped
@;   - challenge to isolate boundaries


@; @; -----------------------------------------------------------------------------
@; @section[#:tag "sec:scale:dev"]{Ask Developers for Help}
@; - ask developers how they would add types
@;   - perhaps to their own project
@; - check the git history of a Typed Racket project, see how conversion happened,
@;   - was each step along the way performant?
