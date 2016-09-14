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

A benchmark's configurations are independent from one another, thus the simplest way to measure larger benchmarks is to parallelize the experiment.
Additional processors will not make our method scalable, but will enable benchmarks of approximately twenty modules.

We explored parallel measurements for this paper, but opted for a simple two-processor experimental protocol.
In support of our decision, we offer two cautionary datasets.
First is @figure-ref{fig:scale:morsecode}, exact running times for the @bm[morsecode] benchmark measured with sixteen cores running in parallel.
Each core measured one column of data; that is, ten runs of a fixed configuration.
    @; TODO recompute data with the powersave governor
    @;      also use the "barrier" protocol 
Compared to the data in @figure-ref{fig:exact-runtimes}, there is significantly more variation within columns.
Furthermore, consecutive iterations yield faster runtimes in many configurations.

These unstable measurements were due to memory contention.
The Racket runtime did not fit in the non-shared space on our machine's cores.
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
Each column contains exact running times for one arbitrary configuration in a benchmark.
Within a column, the three lines of vertically-arranged points are data from separate jobs on the cluster (normalized to the points' mean runtime).
Each job executed on a reserved node.
These jobs
 created a new folder on the cluster's filesystem;
 copied a Racket v6.6 executable to the new folder;
 copied and compiled code for each configuration to the new folder;
 and ran each configuration through twenty consecutive iterations.

The lesson of @Figure-ref{fig:scale:karst} is that collecting the same data with different cluster nodes yielded different results.
Moreover, data from a single cluster node contained more outliers than the data in @figure-ref{fig:uncertainty}, collected on a desktop machine.
Because of this instability, we preferred taking measurements from our reserved desktop.

  @figure["fig:scale:karst" @elem{Cluster data, Racket v6.4. See @figure-ref{fig:uncertainty} for @math{x}-axis legend.}
    @render-karst["./src/karst-cputime.csv"]
  ]


@; -----------------------------------------------------------------------------
@section[#:tag "sec:scale:rnd"]{Sampling the Lattice}
@; - optimistic, pessimistic, 5 randoms
@; - sample size 1/8 1/16
@; - hypothesis: need O(n) samples, maybe 10n or 8n
@;   n=8, cfgs=256, 10n=80

@(define srs-size-str "???")
@(define srs-samples 5)
@(define srs-samples-str (integer->word srs-samples))

Simple random sampling is an effective technique for approximating the number of @deliverable{} configurations in our larger benchmarks.
The plots in @figure-ref{fig:scale:srs}, for example, bound the possible overhead plots after sampling @|srs-size-str| configurations from the four largest benchmarks.
The solid blue line in each plot is the true proportion of @deliverable{} configurations, as shown in @secref{sec:plots}.
The dotted orange line above each blue line presents the proportion of @deliverable{} configurations considering only the untyped configuration and the @|srs-size-str| fastest configurations.
Likewise, the dotted black line below each blue line considers only the untyped configuration and the @|srs-size-str| slowest configurations.
Any random sample lies between these bounds; the faint blue lines on each plot are five such examples.

@todo{exp or linear?}
Whereas measuring a full performance lattice requires exponentially many measurements,
 the random samples in @figure-ref{fig:scale:srs} use only a linear number.

@todo{edge behavior}
sample only the first and last two levels.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:scale:pre"]{Predicting Performance}
@; - software product lines
@; - maxnew method, verymixed success
@;   - might as well use mean, or use untyped
@;   - challenge to isolate boundaries


@; @; -----------------------------------------------------------------------------
@; @section[#:tag "sec:scale:dev"]{Finding Practical Subspaces}
@; - ask developers how they would add types
@;   - perhaps to their own project
@; - check the git history of a Typed Racket project, see how conversion happened,
@;   - was each step along the way performant?

In many programs, there are subspaces in the performance lattice that serve as independently useful benchmarks.
When it is possible to do so, dividing large programs into relevant subspaces can drastically lower the size of the experiment.

The @bm[mbta] benchmark is technically a subspace, as the @emph{whole} program includes the 30-module @library{graph} library.
Future benchmarks might be partitioned into Java-style packages.
Perhaps each folder in a large benchmark is a reasonable, and reasonably-sized, benchmark.

If an application is already gradually typed, our method can apply to possible @emph{evolutions} of the code.
That is, instead of building a lattice from the untyped configuration, begin from current configuration and proceed to the typed configuration.
This strategy tests the hypothesis that the gradual type system supports incremental changes.
If there are too many untyped modules to exhaustively explore this subspace, the application's maintainers can possibly suggest modules they would never type.


