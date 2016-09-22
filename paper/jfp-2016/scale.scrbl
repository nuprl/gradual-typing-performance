#lang scribble/base

@require[
  "common.rkt"
  "util.rkt"
  "benchmark.rkt"
  "typed-racket.rkt"
  (only-in gtp-summarize/lnm-parameters *COLOR-OFFSET* *NUM-SIMPLE-RANDOM-SAMPLES*)
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
We invite other researchers in the field, especially those working with micro-level gradual type systems, to further pursue these alternatives or propose other ways to scale.


@; @; -----------------------------------------------------------------------------
@; @section[#:tag "sec:scale:par"]{Parallelizing the Experiment}
@; @; - embarrasingly parallel
@; @; - only delays the problem
@; @; - memory contention locally
@; @; - outliers on karst
@; 
@; A benchmark's configurations are independent from one another, thus the simplest way to measure larger benchmarks is to parallelize the experiment.
@; Additional processors will not make our method scalable, but will enable benchmarks of approximately twenty modules.
@; 
@; We explored parallel measurements for this paper, but opted for a simple two-processor experimental protocol.
@; In support of our decision, we offer two cautionary datasets.
@; First is @figure-ref{fig:scale:morsecode}, exact running times for the @bm[morsecode] benchmark measured with sixteen cores running in parallel.
@; Each core measured one column of data; that is, ten runs of a fixed configuration.
@;     @; TODO recompute data with the powersave governor
@;     @;      also use the "barrier" protocol 
@; Compared to the data in @figure-ref{fig:exact-runtimes}, there is significantly more variation within columns.
@; Furthermore, consecutive iterations yield faster runtimes in many configurations.
@; 
@; These unstable measurements were due to memory contention.
@; The Racket runtime did not fit in the non-shared space on our machine's cores.
@; As faster configurations finished, the remaining configurations demonstrated a ``speedup'' because there was less chance of a cache conflict.
@; 
@;     @figure["fig:scale:morsecode" @elem{@bm[morsecode] dataset, collected by 16 parallel cores.}
@;       @(let* ([empty-rktd "./src/empty-dataset.rktd"]
@;               [mc25-rktd "./src/morsecode-6.4-25core.rktd"]
@;               [mc25 (benchmark 'mc25 "" 0 "" "" #f '() `(("6.4" . ,mc25-rktd)))])
@;          (parameterize ([*RKT-VERSIONS* '("6.4")]
@;                         [*COLOR-OFFSET* 3])
@;            (render-exact-plot mc25)))
@;     ]
@; 
@; @; cluster, conclusions
@; @; - not all nodes uniform
@; @; - even on one node, not getting perfectly stable measurements
@; @; (could explain this was purpose of experiment, but need to explain graph either way so IDK)
@; 
@; @; TODO recollect for v6.4
@; Second, @figure-ref{fig:scale:karst} presents data collected on Indiana University's high-throughput computing cluster.@note{@url{https://kb.iu.edu/d/bezu}}
@; Each column contains exact running times for one arbitrary configuration in a benchmark.
@; Within a column, the three lines of vertically-arranged points are data from separate jobs on the cluster (normalized to the points' mean runtime).
@; Each job executed on a reserved node.
@; These jobs
@;  created a new folder on the cluster's filesystem;
@;  copied a Racket v6.6 executable to the new folder;
@;  copied and compiled code for each configuration to the new folder;
@;  and ran each configuration through twenty consecutive iterations.
@; 
@; The lesson of @Figure-ref{fig:scale:karst} is that collecting the same data with different cluster nodes yielded different results.
@; Moreover, data from a single cluster node contained more outliers than the data in @figure-ref{fig:uncertainty}, collected on a desktop machine.
@; Because of this instability, we preferred taking measurements from our reserved desktop.
@; 
@;   @figure["fig:scale:karst" @elem{Cluster data, Racket v6.4. See @figure-ref{fig:uncertainty} for @math{x}-axis legend.}
@;     @render-karst["./src/karst-cputime.csv"]
@;   ]


@; -----------------------------------------------------------------------------
@section[#:tag "sec:scale:rnd"]{Sampling the Lattice}

@(define srs-samples 10)
@(define factors '(3 10))
@(define srs-samples-str (integer->word srs-samples))

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

    @figure["fig:scale:srs-sound" "Soundness of Random Samples"
      (parameterize ([*NUM-SIMPLE-RANDOM-SAMPLES* srs-samples]
                     [*RKT-VERSIONS* '("6.4")]
                     [*COLOR-OFFSET* 3])
        (render-srs-sound (list acquire tetris synth gregor) factors))
    ]

    @figure["fig:scale:srs-precise" "Precision of Random Samples"
      (parameterize ([*NUM-SIMPLE-RANDOM-SAMPLES* srs-samples]
                     [*RKT-VERSIONS* '("6.2" "6.4")]
                     [*COLOR-OFFSET* 4])
        (render-srs-precise (list acquire tetris synth gregor) factors))
    ]


@; -----------------------------------------------------------------------------
@; @section[#:tag "sec:scale:pre"]{Predicting Performance}
@; - software product lines
@; - maxnew method, verymixed success
@;   - might as well use mean, or use untyped
@;   - challenge to isolate boundaries

@; What can we report besides "failed attempt"? We didn't even have a STOP advert


@; @; -----------------------------------------------------------------------------
@; @section[#:tag "sec:scale:dev"]{Seeking Practical Subspaces}
@; @; - ask developers how they would add types
@; @;   - perhaps to their own project
@; @; - check the git history of a Typed Racket project, see how conversion happened,
@; @;   - was each step along the way performant?
@; 
@; In many programs, there are subspaces in the performance lattice that serve as independently useful benchmarks.
@; The @bm[mbta] benchmark is technically a subspace, as the whole program includes the 30-module @library{graph} library.
@; We also removed a few string-parsing modules from @bm[gregor] because Typed Racket could not dynamically enforce the ad-hoc polymorphism in these modules.
@; Incidentally, each module we removed halved the @bm[gregor] performance lattice.
@; Making such cuts made these benchmarks' evaluation feasible.
@; 
@; Similarly, if an application is already gradually typed, our method can apply to possible @emph{evolutions} of the code.
@; Instead of building a lattice relative to the untyped configuration, evaluation can begin from the current configuration and proceed to the typed configuration.
@; This strategy tests the hypothesis that the gradual type system in question supports incremental improvements.


