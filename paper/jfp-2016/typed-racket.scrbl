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

@;As validation of the evaluation method, this section presents the results of applying it to the benchmark programs.
@;This section documents our protocol for collecting performance lattice data and presents the derived overhead graphs.


@; -----------------------------------------------------------------------------
@profile-point{sec:tr:protocol}
@section[#:tag "sec:protocol"]{Experimental Protocol}

@; TODO table of changes
@;      (or put in APPENDIX)

@(define MIN-ITERS-STR "3")
@(define MAX-ITERS-STR "30")
@(define FREQ-STR "1.40 GHz")

To generate all configurations in a benchmark, we usually began with untyped code, manually created the typed configuration, and then automatically generated all mixed configurations from these versions.
There were six complications.

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

Fifth, some benchmarks do not have the same module boundaries as the original programs.
The @bm[zombie], @bm[snake], and @bm[tetris] benchmarks use the module boundaries from @exact{@|PHIL|~@|etal|~(2014)} rather than the boundaries from the original programs.
The @bm[kcfa] benchmark is modularized according to comments in the original, single-module program.
The @bm[suffixtree], @bm[synth], and @bm[gregor] benchmarks each have a single file containing all their data structure definitions; the original programs defined these structures in the same module as the functions on the structures.
Lastly, the @bm[quadBG] benchmark inlines two data structure definitions as noted in @secref{sec:bm}.

Sixth, half the configurations in @bm[dungeon] do not run on versions 6.2 and 6.3 due to a defect in the way these versions proxy first-class classes.
For the purpose of our experiment, these configurations have ``infinite'' performance overhead.

All our measurements were taken on a Linux machine with two physical AMD Opteron 6376 2.3GHz processors and 128GB RAM.@note{The Opteron is a NUMA architecture.}
With the exception of @bm[quadBG] and @bm[quadMB],@note{The six datasets for @bm[quad] were collected using 30 cores, but consequently exhibit greater variation between trials. See the appendix.} the machine collected data with at most two of its 32 cores.
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
@section[#:tag "sec:plots"]{Evaluating Absolute Performance}

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
                [num-configs (*TOTAL-NUM-CONFIGURATIONS*)]
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
                [format-% (lambda (n) (format "~a%" (round (* 100 (/ n num-configs)))))]
                [lo (number->string (*LO*))]
                [hi (number->string (*HI*))]
                [lo-prop (format-% (deliverable* (*LO*) v-max ALL-BENCHMARKS))]
                [hi-prop (format-% (- num-configs (deliverable* (*HI*) v-max ALL-BENCHMARKS)))]
               )
          @elem{
            Many curves are quite flat; they demonstrate that gradual typing introduces large and widespread performance overhead in the corresponding benchmarks.
            Among benchmarks with fewer than @|suffixtree-num-modules| modules, the most common shape is a flat line near the 50% mark.
            Such lines imply that the performance of a family of configurations is dominated by a single type boundary.
            @; For instance, there is one type boundary in @bm[fsm] that adds overwhelming slowdown when present; all eight configurations with this boundary have over @|max-str| overhead.
            Benchmarks with @|suffixtree-num-modules| or more modules generally have smoother slopes, but five such benchmarks have essentially flat curves.
            The underlying message is that for many values of @math{D} between 1 and @|max-str|, few configurations are @deliverable{}.

            For example, in @integer->word[(- num-bm num-mostly-2-deliverable)] of the @|num-bm-str| benchmark programs, @emph{at most} half the configurations are @deliverable{2} on any version.
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

            In summary, the application of our evaluation method projects a negative image of Typed Racket's sound gradual typing.
            Only a small number of configurations in our benchmark suite run with low overhead; a mere @|lo-prop| of all configurations are @deliverable[lo] on Racket v@|v-max|.
            Many demonstrate extreme overhead; @|hi-prop| of all configurations are not even @deliverable[hi] on version @|v-max|.
          })
      }
      (for/list ([p (in-list pict*)]
                 [name (in-list name*)]
                 [i (in-naturals 1)])
        (figure name (get-caption i) p)))))


@; -----------------------------------------------------------------------------
@section[#:tag "sec:compare"]{Evaluating Relative Performance}

Although the absolute performance of Racket version 6.4 is underwhelming, it is a significant improvement over versions 6.2 and 6.3.
This improvement is manifest in the difference between curves on the overhead plots.
For example in @bm[gregor] (third plot in @figure-ref{fig:lnm:3}), version 6.4 has at least as many deliverable configurations as version 6.2 for any overhead on the @math{x}-axis.
The difference is greatest near @math{x=2}; in terms of configurations, over 60% of @bm[gregor] configurations are not @deliverable{2} on v6.2 but are @deliverable{2} on v6.4.
The overhead plots for many other benchmarks demonstrate a positive difference between the number of @deliverable{D} configurations on version 6.4 relative to version 6.2.

@Figure-ref{fig:scale:delta} makes the improvement of version 6.4 relative to version 6.2 explicit.
The plot consists of @integer->word[(*NUM-BENCHMARKS*)] purple lines, one for each benchmark.
These lines plot the difference between the curve for v6.4 and the curve for v6.2 on the corresponding overhead plot.
As expected, the line for @bm[gregor] (labeled @math{r}) has a large positive spike in its left half.

Out of the @integer->word[(*NUM-BENCHMARKS*)] benchmarks, fifteen show large improvements in @figure-ref{fig:scale:delta}.
These improvements are due to work by Findler and Sam Tobin-Hochstadt on Racket's contract system and Typed Racket's use of contracts to enforce static types.
In many cases Findler and Tobin-Hochstadt were able to derive a microbenchmark from one of the benchmark programs, implement changes that improved the performance of the microbenchmark, and consequently improve the performance of multiple programs in the benchmark suite.

@; TODO double-check that regression is true. Re-running typed config doesn't match my data file.
The @bm[forth] benchmark, however, shows a significant performance regression.
This is due to a bug in the implementation of class contracts in version 6.2.
In short, the bug would suppress the allocation of certain necessary class contracts.
With the bug fixed, @bm[forth] generates the necessary contracts but suffers additional performance overhead.


@(parameterize ([*RKT-VERSIONS* '("6.2" "6.4")]
                [*PLOT-HEIGHT* 180]
                [*PLOT-WIDTH* 430]
                [*PLOT-FONT-SCALE* 0.02]
                [*X-TICK-LINES?* #t])
 (list
  @figure["fig:scale:delta" @elem{Relative performance of v6.4 versus v6.2}
    (render-delta ALL-BENCHMARKS)
  ]
 ))
