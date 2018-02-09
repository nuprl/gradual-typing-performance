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

@; -----------------------------------------------------------------------------
@profile-point{sec:tr:protocol}
@section[#:tag "sec:protocol"]{Experimental Protocol}

@(define MIN-ITERS-STR "3")
@(define MAX-ITERS-STR "30")
@(define FREQ-STR "1.40 GHz")

@; @seclink is gross here, but it makes a good PDF
Sections @seclink["sec:plots"]{5.2} and @seclink["sec:compare"]{5.3} present the results of an exhaustive performance evaluation of the @integer->word[(*NUM-BENCHMARKS*)] benchmark programs on @integer->word[(length (*RKT-VERSIONS*))] versions of Racket.
The data is the result of applying the following protocol for each benchmark and each version of Typed Racket:
@itemlist[
@item{
  Select a random permutation of the configurations in the benchmark.@note{In principle, there is no need to randomize the order, but doing so helps control against possible confounding variables@~cite[mdhs-asplos-2009].}
}
@item{
  For each configuration: recompile, run once ignoring the result to control against JIT warmup, and run once more and record the running time.
  Use the standard Racket bytecode compiler, JIT compiler, and runtime settings.
}
@item{
  Repeat the above steps @exact{$N \ge 10$} times to produce a sequence of @math{N} running times for each configuration.
}
@item{
  Summarize each configuration with the mean of the corresponding running times.
}
]
@; two AMD Opteron 6376 2.3GHz processors and 128GB RAM

Specifically, a Racket script implementing the above protocol collected the data in this paper.
The script ran on a dedicated Linux machine; this machine has two physical AMD Opteron 6376 processors (with 16 cores each) and 128GB RAM.@note{The Opteron is a NUMA architecture and uses the @tt{x86_64} instruction set.}
For the @bm[quadBG] and @bm[quadMB] benchmarks, the script utilized 30 of the machine's physical cores to collect data in parallel.@note{Specifically, the script invoked 30 OS processes, pinned each to a CPU core using the @tt{taskset} Linux command, waited for each process to report a running time, and collected the results.}
For all other benchmarks, the script utilized only two physical cores.
Each core ran at minimum frequency as determined by the @tt{powersave} CPU governor (approximately @|FREQ-STR|).

The online supplement to this paper contains both the experimental scripts and the full datasets.
@Secref{sec:threats} reports threats to validity regarding the experimental protocol.
The appendix discusses the stability of individual measurements and reports the number of running times for each configuration and version of Racket.


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
        @(apply Figure-ref name*) present the results of measuring the benchmark programs in a series of overhead graphs.
        As in @figure-ref{fig:suffixtree-plot}, the left column of the figures are cumulative distribution functions for @deliverable[] configurations and the right column are cumulative distribution functions for @step["1" "D"] configurations.
        These plots include data for three versions of Racket released between June 2015 and February 2016.
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
            Such lines imply that the performance of a group of configurations is dominated by a single type boundary.
            @; For instance, there is one type boundary in @bm[fsm] that adds overwhelming slowdown when present; all eight configurations with this boundary have over @|max-str| overhead.
            Benchmarks with @|suffixtree-num-modules| or more modules generally have smoother slopes, but five such benchmarks have essentially flat curves.
            The overall message is that for many values of @math{D} between 1 and @|max-str|, few configurations are @deliverable{}.

            For example, in @integer->word[(- num-bm num-mostly-2-deliverable)] of the @|num-bm-str| benchmark programs, at most half the configurations are @deliverable{2} on any version.
            The situation is worse for lower (more realistic) overheads, and does not improve much for higher overheads.
            Similarly, there are ten benchmarks in which at most half the configurations are @deliverable{10}.

            The curves' endpoints describe the extremes of gradual typing.
            The left endpoint gives the percentage of configurations that run at least as quickly as the untyped configuration.
            Except for the @bm[lnm] benchmark, such configurations are a low proportion of the total.@note{The @bm[sieve] case is degenerate. Only its untyped and fully-typed configurations are @deliverable{1}.}
            The right endpoint shows how many configurations suffer over 20x performance overhead.@note{Half the configurations for @bm[dungeon] do not run on versions 6.2 and 6.3 due to a defect in the way these versions proxy first-class classes. The overhead graphs report an ``over 20x'' performance overhead for these configurations.}
            @string-titlecase[num-max-deliverable-str] benchmarks have at least one such configuration.

            Moving from @math{k=0} to @math{k=1} in a fixed version of Racket does little to improve the number of @deliverable{} configurations.
            Given the slopes of the @math{k=0} graphs, this result is not surprising.
            One type conversion step can eliminate a pathological boundary, such as those in @bm[fsm] and @bm[zombie], but the overhead in larger benchmarks comes from a variety of type boundaries.
            Except in configurations with many typed modules, adding types to one additional module is not likely to improve performance.

            In summary, the application of the evaluation method projects a negative image of Typed Racket's sound gradual typing.
            Only a small number of configurations in the benchmark suite run with low overhead; a mere @|lo-prop| of all configurations are @deliverable[lo] on Racket v@|v-max|.
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
The difference is greatest near @math{x=2}; in terms of configurations, over 80% of @bm[gregor] configurations are not @deliverable{2} on v6.2 but are @deliverable{2} on v6.4.
The overhead plots for many other benchmarks demonstrate a positive difference between the number of @deliverable{D} configurations on version 6.4 relative to version 6.2.

The plot of @figure-ref{fig:scale:delta} explicitly shows the improvement of version 6.4 over version 6.2.
It consists of @integer->word[(*NUM-BENCHMARKS*)] purple lines, one for each benchmark.
These lines plot the difference between the curves for v6.4 and v6.2 on the corresponding overhead plot.
For example, the line for @bm[gregor] (labeled @${\mathsf{r}}) demonstrates a large improvement in the number of @deliverable{2} configurations.
The plot also shows that fifteen of the @integer->word[(*NUM-BENCHMARKS*)] benchmarks significantly benefit from running on version 6.4.
Only the line for the @bm[forth] benchmark demonstrates a significant regression;
 the @bm[zombie] benchmark demonstrates a small regression due to an increase in the cost of type casts.

The improved performance of Racket version 6.4 is due to revisions of the contract system and Typed Racket's use of contracts to enforce static types.
In particular, the contract system allocates fewer closures to track the labels that Typed Racket uses to report type boundary errors.
The regression in the @bm[forth] benchmark is due to a bug in the implementation of class contracts in version 6.2.
This bug would suppress the allocation of certain necessary class contracts.
With the bug fixed, @bm[forth] generates the contracts but suffers additional performance overhead.

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
