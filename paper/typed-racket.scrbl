#lang scribble/base

@require["common.rkt"]

@require[pict
         racket/file
         racket/vector
         math/statistics
         (only-in racket/match match-define)
         "render-lnm.rkt"
         "../tools/data-lattice.rkt"]

@title[#:tag "sec:tr"]{Evaluating Typed Racket, Classical}

Typed Racket as evaluated at NU 

@section{A Benchmark in Depth}

In order to explain our experimental setup, we take a closer look at the
@tt{suffixtree} benchmark and explain the pieces that are involved.

The benchmark consists of six main modules which are each available with
and without type annotations:
@tt{data.rkt}, @tt{label.rkt}, @tt{lcs.rkt}, @tt{main.rkt}, @tt{structs.rkt},
and @tt{ukkonen.rkt}.

Typed Racket distinguishes importing a value from a typed module and from an
untyped module. When importing from an untyped source, Typed Racket requires
that the user write down a type annotations using the @racket[require/typed]
form. This form explicitly adds a dynamic check to ensure that the imported
value truly satisfies that type. When the type is higher-order (e.g.,
function or class types), the dynamic check is delayed by wrapping the
imported value with a contract wrapper.

Since modules in our benchmark setup may be either typed or untyped depending
on the configuration, we modify all @racket[require/typed] imports to use a
@racket[require/typed/check] form that installs the dynamic check if the source
module is untyped and ignores the annotation if the source module is typed.
This allows typed modules in the benchmark to work independently of the
configuration of other modules.

For example, the original untyped @tt{lcs.rkt} module contains the import
statement @racket[(require "label.rkt")]. In the ordinary typed version,
this is rewritten to
@;
@racketblock[(require/typed "label.rkt"
                            [label->string (-> Label String)])]
@;
with a type annotation for each
imported value. Finally, in the benchmark instrumented version we replace
@racket[require/typed] with @racket[require/typed/check].

@subsection{Performance of Suffixtree}

@figure*["fig:suffixtree" "Suffixtree performance lattice. Nodes labeled with normalized mean (top) and standard deviation (bottom)."
  @(let* ([vec (file->value "../tools/data/suffixtree-2015-04-02.rktd")]
          [vec* (vector-map (λ (p) (cons (mean p) (stddev p))) vec)])
     (make-performance-lattice vec*))
]

To further understand the @tt{suffixtree} benchmark, we will look at its
performance lattice, shown in @figure-ref{fig:suffixtree}. The lattice
in the figure displays each of the modules in the program with a circle.
A filled black circle means the module is typed, an open circle means the
module is untyped. The circles are ordered from left to right and correspond
to the modules of @tt{suffixtree} in alphabetical order: 
@tt{data.rkt}, @tt{label.rkt}, @tt{lcs.rkt}, @tt{main.rkt}, @tt{structs.rkt},
and @tt{ukkonen.rkt}. Each configuration lists the average (on top) and standard deviation
(below) of the runtime of 30 iterations normalized to the untyped average.

Following the definitions in @secref{sec:fwk}, we can first determine the
typed/untyped ratio. The fully typed configuration (the top one)
for @tt{suffixtree} has the good property that
it is @emph{faster} than the fully untyped (bottom)
configuration by about 30%. This puts the ratio at about 0.7.

The typed configuration is faster due to Typed Racket's optimizer, which is
able to perform type-based specialization of arithmetic operations,
optimization of record field access, and elimination of some bounds
checking for vectors in @tt{suffixtree}. We determined that the optimizer
is responsible for the speedup by comparing the runtime of the fully typed
configuration with and without type-based optimization enabled.

Despite the speedup on the fully typed configuration, in-between configurations
have slowdowns that vary drastically from 1.02x to 100x. Inspecting
the lattice, several conclusions can be drawn about adding types to this
program. For example, adding type annotations to the @tt{main.rkt} module neither
subtracts or adds much overhead since it is a driver module that is not tightly
coupled to other modules. Comparatively, adding types to any of
@tt{data.rkt}, @tt{label.rkt}, or @tt{structs.rkt} from the fully
untyped configuration adds at least a 35x slowdown. This suggests that these
modules are highly coupled.

Inspecting @tt{data.rkt} and @tt{label.rkt} reveals, for example, that the
latter depends on the former through an adaptor module. The adaptor introduces
contract overheads when either of the two modules is untyped. When both
modules are typed but all others remain untyped, the slowdown is reduced to
about 12x.

The @tt{structs.rkt} module depends on @tt{data.rkt} in the same fashion.
However, since @tt{structs.rkt} also depends on @tt{label.rkt}, the configuration
in which both @tt{structs.rkt} and @tt{data.rkt} are typed still has a large
slowdown. When all three modules are typed, the slowdown is reduced to about
5x.

Another fact that the lattice shows is that the configurations whose
slowdown is closest to the worst case are those in which the @tt{data.rkt} module
is left untyped but several of the other modules are typed. This makes sense given
the coupling we observed above; the contract boundaries induced between the
untyped @tt{data.rkt} and other typed modules slow down the program.
The module structure diagram for @tt{suffixtree} in @figure-ref{fig:bm}
corroborates the presence of this coupling. The rightmost node in that
diagram corresponds to the @tt{data.rkt} module, which has the most in-edges in
that particular graph.


@section{Experimental Results}
@(Figure-ref "fig:lnm1" "fig:lnm2") summarize our findings after testing the performance lattice for each benchmark program.
Rather than displaying the entire lattice for each of the 12 programs, we summarize the @emph{L-N/M} characteristics of each program with a row of figures.

@figure*["fig:lnm1" @list{@emph{L-step N/M-usable} results for selected benchmarks. The x-axes measure overhead and the y-axes count variations.}
  @(let* ([data '(
                   ("synth"      "./data/funkytown.rktd")
                   ("gregor"     "./data/gregor-05-11.rktd")
                   ("kcfa"       "./data/kcfa-06-01.rktd") ;; TODO need to re-run the LARGE one, row 111 of data is malformed
                   ("quad"       "./data/quad-placeholder.rktd")
                   ("snake"      "./data/snake-04-10.rktd")
                   ("suffixtree" "./data/suffixtree-06-10.rktd")
                  )])
     (data->pict data #:tag "1"))
]

@figure*["fig:lnm2" @list{@emph{L-step N/M-usable} results for the rest of the benchmarks}
  @(let* ([data '(
                   ("echo"         "./data/echo.rktd")
                   ("morse-code"   "./data/morsecode-06-19.rktd")
                   ("mbta"         "./data/mbta-04-25.rktd")
                   ("sieve"        "./data/sieve-04-06.rktd")
                   ;; ("lnm" "./data/lnm-06-22.rktd")
                   ("tetris"       "./data/tetris-large-06-20.rktd")
                   ("zo-traversal" "./data/zordoz-04-09.rktd")
                  )])
     (data->pict data #:tag "2"))
]

@subsection{Reading the Figures}
@; Describe the lines & units on the figures

The line graphs show the number of variations that are @emph{L-step} acceptable for a particular @emph{L} and overhead factor.
Each line is the result of sampling @id[PARAM-NUM-SAMPLES] values linearly spaced along the x-axis.

Overhead factors range from 1x, indicating performance no worse than the untyped program, to a generous @id[PARAM-MAX-OVERHEAD]x slowdown compared to the untyped variation.
To put these slowdown factors in perspective, we draw a @exact{\color{ForestGreen}{green}} vertical line at @id[PARAM-N]x overhead and a @exact{\color{Goldenrod}{yellow}} vertical line at @id[PARAM-M]x as hypothetical upper-bounds for @emph{N} and @emph{M}.
Realistic choices for @exact{$N$} and @exact{$M$} would be much lower, but for the purposes of the figures we consider variations at or below 3x acceptable and variations between 3x and 10x to be usable.

On each y-axis, we count the absolute number of variations in the program.
The labels range from 0 variations to @exact{$2^n$} variations, where @exact{$n$} is the number of modules in that row's benchmark program.
The axes themselves are scaled to be the same height for all figures; in particular, we draw a @exact{\color{red}{red}} dashed line at the number corresponding to 60% of all variations in the program.

Each column of figures shows results for a fixed value of @emph{L} ranging between 0 and @id[PARAM-L], inclusive.
Thus the leftmost column simply counts the number of variations with performance below a given overhead factor.
In contrast, the graphs in the rightmost column count all variations that are at most @id[PARAM-L] type-annotation steps away from a usable variation.

Lastly, each row of figures is accompanied by a brief table of summary statistics.
These statistics include the number of modules in the program, the average overhead of the fully-typed variation (@exact{$\tau$}), and the overhead of the worst-case and average-case gradually typed variations.
Note that the worst and average case numbers do not include the fully-typed and untyped variations.


@subsection{Interpreting the Figures}
@; Judgments to make from the figures

The graphs help answer three high-level performance questions:
@itemlist[
@item{How many variations are deliverable for a fixed overhead @exact{$N$}?}
@item{How many variations become usable for an @exact{$M > N$}?}
@item{How does increasing the unit of work from one conversion step to 2 or 3 change the performance picture?}
]
The red dashed line represents our bottom line: supposing a developer has chosen appropriate values for @exact{$N$}, @exact{$M$}, and @exact{$L$}, we consider gradual typing @emph{impractical} unless at least 60% of variations are @emph{L-N/M}-acceptable.


We did not expect these graphs to be interesting---they should all look like @bold{gregor} or @bold{echo}.
@; shape we would hope for, given these X and Y

@subsection{All Benchmarks, in some depth}
@; Due dilligence for each benchmark,
@; TODO we should re-title and compress this section before submitting
@; TODO maybe add:
@; - reasons why, for performance
@; - porting story, how difficult is 2 paths

Brief descriptions of the graphs for each benchmark.


@;; First L-N/M figure
@parag{Synth}
The @tt{synth} benchmark performs well at the top and bottom of the lattice, but is significantly worse when gradually typed.
Over half the gradually typed variations suffer an overhead of more than 20x.
Increasing @exact{$L$} does increase the slopes of the lines, meaning a larger number of variations become usable for an @exact{$N/M$} pair, but gradual typing still introduces a large overhead.
Even at @exact{$L$}=2 only 30% of all variations lie in reach of a point with at most 3x slowdown.


@parag{Gregor}
Despite being a large benchmark, @tt{gregor} performs reasonably well even when gradually typed.
The worst-case slowdown of 6x is quite good compared to the other large benchmarks, and the steep vertical slope is also promising.
@; Contracts mostly simple types, I bet pycket or soft contracts could do great here


@parag{K-CFA}
@; Control-flow analyses typically run slowly, and the implementation in our benchmark is poor even in comparison.
@; Without typed/untyped boundaries, our benchmark takes almost a minute to analyze a small arithmetic expression for @exact{$k$}=2 evaluation steps.
@; k-CFA is not a good application for gradual typing! Then again it's not much of a real program either

The @tt{kcfa} benchmark has a very jagged shape, implying that @exact{$N/M$}-usability is not a helpful tradeoff for this program.
At @exact{$L$}=0, selecting an @exact{$N$} determines the proportion of usable variations for small values of @exact{$M$}.
This is expecially true for @exact{$N$} between 1x and 6x overhead, and remains true even after increasing @exact{$L$} to 1; however at @exact{$L$}=2 the performance problem is apparently solved (assuming a method of finding the performant variations).


@parag{Quad}
@todo{PROBABLY BAD}


@parag{Snake}
The @tt{snake} benchmark has similar performance characteristics to @tt{synth}.
Most gradually-typed variations suffer more than 20x overhead and increasing @exact{$L$} helps somewhat, but still one must accept at least a 6x overhead before 60% of variations may be considered usable.

@; because of a tightly-coupled module structure?
@; anywy, it's interesting that synth was not an isolated problem


@parag{Suffixtree}
At @exact{$L$}=0, @tt{suffixtree} shows the worst performance characteristics of all our benchmarks.
The slope is nearly a plateau, implying that over half the gradually-typed variations hit a performance wall and are not usable at any realistic value of @exact{$M$}.
Increasing @exact{$L$}, however, drastically improves this picture.
Although most variations suffer large performance overhead, they are in theory close to a variation with much better performance.


@;; Second L-N/M figure
@parag{Echo}
The shape of the @tt{echo} graphs is ideal.
The sharp vertical line at @exact{$L$}=0 indicates that all variations are deliverable for a small value of @exact{$N$}.
Naturally, the same shape is repeated for larger @exact{$L$}.

@; If all graphs were similar to this at @exact{$L$}=1, performance would not be a significant issue.
@; Even at @exact{$L$}=2, we could shift focus to identifying the good variations rather than finding a new implementation strategy.


@parag{Morse code}
The @tt{morse-code} benchmark also has excellent performance.
Moreover, it is an example of a real program with such performance, as opposed to the toy @tt{echo} example.


@parag{MBTA} @;fixed version
The @tt{mbta} benchmark is nearly a steep vertical line, but for one flat area.
This implies that a boundary (or group of boundaries) accounts for a 3x slowdown.


@parag{Sieve}
At @exact{$L$}=0, the @tt{sieve} benchmark appears dead in the water, as
half of the 4 variations suffer extremely large overhead.
Increasing @exact{$L$}, however, makes all variations usable at @exact{$N$}=1.

@; This benchmark is admittedly contrived, but proves an interesting point: pathologically-bad variations can be avoided if the programmer is able to identify tightly-connected modules and ensure there is no boundary between them.


@parag{Tetris}
Like @tt{suffixtree}, the @tt{tetris} benchmark is a success story for increasing @exact{$L$}.
When @exact{$L$}=0 we see that half of all modules are within 6x overhead, but the rest are more than 20x worse than the untyped program.
The ``good half'', however, is apparently spread throughout the lattice and reachable in few steps from many other variations.
Interestingly a high plateau remains at @exact{$L$}=1, presumably because there is a set of high-cost boundaries that dominate the performance of some variations.


@parag{ZO Traversal}
The lines for @tt{zo-traversal} are fairly steep, but not as drastic as the lines for @tt{morse-code} or even @tt{mbta}.
More interestingly, half the variations suffer a 2x overhead even as @exact{$L$} increases.
This behavior is explained by the summary numbers: because the fully-typed variation incurs some overhead, the ability to convert additional modules does not often help reach a more performant variation.

@; core problem: the data is always untyped
@; The @tt{zo-traversal} benchmark unfortunately performs worse as more modules are typed.

