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

In order to explain our experimental setup, we take a closer look at
one benchmark, @tt{suffixtree}, and explain the pieces that are involved.

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
This delayed check is evaluated each time the imported value is used.

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

@figure*["fig:suffixtree" "Suffixtree performance lattice. Nodes are labeled with their normalized mean (top) and standard deviation (bottom)."
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

Following the definitions in section@secref{sec:fwk}, we can first determine the
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
have slowdowns that range drastically from 1.02x to 100x. Inspecting
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
The @tt{suffixtree} example demonstrates that although type information can enable certain optimizations, these improvements may be offset by the dynamic checks inserted at typed-untyped boundaries.
The net result of this tradeoff depends heavily on the structure of the program and the set of modules given type annotations.
Thus it remains to be seen whether the gradual typing promise---that any variation of typed and untyped modules will run---tends to yield programs that are indeed practical to run.

@(Figure-ref "fig:lnm1" "fig:lnm2") summarize our findings after testing all gradually-typed variations for each of our 12 benchmark programs.
Rather than displaying the lattice of results, we summarize the each benchmark's @step["L" "N" "M"] characteristics with a row of figures showing the proportion of variations that are acceptable for particular choices of @math{L}, @math{N}, and @math{M}.

@figure*["fig:lnm1" @list{@step["L" "N" "M"] results for the first 6 benchmarks. The x-axes measure overhead and the y-axes count variations.}
  @(let* ([data '(
                   ("sieve"        "./data/sieve-04-06.rktd")
                   ("echo"         "./data/echo.rktd")
                   ("morse-code"   "./data/morsecode-large-06-24.rktd") ;; Medium-sized morecode case
                   ("mbta"         "./data/mbta-04-25.rktd")
                   ("zo-traversal" "./data/zordoz-04-09.rktd")
                   ("suffixtree" "./data/suffixtree-06-10.rktd")
                   ;; ("lnm" "./data/lnm-06-22.rktd")
                  )])
     (data->pict data #:tag "1"))
]

@figure*["fig:lnm2" @list{@step["L" "N" "M"] results for the remaining benchmarks}
  @(let* ([data '(
                   ("kcfa"       "./data/kcfa-2015-06-25T13:48:52.rktd") ;; TODO need to re-run the LARGE one, row 111 of data is malformed
                   ("synth"      "./data/funkytown.rktd")
                   ("tetris"       "./data/tetris-large-06-20.rktd")
                   ("snake"      "./data/snake-large-06-20.rktd")
                   ("gregor"     "./data/gregor-05-11.rktd")
                   ("quad"       "./data/quad-placeholder.rktd")
                  )])
     (data->pict data #:tag "2"))
]

@subsection{Reading the Figures}
@; Describe the lines & units on the figures

@; line graphs are essentially histograms
The line graphs show the number of variations that are @emph{L-step} acceptable for a particular @math{L} and overhead factor.
Each line is the result of sampling @id[PARAM-NUM-SAMPLES] overheads linearly spaced along the x-axis.

Overhead factors range from 1x, indicating performance no worse than the untyped program, to a @id[PARAM-MAX-OVERHEAD]x slowdown compared to the untyped variation.
To put these slowdown factors in perspective, we draw a @exact{\color{ForestGreen!90!black}{green}} vertical line at @id[PARAM-N]x overhead and a @exact{\color{Goldenrod!65!black}{yellow}} vertical line at @id[PARAM-M]x as hypothetical upper-bounds for @math{N} and @math{M}.
Realistic choices for @math{N} and @math{M} would be much lower, perhaps 1.1x and 1.5x.

On each y-axis, we count the number of variations from 0 to @math{2^n}, where @math{n} is the number of modules in that row's benchmark.
The y-axes themselves are scaled to be the same height for all figures; in particular, we draw a @exact{\color{red}{red}} dashed line at the number corresponding to 60% of all variations in the program.
Below these red lines we consider gradual typing @emph{impractical}, in the sense that more than half of all variations have unacceptable performance.

Each column of figures shows results for a fixed value of @math{L} ranging between 0 and @id[PARAM-L], inclusive.
Thus the figures in the leftmost column simply count the number of variations with performance below a given overhead factor.
In contrast, the graphs in the rightmost column count all variations that are at most @id[PARAM-L] type-annotation steps away from a usable variation.
These counts are optimistic; for nonzero @math{L} and @math{n} modules, we search the entire space of @math{O(n!-(n-L)!)} reachable variations to find a neighbor with usable runtime.

Lastly, each row of figures is accompanied by a brief table of summary statistics.
These statistics include the number of modules in the program, the average overhead of the fully-typed variation (@exact{$\tau$}), and the overhead of the worst-case and average-case gradually typed variations.
Note that the worst and average case numbers do not include the fully-typed and untyped variations, and are calculated over all runtimes we observed rather than, say, the mean runtime for each variation.


@subsection{Discussion}
@; Due dilligence for each benchmark,
@; The "WHY" try to explain the performance.
@; The "PATH" comment on how difficult porting was

The ideal shape for these curves is a flat line at the top of the y-axis, indicating that all variations performed no worse than the original untyped program.
Of course the dynamic checks inserted by gradual type systems make this ideal difficult to achieve even with type-driven optimizations, so the next-best shape is a steep vertical line reaching the 100% count at a low x-value.
A steep slope from the 1x point means that a large proportion of all variations run within a small constant overhead.
For lines with lower gradients this small constant must be replaced with a larger overhead factor for the same proportion of variations to qualify as acceptable.

Given the wide x-axis range of overhead factors, we would expect that only the leftmost quarter of each graph shows any interesting vertical slope.
Put another way, if it were true that Typed Racket's sound gradual typing is reasonably practical but requires tuning and optimization, the data right of the 10x point should be nearly horizontal and well above the red dashed line for @math{L}=0.@note{Increasing @math{L} should remove pathologically-bad cases.}
This shape would suggest that although a small percentage of variations suffer an order of magnitude slowdown, the performance of most gradually-typed variations is within a (large) constant factor.

We now describe the shape of the results for each benchmark.
Our procedure is to focus on the left column, where @math{L}=0, and to consider the center and right column as rather drastic countermeasures to recover performance.


@parag{Sieve}
At @exact{$L$}=0, the @tt{sieve} benchmark appears dead in the water, as half of the 4 variations suffer extremely large overhead.
Increasing @exact{$L$}, however, shows that augmenting the two gradually-typed configurations with one additional typed module solves the performance issue.
This is our only ``perfect'' graph, in which every variation is within short reach of a variation that performs at least as well as the untyped program.

@; This benchmark is admittedly contrived, but proves an interesting point: pathologically-bad variations can be avoided if the programmer is able to identify tightly-connected modules and ensure there is no boundary between them.
@; WHY:
@; - tons of higher-order interaction because streams are lambdas
@; PATH: (easy)
@; - but in practice might be hard -- depending on the untyped library your untyped script maybe isn't safe


@parag{Echo}
The shape of the @tt{echo} graphs is ideal.
The sharp vertical line at @exact{$L$}=0 indicates that gradual typing introduces only a small overhead compared to the untyped program.
Indeed, the summary statistics for @tt{echo} confirm that the overall slowest running time we observed was within a 20% slowdown over the untyped baseline.

@; If all graphs were similar to this at @exact{$L$}=1, performance would not be a significant issue.
@; Even at @exact{$L$}=2, we could shift focus to identifying the good variations rather than finding a new implementation strategy.

@; WHY
@; - little goes across the boundary. client/server communicate over ports
@; PATH (easy)
@; - very small program


@parag{Morse code}
The @tt{morse-code} benchmark also shows excellent performance.
At @math{L}=0 three variations perform at least as well as the untyped program, and the worst-case overhead is below 3x.
Increasing @math{L} raises the y-intercept of the lines, which matches the observation given that the fully-typed @tt{morse-code} runs faster than the original program.
Adding more type annotations helps improve this benchmark's performance.

@; WHY
@; - Very little inter-module communication
@; PATH (easy)
@; - small APIs (levenshtein was 300 lines for 1 export)
@; - only 2 modules really did things, the others were data(+parser) & main


@parag{MBTA} @;fixed version
The @tt{mbta} benchmark is nearly a steep vertical line, but for one flat area.
This implies that a boundary (or group of boundaries) accounts for a 3x slowdown, such that the set of variations where these boundaries connect typed and untyped modules all experience similar overhead.

@; WHY
@; - run-t and t-graph are tightly coupled
@; PATH (easy)
@; - small API, even with objects


@parag{ZO Traversal}
The lines for @tt{zo-traversal} are fairly steep, but not as drastic as the lines for @tt{morse-code} or even @tt{mbta}.
More interestingly, half the variations suffer a 2x overhead even as @exact{$L$} increases.
This behavior is explained by the summary numbers: because the fully-typed variation incurs a 4x overhead, the ability to convert additional modules rarely helps reach a more performant variation.

@; WHY
@; - the data is untyped, and this script is just an interface to that data
@; - funny consequence: adding types just makes things worse
@; PATH (easy)
@; - HUGE bottleneck typing the zo structs
@;   lots to do (62 structs, two zo-traversal functions for each)
@; - afterwards, straightforward (at least for the author)


@parag{Suffixtree}
At @exact{$L$}=0, @tt{suffixtree} shows the worst performance characteristics of all our benchmarks.
Over half the gradually-typed variations hit a performance wall and are not usable at any realistic overhead.
Increasing @exact{$L$}, however, drastically improves this picture.
Thus although most variations suffer large performance overhead, they are in theory close to a variation with much better performance.

@; WHY
@; - explained in the in-depth, below
@; PATH (hard)
@; - lots of continuations and letrec, (one cont. instatiated with Values was rejected by TR)
@; - module structure not bad


@; @parag{L-NM}
@; Our script for processing and plotting experimental data is a Typed Racket success story.
@; The fully-typed version performs much better than the untyped one, and gradual typing at worst introduces modest overhead.


@parag{K-CFA}
@; Control-flow analyses typically run slowly, and the implementation in our benchmark is poor even in comparison.
@; Without typed/untyped boundaries, our benchmark takes almost a minute to analyze a small arithmetic expression for @exact{$k$}=2 evaluation steps.
@; k-CFA is not a good application for gradual typing! Then again it's not much of a real program either

The @tt{kcfa} benchmark has a very jagged shape, implying that @exact{$N/M$}-usability is not a helpful tradeoff for this program.
At @exact{$L$}=0, selecting an @exact{$N$} strongly influences the proportion of acceptable variations for small values of @exact{$M$}.
This is expecially true for @exact{$N$} between 1x and 6x overhead, and remains true even after increasing @exact{$L$} to 1; however at @exact{$L$}=2 the performance problem is apparently solved (assuming a method of finding the performant variations).

@; WHY
@; inefficient algorithm
@; recursive struct hierarchy (though underlying types are simple)
@; - later structs contain lists and hashtables
@; - opaques might make it all better
@; PATH (easy)
@; - organized & simple project
@; - code was originally 1 module and educational


@parag{Synth}
The @tt{synth} benchmark performs well when fully-typed, but is significantly worse when gradually typed.
Over half the variations suffer an overhead of more than 20x.
Increasing @math{L} does increase the slopes of the lines, meaning a larger number of variations become usable for a fixed @math{N}/@math{M} pair, but gradual typing still introduces a large overhead.
Even at @math{L}=2 only 30% of all variations lie in reach of a point with at most 3x slowdown.

@; WHY
@; - original had poor typed/untyped performance,
@; - math library is documented to be bad for untyped interaction
@;   - (probably, not confirmed) tightly coupled module structure
@;   - complex array type : function with rectangular domain
@; PATH (hard)
@; - had to re-type files often, was easier to bottom-up
@;   (may have just been Ben's inexperience)
@; - array functions were all polymorphic, made for difficult boundaries
@; heavy use/export of macros


@parag{Tetris}
Like @tt{suffixtree}, the @tt{tetris} benchmark is a success story for increasing @exact{$L$}.
When @exact{$L$}=0 we see that half of all modules are within 6x overhead, but the rest are more than 20x worse than the untyped program.
The ``good half'', however, is apparently spread throughout the lattice and reachable in few steps from many other variations.

@; WHY
@; - where is the heavy boundary?
@; - why is this different from snake?
@; PATH (easy)
@; - like snake, simple types + small + full contracts


@parag{Snake}
The @tt{snake} benchmark has similar performance characteristics to @tt{synth}.
Most gradually-typed variations suffer more than 20x overhead and increasing @exact{$L$} helps somewhat, but still one must accept approximately 6x overhead before the majority of variations qualify as usable.

@; WHY
@; - (probably) tightly-coupled module structure?
@; - anyway, it's interesting that synth was not an isolated problem
@; - also interesting that it's not exactly tetris
@; PATH (easy)
@; - simple types, small project, fully contracted (it was already a contract benchmark)


@parag{Gregor}
Despite being a large benchmark, @tt{gregor} performs reasonably well even when gradually typed.
The worst-case slowdown of 6x is quite good compared to the other large benchmarks, and the steep vertical slope is close to ideal.

@; WHY
@; Contracts are all on simple types, pycket or soft contracts could do great things here
@; - structures all contain simple types (may as well be tuples)
@; - no higher-order functions, just simple -> simple contracts
@; - fanciest: optional args
@; PATH (easy)
@; - not bad. Library already had contracts
@; - intricate module structure, but again most things had an api



@parag{Quad}
@; TODO
PROBABLY BAD

@; WHY
@; - the ocm struct has vector & functions
@; - many quad types, often recursive
@; PATH (hard?)
@; - hard to tell, was already typed
@; - recovering API was VERY HARD; macros often generated definitions
@; - typing hyphenate (one untyped module) was tricky -- had to replace 'parititon' with 2 filters
