#lang scribble/base

@require["common.rkt"]

@require[pict
         racket/file
         racket/vector
         math/statistics
         (only-in racket/match match-define)
         "render-lnm.rkt"
         "../tools/data-lattice.rkt"]

@title[#:tag "sec:tr"]{Evaluating Typed Racket} @; , Classical

Measuring the running time for the performance lattices of our chosen dozen
of benchmarks means compiling, running, and timing hundreds of thousands of
configurations. Each configuration is run 30 times to ensure that the
@;
@;@margin-note*{cite the random number paper?}
@;
timing is not affected by random factors; some configurations take minutes
to run. Presenting and analyzing this wealth of data poses a separate
challenge all by itself.

This section present our measurements.
The first subsection discusses one benchmark in detail, demonstrating how we
 create the configurations, how the boundaries affect the performance of
 various configurations, and how the Typed Racket code base limits the
 experiment.  The second subsection explains how we present the data in
 terms of the definitions of section@secref{sec:fwk}.  The last subsection
 discuss the results for all benchmarks.

@parag{Experimental setup}
Due to the high resource requirements of evaluating 
the performance lattices, experiments were run on a cluster consisting of a
Machine A with 12 physical Xeon E5-2630 2.30GHz cores and 64GB RAM, Machine B
with 4 physical Core i7-4790 3.60GHz cores and 16GB RAM, and a set of Machines C
@;from the Northeastern University Discovery Cluster@note{@url{http://nuweb12.neu.edu/rc/?page_id=27}}
with identical configurations of 20 physical Xeon E5-2680 2.8GHz cores
with 64GB RAM. All machines run a variant of Linux. The following
benchmarks were run on machine A: @tt{morse-code} and @tt{lnm}.
The following were run on machine B: @tt{sieve} and @tt{kcfa}.
The following were run on machine C: @tt{suffixtree}, @tt{snake},
@tt{synth}, @tt{tetris}, and @tt{gregor}.
@;; FIXME edit this when all are run
For each configuration we report the average of 30 runs.
@; TODO: talk about warm up?  
@; TODO:Compilation v. interpreteration v. jitting?
All of our runs use a single core with green threads where applicable.
We did some sanity checks and were able to validate that performance differentials reported
in the paper were not affected by the choice of machine.

@; -----------------------------------------------------------------------------
@section{Suffixtree in Depth}

To illustrate the key points of the experiments, this section describes
one of the benchmarks, @tt{suffixtree}, and explains the setup and
its timing results in detail.

@tt{Suffixtree} consists of six modules: @tt{data} to define label and
tree nodes, @tt{label} with functions on suffixtree node labels,
@tt{lcs} to compute Longest-Common-Subsequences, @tt{main} to apply
lcs to data, @tt{structs} to create and traverse suffix tree nodes,
@tt{ukkonen} to build suffix trees via Ukkonen's algorithm. Each
module is available with and without type annotations.  Each configuration
thus links six modules, some of them typed and others untyped.

@; TODO Why does this look so bad? Too much space at end of table
@; TODO <jan>Can this figure go to the bottom of the column? </jan>
@; @figure["fig:purpose-statements" "Suffixtree Modules"
@; @tabular[#:sep @hspace[2]
@; (list (list @bold{Module} @bold{Purpose})
@; (list @tt{data.rkt}    "Label and tree node data definitions")
@; (list @tt{label.rkt}   "Functions on suffixtree node labels")
@; (list @tt{lcs.rkt}     "Longest-Common-Subsequence implementation")
@; (list @tt{main.rkt}    "Apply lcs to benchmark data")
@; (list @tt{structs.rkt} "Create and traverse suffix tree nodes")
@; (list @tt{ukkonen.rkt} "Build whole suffix trees via Ukkonen's algorithm"))]]


@figure*["fig:suffixtree" 
          @list{Performance lattice (labels are slowdowns).}
  @(let* ([vec (file->value SUFFIXTREE-DATA)]
          [vec* (vector-map (λ (p) (cons (mean p) (stddev p))) vec)])
     (make-performance-lattice vec*))
]

For typed modules, a programmer equips each structure, function, and class
with types. Modules provide their exports together with types, so that the
type checker can cross-check modules. A typed @defterm{client} module may import
values from an untyped @defterm{server} module, which means that the
corresponding @racket[require] specifications must be equipped with
types. Consider this example:
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(require 
  (only-in "label.rkt" 
            make-label 
            sublabel 
            ... 
            vector->label/s))
))
@;%
The server module is called @tt{label.rkt}, and the client imports specific
 values, e.g., @tt{make-label}.  This specification is replaced with a
 @racket[require/typed] specification where each imported identifier is
 typed:
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(require/typed/check "label.rkt" 
 [make-label
  (-> (U String (Vectorof (U Char Symbol))) Label)]
 [sublabel 
  (case-> 
    (-> Label Index Label)
    (-> Label Index Index Label))]
 ...
 [vector->label/s
  (-> (Vectorof (U Char Symbol)) Label)])
))
@; 

The types in a
@racket[require/typed] specification are compiled into run-time checks and
contracts for the values that flow into the module. For example, if some
imported variable is declared to be a @tt{Char}, the check @racket[char?]
is performed as the value flows across the module boundary. Higher-order
types (functions, objects, or classes) become contracts that are wrapped
around the imported value and which check each future interaction of this
value with its context.

The performance costs of gradual typing thus consist of allocation of
wrapper and run-time checks. Moreover, the compiler has to assume that
any value could be wrapped, so cannot generate direct field access code
as would be done in a statically typed language.

Since our evaluation setup calls for linking typed modules to both typed
and untyped server modules, depending on the configuration, we replace
@racket[require/typed] specifications with @racket[require/typed/check]
versions. This new syntax can determine whether the server module is typed
or untyped. It installs dynamic checks and contracts if the server module
is untyped, and it ignores the annotation if the server module is typed.
As a result, typed modules function independently of the rest of the
modules in a configuration.


@; -----------------------------------------------------------------------------
@parag{Performance Lattice.}

@Figure-ref{fig:suffixtree} shows the performance lattice annotated with the
  timing measurements. The lattice displays each of the modules in the
  program with a shape.  A filled black shape means the module is typed, an
  open shape means the module is untyped. The shapes are ordered from left
  to right and correspond to the modules of @tt{suffixtree} in alphabetical
  order: @tt{data}, @tt{label}, @tt{lcs}, @tt{main}, @tt{structs}, and
  @tt{ukkonen}.

 For each configuration in the lattice, the ratio is
 computed by dividing the average timing of the typed program by
 the untyped average. The figure omits standard deviations
 as they small enough to not affect the discusion.

The fully typed configuration (top) is @emph{faster} than the fully untyped
 (bottom) configuration by around 30%, which puts the typed/untyped ratio at 0.7. This is
 easily explained by Typed Racket's optimizations such as specialization of
 arithmetic operations, improved field accesses, and elimination of some
 bounds checks@~cite[thscff-pldi-2011]. When the optimizer is turned off,
 the ratio goes back up to 1. 


Sadly, the performance improvement of the fully typed configuration is the
 only good part of this benchmark. Almost all partially typed configurations
 exhibit slowdowns ranging from 0.7x to 105x. Inspection of the lattice
 clarifies several points about these slowdowns: @itemlist[

@item{Adding type annotations to the @tt{main} module neither subtracts or
 adds much overhead because it is a driver module that is not coupled to
 other modules.}


@item{Adding types to any of the workhorse modules---@tt{data}, @tt{label},
@; TODO -- check if 35x is correct
 or @tt{structs}---while leaving all other modules untyped causes slowdown of
 at least 35x. These modules make up
 tightly coupled clique. Laying down a type-untyped boundary to separate
 this clique causes many crossings of values, with associated
 contract-checking cost.}

@item{Inspecting @tt{data} and @tt{label} further reveals that the latter
 depends on the former through an adaptor module. This adaptor introduces a
 contract boundary when either of the two modules is untyped. When both
 modules are typed but all others remain untyped, the slowdown is reduced
 to about 13x.
@;  TODO - check 13x

 The @tt{structs} module depends on @tt{data} in the same fashion.  Because
 @tt{structs} also depends on @tt{label}, the configuration in which both
 @tt{structs} and @tt{data} are typed still has a large slowdown. When all
 three modules are typed, the slowdown is reduced to about 5x.}

@item{Finally, the configurations close to the worst slowdown case are
 those in which the @tt{data} module is left untyped but several of the
 other modules are typed. This makes sense given the coupling noted
 above; the contract boundaries induced between the untyped @tt{data} and
 other typed modules slow down the program.  The module structure diagram
 for @tt{suffixtree} in @figure-ref{fig:bm} corroborates the presence of
 this coupling. The rightmost node in that diagram corresponds to the
 @tt{data} module, which has the most in-edges in that particular
 graph. We observe a similar kind of coupling in the simpler @tt{sieve}
 example, which consists of just a data module and its client.}
]

@; TODO check numbers
The reason @tt{suffixtree} is bad news for gradual typing is because of
performance ``valleys'' in which a maintenance programmer can get stuck.
Consider that we start with the untyped program, and for some reason choose
to add types to @tt{label}. The program slows down by 88x. Without any
guidance, a developer may choose to then type @tt{structs} and see the
program slow down to 104x.  After that typing @tt{main} (104x), @tt{ukkonen}
(99x), and @tt{lcs} (103x) do little to improve performance. It is only
when all the modules are typed that performance improves (0.7x).


@figure*["fig:lnm1" 
  @list{@step["L" "N" "M"] results for the first 6
  benchmarks. The x-axes measure overhead and the y-axes count configurations.} 
  @(let* ([data `(("sieve"        ,SIEVE-DATA)
                  ("morse-code"   ,MORSECODE-DATA)
                  ("mbta"         ,MBTA-DATA)
                  ("zo-traversal" ,ZORDOZ-DATA)
                  ("suffixtree"   ,SUFFIXTREE-DATA)
                  ("lnm"          ,LNM-DATA)
                  )])
     (data->pict data #:tag "1"))
]

@figure*["fig:lnm2" @list{@step["L" "N" "M"] results for the remaining benchmarks}
  @(let* ([data `(("kcfa"       ,KCFA-DATA)
                  ("snake"      ,SNAKE-DATA)
                  ("tetris"     ,TETRIS-DATA)
                  ("synth"      ,SYNTH-DATA)
                  ("gregor"     ,GREGOR-DATA)
                  ("quad"       ,QUAD-DATA))])
     (data->pict data #:tag "2"))
]

@; -----------------------------------------------------------------------------
@section{Experimental Results}

@Figure-ref["fig:lnm1" "fig:lnm2"]
summarize the results from exhaustively exploring the performance lattice
of our twelve benchmarks. Each row
reports the results for one program.  On the left, a table spells out the
typed/untyped ratio, the maximum overhead, and the average overhead. This is
followed by three graphs that show how many configurations impose an overhead
between 1x and 20x for three values of @math{L}: @math{0}, @math{1}, and
@math{2}.

The typed/untyped ratio is the slowdown or speed up of fully typed code over
untyped code. Values smaller than 1 indicate a speedup due to some of the
Typed Racket optimizations. Values larger than 1 are slowdowns caused by
interaction with untyped parts of the underlying Racket runtime.  The ratios
ranges between 0.28x (@tt{lnm}) and 3.22x (@tt{zo-traversal}).

The maximum overhead is computed by finding the running time of the slowest configuration and
dividing it by the running time of the untyped version. The average overhead is obtained by
computing the average over all configurations (excluding the top and bottom
ones) and dividing it by the running time of the untyped configuration. Maximum overheads range
from 1.25x (@tt{lmn}) to 168x (@tt{tetris}).  Average overheads range from
0.6x (@tt{lmn}) to 68x (@tt{tetris}). The slowdowns reported in the
partially typed configurations come from contracts and checks performed as
untyped code interacts with typed code.

Here is how to read the three cumulative performance graphs. The x-axis
represents the slowdown over the untyped program (from 1x to
@id[PARAM-MAX-OVERHEAD]x).  The y-axis is a count of the number of
configurations (from @math{0} to @math{2^n}) scaled so all graphs are the
same height. The blue curves show how many configurations have less than a
given slowdown.  They are, by definition, monotonically increasing because
the more overhead is allowed, the more configurations satisfy the
condition. The ``ideal'' result would be a flat line at a graph's top, this
would mean that all configuration are as fast (or faster) as the untyped
one.  The ``worst'' case scenario is a flat line at the graph's bottom,
indicating that all configuration are more than 20x slower than the untyped
one. For ease of comparison between graphs, a dashed
(@exact{\color{red}{red}}) horizontal line indicates 60% point of all
configurations.  As a rule of thumb, curves that climb faster are better than
curves with a smaller slope.

@;Each line was obtained by measuring @id[PARAM-NUM-SAMPLES] overheads 
@;linearly spaced along the x-axis.

Our method defines @deliverable{N} and @usable["N" "M"] as key
functions for measuring the quality of a gradual type system.  The
cumulative performance graphs display the values of parameters @math{N} and
@math{M} as, respectively, a @exact{\color{ForestGreen!90!black}{green}} and
a @exact{\color{Goldenrod!65!black}{yellow}} vertical line. For this
experiment we have chosen values of 3x and 10x. They are rather liberal,
most production contexts would not tolerate anything higher than 2x (and
some would object to any slowdown). Thus, for any program, the value of
@deliverable{3} is the value of the y-axis where the blue line intersects
the green one.  This is how many configurations are deliverable.  Similarly
for @usable["3" "10"], its value is the difference of the intercepts.
Higher values for both of these measures are better.

Lastly, the figures show cumulative performance graphs for different values
of @math{L} (from 0 to 2).  The interpretation of @math{L} = 1 and 2 are as
follows. For each configuration, we search the entire space of
@math{O(n!-(n-L)!)}  reachable configurations to find a neighbor with usable
running time.  If the
top configuration (fully typed) is reachable, it is included in the set. Clearly the
number of steps is bounded by the height of the performance lattice. Thus,
for example, @tt{sieve} which has only two modules can get an ideal result
in one step.

@; -----------------------------------------------------------------------------
@section[#:tag "sec:all-results"]{Interpretation}

@; Due dilligence for each benchmark,
@; The "WHY" try to explain the performance.
@; The "PATH" comment on how difficult porting was

As mentioned, the ideal shape for these curves is a flat line at the top of the y-axis.
Of course the dynamic checks inserted by gradual type systems make this
ideal difficult to achieve even with type-driven optimizations, so the
next-best shape is a steep vertical line reaching the 100% count at a low
x-value.  A steep slope from the 1x point means that a large proportion of
all configurations run within a small constant overhead.  For lines with
lower gradients this small constant must be replaced with a larger overhead
factor for the same proportion of configurations to qualify as acceptable.

Given the wide x-axis range of overhead factors, we would expect that only
the leftmost quarter of each graph shows any interesting vertical slope.
Put another way, if it were true that Typed Racket's sound gradual typing
is reasonably practical but requires tuning and optimization, the data
right of the 10x point should be nearly horizontal and well above the red
dashed line for @math{L}=0.@note{Increasing @math{L} should remove
pathologically-bad cases.}  This shape would suggest that although a small
percentage of configurations suffer an order of magnitude slowdown, the
performance of most gradually-typed configurations is within a (large) constant
factor.

We now describe the shape of the results for each benchmark.  Our procedure
is to focus on the left column, where @math{L}=0, and to consider the
center and right column as rather drastic countermeasures to recover
performance.


@parag{Sieve} At @exact{$L$}=0, the @tt{sieve} benchmark is dead in the
water, as both partially typed configuration have more than 20x overhead.
Increasing @exact{$L$}, however, shows unsurprisingly that the fully typed
configuration is one step away.  This is our only ``perfect'' graph, in
which every configuration is within reach of a configuration that performs
at least as well as the untyped program.

@; This benchmark is admittedly contrived, but proves an interesting point:
@; pathologically-bad configurations can be avoided if the programmer is able
@; to identify tightly-connected modules and ensure there is no boundary
@; between them. 
@; WHY:
@; - tons of higher-order interaction because streams are lambdas
@; PATH: (easy)
@; - but in practice might be hard -- depending on the untyped library your 
@;   untyped script maybe isn't safe

@parag{Morse code} The @tt{morse-code} benchmark shows acceptable
performance.  At @math{L}=0 three configurations perform at least as well as
the untyped program, and the maximum overhead is below 2x.  Increasing
@math{L} raises the y-intercept of the lines, which matches the observation
given that the fully-typed @tt{morse-code} runs faster than the original
program. 

@; WHY
@; - Very little inter-module communication
@; PATH (easy)
@; - small APIs (levenshtein was 300 lines for 1 export)
@; - only 2 modules really did things, the others were data(+parser) & main

@;FIXED VERSION:

@parag{MBTA} The @tt{mbta} benchmark is nearly a steep vertical line, but
for one flat area.  This implies that a boundary (or group of boundaries)
accounts for a 3x slowdown, such that the set of configurations where these
boundaries connect typed and untyped modules all experience similar
overhead. Many configurations are @deliverable{2}, but, interestingly, not
the fully typed one. This explains why increasing @math{L} to 1 and 2 does not 
help much.
@;TODO Strange no? <jan>

@; WHY
@; - run-t and t-graph are tightly coupled
@; PATH (easy)
@; - small API, even with objects


@parag{ZO Traversal} The curves for @tt{zo-traversal} are fairly steep, but
not as drastic as @tt{morse-code} or @tt{mbta}.  Half the configurations
suffer a 2x overhead, even when @exact{$L$} increases.  This behavior is
again explained by the summary data: as the fully-typed configuration incurs
a 4x overhead, the ability to convert additional modules rarely helps reach
a more performant configuration. This benchmark is a case in which
programmers may have to
undo some of the type annotations to recover  performance.

@; WHY
@; - the data is untyped, and this script is just an interface to that data
@; - funny consequence: adding types just makes things worse
@; PATH (easy)
@; - HUGE bottleneck typing the zo structs
@;   lots to do (62 structs, two zo-traversal functions for each)
@; - afterwards, straightforward (at least for the author)


@parag{Suffixtree} At @exact{$L$}=0, @tt{suffixtree} is the worst of our
benchmarks.  Over half the gradually-typed configurations
are not usable at any realistic overhead.  Increasing @exact{$L$},
however, improves this picture.  Thus although most configurations suffer
large performance overhead, they are, in theory, close to a configuration
with better performance. Nevertheless there are still too many bad
configurations for comfort.

@; WHY
@; - explained in the in-depth, below
@; PATH (hard)
@; - lots of continuations and letrec, (one cont. instatiated with Values was rejected by TR)
@; - module structure not bad


@parag{LNM} The shape of the @tt{lnm} graphs is ideal.  The sharp vertical
line at @exact{$L$}=0 indicates that gradual typing introduces only a small
overhead compared to the untyped program.  Indeed, the summary for @tt{lnm}
confirm that the maximum overhead is 1.14x slower than the untyped
baseline. Furthermore, the fully typed performance is very good. Likely,
this is due to the heavy use of Racket's plotting library, which is typed.
This also suggest that the untyped program suffers from a performance
penalty due to contracts.

@; If all graphs were similar to this at @exact{$L$}=1, performance would not be a significant issue.
@; Even at @exact{$L$}=2, we could shift focus to identifying the good
@; configurations rather than finding a new implementation strategy. 

@; WHY
@; - little goes across the boundary. client/server communicate over ports
@; PATH (easy)
@; - very small program


@parag{K-CFA} The @tt{kcfa} benchmark has a jagged shape, implying
that @exact{$N/M$}-usability is not a helpful tradeoff for this program.
At @exact{$L$}=0, selecting an @exact{$N$} strongly influences the
proportion of acceptable configurations for small values of @exact{$M$}.  This
is especially true for @exact{$N$} between 1x and 6x, and remains
true even after increasing @exact{$L$} to 1; however at @exact{$L$}=2 it
is possible to find @deliverable{N} configurations from any configuration.

@; Control-flow analyses typically run slowly, and the implementation in
@; our benchmark is poor even in comparison. 
@; Without typed/untyped boundaries, our benchmark takes almost a minute to
@; analyze a small arithmetic expression for @exact{$k$}=2 evaluation
@; steps. 
@; k-CFA is not a good application for gradual typing! Then again it's not
@; much of a real program either 

@; WHY
@; inefficient algorithm
@; recursive struct hierarchy (though underlying types are simple)
@; - later structs contain lists and hashtables
@; - opaques might make it all better
@; PATH (easy)
@; - organized & simple project
@; - code was originally 1 module and educational

@parag{Snake} The @tt{snake} benchmark performs well when fully typed, but
most partially typed configurations suffer from more than 20x
overhead. Increasing @exact{$L$} helps somewhat, but still one must accept
6x overhead before the majority of configurations qualify as usable.

@; WHY
@; - (probably) tightly-coupled module structure?
@; - anyway, it's interesting that synth was not an isolated problem
@; - also interesting that it's not exactly tetris
@; PATH (easy)
@; - simple types, small project, fully contracted (it was already a contract benchmark)


@parag{Tetris} Like @tt{suffixtree}, the @tt{tetris} benchmark is a success
story for increasing @exact{$L$}.  When @exact{$L$}=0 we see that most of
the modules are more than 20x worse than the untyped program.  The good configurations
are apparently spread  throughout the lattice so that they are easily
reachable in two steps of typing.

@; WHY
@; - where is the heavy boundary?
@; - why is this different from snake?
@; PATH (easy)
@; - like snake, simple types + small + full contracts

@parag{Synth} The @tt{synth} benchmark is similar to @tt{snake}. Over half
the configurations suffer an overhead of more than 20x.  Increasing @math{L}
does increase the slopes of the lines, meaning a larger number of
configurations become usable for a fixed @math{N}/@math{M} pair, but gradual
typing still introduces a large overhead.  Even at @math{L}=2 only 30% of
all configurations lie in reach of a point with at most 3x slowdown.

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

@parag{Gregor} Despite being a large benchmark, @tt{gregor} performs
reasonably well even when gradually typed.  The worst-case slowdown of 5.2x
is quite good compared to the other large benchmarks, and the steep
vertical slope is encouraging.

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
Due to the massive size of this performance lattice (65536 configurations), this benchmark
takes an extraordinary amount of time to run to completion. As a result, we are missing
the final numbers for this benchmark except for the fully typed runtime. We will add the
remaining data in the final version of this paper.

@; WHY
@; - the ocm struct has vector & functions
@; - many quad types, often recursive
@; PATH (hard?)
@; - hard to tell, was already typed
@; - recovering API was VERY HARD; macros often generated definitions
@; - typing hyphenate (one untyped module) was tricky -- had to replace 'parititon' with 2 filters
