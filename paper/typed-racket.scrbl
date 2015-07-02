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
challenge all by itself. This section present our results.

The first subsection explains at a high level how we present the data in
terms of the definitions of section@secref{sec:fwk}. In the second one, we
discuss one benchmark in depth, @tt{suffixtree}, demonstrating how we create
the configurations, how the boundaries affect the performance of various
configurations, and how the Typed Racket code base limits the
experiment. Finally, subsection@secref{sec:all-results} describes the
results for all remaining benchmarks.

@parag{Experimental setup}
Due to the high resource requirements of evaluating 
the performance lattice, experiments were on a cluster consisting of a
Machine A with 12 physical Xeon E5-2630 2.30GHz cores and 64GB RAM, Machine B
with 4 physical Core i7-4790 3.60GHz cores and 16GB RAM, and a set of Machines C
@;from the Northeastern University Discovery Cluster@note{@url{http://nuweb12.neu.edu/rc/?page_id=27}}
with identical configurations of 20 physical Xeon E5-2680 2.8GHz cores
with 64GB RAM. All machines run a variant of Linux. The following
benchmarks are run on machine A: @tt{morse-code} and @tt{lnm}.
The following are run on machine B: @tt{sieve} and @tt{kcfa}.
The following are run on machine C: @tt{suffixtree}, @tt{snake},
@tt{synth}, @tt{tetris}, and @tt{gregor}.
@;; FIXME edit this when all are run
All of our programs are single threaded.
We did some sanity checks and were able to validate that performance differentials reported
in the paper were not affected by the choice of machine.



@; -----------------------------------------------------------------------------
@section{Suffixtree in Depth}

To illustrate the key points of the experiments, it is helpful to look
closely at one of the benchmarks, @tt{suffixtree}, and explain the setup and
the timing results in detail.

@tt{Suffixtree} consists of six modules (@tt{data.rkt} to define label and
tree nodes, @tt{label.rkt} with functions on suffixtree node labels,
@tt{lcs.rkt} to compute Longest-Common-Subsequences, @tt{main.rkt} to apply
lcs to data, @tt{structs.rkt} to create and traverse suffix tree nodes,
@tt{ukkonen.rkt} to build suffix trees via Ukkonen's algorithm).  Each
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
type checker can cross-check modules. A @defterm{client} module may import
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
 values (e.g. @tt{make-label}).  This specification is replaced with a
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

When a module imports values from an untyped source, the types in a
@racket[require/typed] specification are compiled into run-time checks and
contracts for the values that flow into the module. For example, if some
imported variable is declared to be a @tt{Char}, the check @racket[char?]
is performed as the value flows across the module boundary. Higher-order
types (functions, objects, or classes) become contracts which are wrapped
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

Following the methodology outlined in section@secref{sec:fwk}, we start by
 considering the typed/untyped ratio. For each configuration, the ratio is
 computed by dividing the average of 30 iterations of the typed program by
 the average of 30 untyped iterations.  The figure omits standard deviations
 as they small enough to not affect the discusion.

The fully typed configuration (top) is @emph{faster} than the fully untyped
 (bottom) configuration by around 30%, which puts the ratio at 0.7. This is
 easily explained by Typed Racket's optimizations such as specialization of
 arithmetic operations, improved field accesses, and elimination of some
 bounds checks@~cite[thscff-pldi-2011]. When the optimizer is turned off,
 the ratio goes back up to 1. 


Sadly, the performance improvement of the fully typed configuration is the
 only good part of this benchmark. Almost all partially typed configurations
 exhibit slowdowns ranging from 2% to 66x. Inspection of the lattice
 clarifies several points about these slowdowns: @itemlist[

@item{Adding type annotations to the @tt{main} module neither subtracts or
 adds much overhead because it is a driver module that is not coupled to
 other modules.}


@item{Adding types to any of the workhorse modules---@tt{data}, @tt{label},
@; TODO -- check if 35x is correct
 or @tt{structs}---causes slowdown of at least 35x. These modules make up
 tightly coupled clique. Laying down a type-untyped boundary to separate
 this clique causes many crossings of values across these contract
 boundaries.}

@item{Inspecting @tt{data} and @tt{label} further reveals that the latter
 depends on the former through an adaptor module. The latter introduces a
 contract boundary when either of the two modules is untyped. When both
 modules are typed but all others remain untyped, the slowdown is reduced
 to about 12x.
@;  TODO - check 12x

 The @tt{structs} module depends on @tt{data} in the same fashion.  Because
 @tt{structs} also depends on @tt{label}, the configuration in which both
 @tt{structs} and @tt{data} are typed still has a large slowdown. When all
 three modules are typed, the slowdown is reduced to about 5x.}

@item{Finally, the configurations close to the worst slowdown case are
 those in which the @tt{data} module is left untyped but several of the
 other modules are typed. This makes sense given the coupling we observed
 above; the contract boundaries induced between the untyped @tt{data} and
 other typed modules slow down the program.  The module structure diagram
 for @tt{suffixtree} in @figure-ref{fig:bm} corroborates the presence of
 this coupling. The rightmost node in that diagram corresponds to the
 @tt{data.rkt} module, which has the most in-edges in that particular
 graph. We observe a similar kind of coupling in the simpler @tt{sieve}
 example, which consists of just a data module and its client.}
]


@figure*["fig:lnm1" 
  @list{@step["L" "N" "M"] results for the first 6
  benchmarks. The x-axes measure overhead and the y-axes count variations.} 
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
                  ("synth"      ,SYNTH-DATA)
                  ("tetris"     ,TETRIS-DATA)
                  ("snake"      ,SNAKE-DATA)
                  ("gregor"     ,GREGOR-DATA)
                  ("quad"       ,QUAD-DATA))])
     (data->pict data #:tag "2"))
]

@; -----------------------------------------------------------------------------
@section{Experimental Results}

@Figure-ref["fig:lnm1" "fig:lnm2"] summarize our findings for each of our
twelve benchmark programs.  Each row reports the results for one benchmark,
expressed in the terminology of section@secref{sec:fwk}: the typed-untyped
ratio, a value for @math{N} and @math{M} to check on @math{N}-deliverable
and @math{N/M}-usable configurations, and three values for @math{L} to
determine how much extra work affects deliverability and usability.

On the left side in each row, a table spells out the typed/untyped
ratio, the maximal overhead that any configuration imposes over the running
time of the untyped version, and the average over all
configurations.@note{The worst and average case numbers do not include the
fully-typed and untyped variations, and are calculated over all runtimes we
observed rather than, say, the mean runtime for each variation.}

On the right side in each row, three line graphs show how many
configurations impose an overhead between 1x and 20x for three values of
@math{L}: @math{0}, @math{1}, and @math{2}.

The units on the x-axis are overhead factors. An overhead factor of 1x
means the performance is no worse than the untyped program. It goes all the
way to an overhead factor of @id[PARAM-MAX-OVERHEAD]x, which is that much
of a slowdown compared to the untyped configuration. Each line was obtained
by measuring @id[PARAM-NUM-SAMPLES] overheads linearly spaced along
the x-axis.

We put these overhead factors in perspective with two vertical lines: (a
@exact{\color{ForestGreen!90!black}{green}}) one for @math{N} at
@id[PARAM-N]x and (a @exact{\color{Goldenrod!65!black}{yellow}}) one for
@math{M} at @id[PARAM-M]x. These lines are extremely liberal upper-bounds
for @math{N} and @math{M};  realistic choices for @math{N} and @math{M}
would be much lower, perhaps 1.1x and 2x.

The units on the y-axis are plain counts from @math{0} to @math{2^n}, where
@math{n} is the number of modules in the benchmark. To simplify the table
layout, all the y-axes themselves are scaled to be the same height for all
figures. To make the y-dimensions comparable, the line graphs come with a
dashed (@exact{\color{red}{red}}) horizontal to mark the 60% line of all
program configurations. 

Each @math{L}-step @math{N/M}-usable figure stands for itself. The leftmost
graphs column simply count the number of variations with performance below
a given overhead factor.  In contrast, the rightmost graphs count all
variations that are at most @id[PARAM-L] type-annotation steps away from a
usable variation. These counts are optimistic; for nonzero @math{L} and
@math{n} modules, we search the entire space of @math{O(n!-(n-L)!)}
reachable configurations to find a neighbor with usable running time.

In sum, the graphs report the number of configurations that impose at most
a certain overhead. They are, by definition, monotonically increasing
because the more overhead is allowed, the more configurations satisfy the
condition. Hence, the key takeaway is the shape of the curve and how slowly or quickly
it climbs, and where it crosses the vertical @math{N} and @math{M} lines
and the horizontal 60% line. 

@; -----------------------------------------------------------------------------
@section[#:tag "sec:all-results"]{Discussion}

@; Due dilligence for each benchmark,
@; The "WHY" try to explain the performance.
@; The "PATH" comment on how difficult porting was

The ideal shape for these curves is a flat line at the top of the y-axis,
indicating that all configurations performed no worse than the original untyped
program.  Of course the dynamic checks inserted by gradual type systems
make this ideal difficult to achieve even with type-driven optimizations,
so the next-best shape is a steep vertical line reaching the 100% count at
a low x-value.  A steep slope from the 1x point means that a large
proportion of all configurations run within a small constant overhead.  For
lines with lower gradients this small constant must be replaced with a
larger overhead factor for the same proportion of configurations to qualify as
acceptable.

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


@parag{Sieve} At @exact{$L$}=0, the @tt{sieve} benchmark appears dead in
the water, as half of the 4 configurations suffer extremely large overhead.
Increasing @exact{$L$}, however, shows that augmenting the two
gradually-typed configurations with one additional typed module solves the
performance issue.  This is our only ``perfect'' graph, in which every
configuration is within short reach of a configuration that performs at least as
well as the untyped program.

@; This benchmark is admittedly contrived, but proves an interesting point:
@; pathologically-bad configurations can be avoided if the programmer is able
@; to identify tightly-connected modules and ensure there is no boundary
@; between them. 
@; WHY:
@; - tons of higher-order interaction because streams are lambdas
@; PATH: (easy)
@; - but in practice might be hard -- depending on the untyped library your untyped script maybe isn't safe

@parag{LNM} The shape of the @tt{lnm} graphs is ideal.  The sharp
vertical line at @exact{$L$}=0 indicates that gradual typing introduces
only a small overhead compared to the untyped program.  Indeed, the summary
statistics for @tt{lnm} confirm that the overall slowest running time we
observed was within a 25% slowdown over the untyped baseline. Furthermore,
the fully typed performance is very good. Most likely the typed performance
is due to the fact that @tt{lnm} relies heavily on Racket's plotting library,
which is a typed library. This implies that the original untyped program
likely suffers from a performance @emph{penalty} due to contracts.

@; If all graphs were similar to this at @exact{$L$}=1, performance would not be a significant issue.
@; Even at @exact{$L$}=2, we could shift focus to identifying the good
@; configurations rather than finding a new implementation strategy. 

@; WHY
@; - little goes across the boundary. client/server communicate over ports
@; PATH (easy)
@; - very small program


@parag{Morse code} The @tt{morse-code} benchmark also shows excellent
performance.  At @math{L}=0 three configurations perform at least as well as
the untyped program, and the worst-case overhead is below 3x.  Increasing
@math{L} raises the y-intercept of the lines, which matches the observation
given that the fully-typed @tt{morse-code} runs faster than the original
program.  Adding more type annotations helps improve this benchmark's
performance.

@; WHY
@; - Very little inter-module communication
@; PATH (easy)
@; - small APIs (levenshtein was 300 lines for 1 export)
@; - only 2 modules really did things, the others were data(+parser) & main


@;FIXED VERSION:

@parag{MBTA} 
The @tt{mbta} benchmark is nearly a steep vertical line, but for one flat
area.  This implies that a boundary (or group of boundaries) accounts for a
3x slowdown, such that the set of configurations where these boundaries connect
typed and untyped modules all experience similar overhead.

@; WHY
@; - run-t and t-graph are tightly coupled
@; PATH (easy)
@; - small API, even with objects


@parag{ZO Traversal} The lines for @tt{zo-traversal} are fairly steep, but
not as drastic as the lines for @tt{morse-code} or even @tt{mbta}.  More
interestingly, half the configurations suffer a 2x overhead even as @exact{$L$}
increases.  This behavior is explained by the summary numbers: because the
fully-typed configuration incurs a 4x overhead, the ability to convert
additional modules rarely helps reach a more performant configuration.

@; WHY
@; - the data is untyped, and this script is just an interface to that data
@; - funny consequence: adding types just makes things worse
@; PATH (easy)
@; - HUGE bottleneck typing the zo structs
@;   lots to do (62 structs, two zo-traversal functions for each)
@; - afterwards, straightforward (at least for the author)


@parag{Suffixtree} At @exact{$L$}=0, @tt{suffixtree} shows the worst
performance characteristics of all our benchmarks.  Over half the
gradually-typed configurations hit a performance wall and are not usable at any
realistic overhead.  Increasing @exact{$L$}, however, drastically improves
this picture.  Thus although most configurations suffer large performance
overhead, they are in theory close to a configuration with much better
performance.

@; WHY
@; - explained in the in-depth, below
@; PATH (hard)
@; - lots of continuations and letrec, (one cont. instatiated with Values was rejected by TR)
@; - module structure not bad


@; @parag{L-NM}
@; Our script for processing and plotting experimental data is a Typed Racket success story.
@; The fully-typed version performs much better than the untyped one, and gradual typing at worst introduces modest overhead.

@parag{K-CFA} The @tt{kcfa} benchmark has a very jagged shape, implying
that @exact{$N/M$}-usability is not a helpful tradeoff for this program.
At @exact{$L$}=0, selecting an @exact{$N$} strongly influences the
proportion of acceptable configurations for small values of @exact{$M$}.  This
is expecially true for @exact{$N$} between 1x and 6x overhead, and remains
true even after increasing @exact{$L$} to 1; however at @exact{$L$}=2 the
performance problem is apparently solved (assuming a method of finding the
performant configurations).


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


@parag{Synth} The @tt{synth} benchmark performs well when fully-typed, but
is significantly worse when gradually typed.  Over half the configurations
suffer an overhead of more than 20x.  Increasing @math{L} does increase the
slopes of the lines, meaning a larger number of configurations become usable
for a fixed @math{N}/@math{M} pair, but gradual typing still introduces a
large overhead.  Even at @math{L}=2 only 30% of all configurations lie in reach
of a point with at most 3x slowdown.

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


@parag{Tetris} Like @tt{suffixtree}, the @tt{tetris} benchmark is a success
story for increasing @exact{$L$}.  When @exact{$L$}=0 we see that half of
all modules are within 6x overhead, but the rest are more than 20x worse
than the untyped program.  The ``good half'', however, is apparently spread
throughout the lattice and reachable in few steps from many other
configurations.

@; WHY
@; - where is the heavy boundary?
@; - why is this different from snake?
@; PATH (easy)
@; - like snake, simple types + small + full contracts


@parag{Snake} The @tt{snake} benchmark has similar performance
characteristics to @tt{synth}.  Most gradually-typed configurations suffer more
than 20x overhead and increasing @exact{$L$} helps somewhat, but still one
must accept approximately 6x overhead before the majority of configurations
qualify as usable.

@; WHY
@; - (probably) tightly-coupled module structure?
@; - anyway, it's interesting that synth was not an isolated problem
@; - also interesting that it's not exactly tetris
@; PATH (easy)
@; - simple types, small project, fully contracted (it was already a contract benchmark)


@parag{Gregor} Despite being a large benchmark, @tt{gregor} performs
reasonably well even when gradually typed.  The worst-case slowdown of 6x
is quite good compared to the other large benchmarks, and the steep
vertical slope is close to ideal.

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
