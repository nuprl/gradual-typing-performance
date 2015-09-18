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
benchmarks means compiling, running, and timing hundreds of thousands of
configurations. Each configuration is run 30 times to ensure that the
@;
@;@margin-note*{cite the random number paper?}
@;
timing is not affected by random factors; some configurations take minutes
to run. Presenting and analyzing this wealth of data poses a separate
challenge all by itself.

This section presents our measurements.
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
benchmarks were run on machine A: @tt{sieve} and @tt{kcfa}.
The following were run on machine B: @tt{suffixtree}, @tt{morse-code}, and @tt{lnm}.
The following were run on machine C: @tt{snake},
@tt{synth}, @tt{tetris}, @tt{gregor}, and @tt{quad}.
@;; FIXME edit this when all are run
For each configuration we report the average of 30 runs.
@; TODO: talk about warm up?  
@; TODO:Compilation v. interpreteration v. jitting?
All of our runs use a single core for each configuration with green threads where applicable.
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

Typed modules require type annotations on their datatypes and functions.
Modules provide their exports with types, so that the
type checker can cross-check modules. A typed module may import
values from an untyped module, which forces the
corresponding @racket[require] specifications to come with
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
@racket[require/typed] specification are compiled into contracts for
the values that flow into the module. For example, if some
imported variable is declared to be a @tt{Char}, the check @racket[char?]
is performed as the value flows across the module boundary. Higher-order
types (functions, objects, or classes) become contracts that are wrapped
around the imported value and which check future interactions of this
value with its context.

The performance costs of gradual typing thus consist of allocation of
wrapper and run-time checks. Moreover, the compiler has to assume that
any value could be wrapped, so cannot generate direct field access code
as would be done in a statically typed language.

Since our evaluation setup calls for linking typed modules to both typed
and untyped server modules, depending on the configuration, we replace
@racket[require/typed] specifications with @racket[require/typed/check]
versions. This new syntax can determine whether the server module is typed
or untyped. It installs contracts if the server module
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


Sadly, the performance improvement of the typed configuration is the
 only good part of this benchmark. Almost all partially typed configurations
 exhibit slowdowns between 0.7x and 105x. Inspection of the lattice
 suggests several points about these slowdowns: @itemlist[

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
The performance lattice for @tt{suffixtree} is bad news for gradual typing.
It exhibits performance ``valleys'' in which a maintenance programmer can get stuck.
Consider starting with the untyped program, and for some reason choosing
to add types to @tt{label}. The program slows down by 88x. Without any
guidance, a developer may choose to then type @tt{structs} and see the
program slow down to 104x.  After that, typing @tt{main} (104x), @tt{ukkonen}
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
typed/untyped ratio, the maximum overhead, the average overhead, and the number
of @deliverable{300} and @usable["300" "1000"] configurations. This is
followed by three graphs that show how many configurations impose an overhead
between 1x and 20x for three values of @math{L}: @math{0}, @math{1}, and
@math{2}.

The typed/untyped ratio is the slowdown or speedup of fully typed code over
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

The @deliverable{300} and @usable["300" "1000"] counts are computed for
@math{L}=0. Next to each count, we report (in parentheses) the count divided
by the number of configurations.

The three cumulative performance graphs are read as follows. The x-axis
represents the slowdown over the untyped program (from 1x to
@id[PARAM-MAX-OVERHEAD]x).  The y-axis is a count of the number of
configurations (from @math{0} to @math{2^n}) scaled so all graphs are the
same height. The blue curves show how many configurations have less than a
given slowdown.  They are, by definition, monotonically increasing because
the more overhead is allowed, the more configurations satisfy the
condition. The ``ideal'' result would be a flat line at a graph's top. Such
a result would mean that all configuration are as fast (or faster) as the untyped
one.  The ``worst'' scenario is a flat line at the graph's bottom,
indicating that all configuration are more than 20x slower than the untyped
one. For ease of comparison between graphs, a dashed
(@exact{\color{red}{red}}) horizontal line indicates 60% point of all
configurations.

@;Each line was obtained by measuring @id[PARAM-NUM-SAMPLES] overheads 
@;linearly spaced along the x-axis.

Our method defines @deliverable{N} and @usable["N" "M"] as key
functions for measuring the quality of a gradual type system.  The
cumulative performance graphs display the values of parameters @math{N} and
@math{M} as, respectively, a @exact{\color{ForestGreen!90!black}{green}} and
a @exact{\color{Goldenrod!65!black}{yellow}} vertical line. For this
experiment we have chosen values of 300% and 1000%. These values are rather liberal
and we expect that most production contexts would not tolerate anything higher than 200% (and
some would object to any slowdown). Thus, for any program, the value of
@deliverable{300} is the value of the y-axis where the blue line intersects
the green one.  This is how many configurations are deliverable.  Similarly
for @usable["300" "1000"], its value is the difference of the intercepts.
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

As mentioned, the ideal shape for these curves is a flat line at the top of the y-axis.
Of course the dynamic checks inserted by gradual type systems make this
ideal difficult to achieve even with type-driven optimizations, so the
next-best shape is a nearly-vertical line reaching the 100% count at a low x-value.
Generally, a steep slope implies a good tradeoff between accepting a larger
performance overhead and the number of configurations that run within the
accepted overhead.
A large y-intercept means that many configurations run at least as quickly
as the untyped program.

Given the wide x-axis range of overhead factors, we would expect that only
the leftmost quarter of each graph shows any interesting vertical slope.
Put another way, if it were true that Typed Racket's sound gradual typing
is practical but requires tuning and optimization, the data
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


@parag{Sieve}
The flat line at @math{L}=0 shows that half of all configurations suffer
unacceptable overhead. As there are only 4 configurations in the lattice
for @tt{sieve}, increasing @math{L} improves performance.

@parag{Morse code}
The steep lines show that a few configurations suffer modest overhead (below 2x),
otherwise @tt{morse-code} performs well.
Increasing @math{L} improves the worst cases.

@parag{MBTA}
These lines are also steep, but flatten briefly at @math{N}=2.
This coincides with the @deliverable{200} performance of the fully-typed
configuration.
Increasing @tt{L} draws the flat line further left.
As one would expect, freedom to type additional modules brings more configurations
into a @deliverable{200} equivalence class.

@parag{ZO Traversal}
Plots here are similar to @tt{mbta}.
The fully-typed configuration is @deliverable{322} and this limits the possible
benefits of increasing @math{L}.

@parag{Suffixtree}
The wide horizontal areas are explained by the performance lattice in
@figure-ref{fig:suffixtree}: configurations' running times are not evenly
distributed but rather increase drastically when certain boundaries exist.
Increasing @math{L} significantly improves the number of acceptable configuration
at 10x and even 3x overhead.

@parag{LNM}
These results are ideal.
Note the large y-intercept at @math{L}=0.
This shows that very few configurations suffer any overhead.

@parag{K-CFA}
The most distinctive feature at @math{L}=0 is the flat portion between @math{N}=0
and @math{N}=6. This characteristic remains at @math{L}=1, and overall performance
is very good at @tt{L}=2.

@parag{Snake}
The slope at @math{L}=0 is very low.
Allowing @math{L}=1 brings a noticeable improvement when @math{N} is at least 5,
but the difference between @math{L}=1 and @math{L}=2 is small.

@parag{Tetris}
Each @tt{tetris} plot is essentially a flat line.
At @math{L}=0 roughly 1/3 of configurations lie below the line.
This improves to 2/3 at @math{L}=1 and only a few configurations suffer overhead
if we let @math{L}=2.

@parag{Synth}
Each slope is very low.
Furthermore, some configurations remain unusable even at @math{L}=2.
These plots have few flat areas, which implies that overheads are spread
evenly throughout possible boundaries in the program.

@parag{Gregor}
These steep lines are impressive given that @tt{gregor} has 13 modules.
Increasing @math{L} brings consistent improvements.

@parag{Quad}
The @tt{quad} plots follow the same pattern as @tt{mbta} and the @tt{zo} analyzer, despite being visually distinct.
In all three cases, there is a flat slope for overheads below the typed/untyped ratio and a steep increase just after.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:postmortem"]{Postmortem}

@; Note: these results are for v6.2. On HEAD things are 30% better; see `postmortem/profile/contract-speedup.org`
@figure*["fig:postmortem" "Profiling the worst-case contract overhead"
@exact|{
\begin{tabular}{l r || r r r r r r}
Project         & \%C (S.E.) & adaptor & higher-order & library & \tt{(T->any)} & \tt{(any->T)} & \tt{(any->bool)} \\\hline
\tt{sieve}      & 92 (2.33)  &       0 &           46 &       0 &             0 &            54 &               31 \\
\tt{morse-code} & 29 (6.80)  &       0 &            0 &       0 &             0 &           100 &                0 \\
\tt{mbta}       & 39 (3.65)  &       0 &            0 &      65 &             0 &            65 &                0 \\
\tt{zo}         & 95 (0.10)  &       0 &            0 &      45 &             0 &            99 &               43 \\
\tt{suffixtree} & 94 (0.18)  &      98 &           <1 &       0 &             2 &            94 &               18 \\
\tt{lnm}        & 81 (0.73)  &       0 &            9 &      99 &            91 &             0 &                0 \\
\tt{kcfa}       & 91 (0.26)  &     100 &            0 &       0 &             0 &            54 &               31 \\
\tt{snake}      & 98 (0.21)  &      93 &            0 &       0 &             1 &            99 &               49 \\
\tt{tetris}     & 96 (0.35)  &      89 &            0 &       0 &            11 &            89 &               44 \\
\tt{synth}      & 83 (1.22)  &      51 &           90 &       0 &            29 &            20 &                0 \\
\tt{gregor}     & 83 (4.01)  &      78 &            0 &       3 &             7 &            85 &               31 \\
\tt{quad}       & 80 (0.96)  &      <1 &           <1 &       0 &             3 &            <1 &               <1 \\
\bottomrule
\end{tabular}
}|
]

To help understand the source of costs, we used the feature-specific profiler
of St. Amour @etal @~cite[saf-cc-2015] to measure the contract
overhead of each benchmark's @emph{slowest} configuration.
@Figure-ref{fig:postmortem} summarizes our findings.
The leftmost data column, labeled ``%C'', gives the percent of each benchmark's total running
time that was spent checking contracts on its slowest configuration.
These numbers are the average of 10 trials, so in parentheses we give the standard error of our measurements.
Except for the short-running benchmarks@note{@tt{gregor}, @tt{morse-code}, and @tt{mbta}
finished in under 2 seconds.
All other benchmarks ran for at least 12 seconds on their worst-case configuration.},
we saw little variability across trials.

As expected, the programs spend an incredible amount of time checking contracts.
The more interesting question is: what patterns, if any, do these contracts share?

To answer this question, the remaining columns of @figure-ref{fig:postmortem} tell what
percentage of each benchmark's @emph{contract-checking} execution time was spent on a
particular variety of contracts.
Adaptor contracts lie between a typed module and untyped data structures.
Higher-order contracts are function contracts with at least one function in their domain.
Library contracts separate an untyped library from typed modules, or in the case of @tt{lnm},
a typed library from untyped modules.
The last three columns classify patterns of function contracts.
First, @tt{(T->any)} denotes function contracts with protected domains and unchecked codomains.@note{In Racket, the @tt{any/c} contract is a no-op contract.}
Contracts of this shape guard typed functions called in untyped modules.
The shape @tt{(any->T)} is the opposite; these contracts guard functions with unchecked domains and protected codomains.
For example, if a typed module calls an untyped function with immutable arguments, Typed Racket
statically verifies that the untyped function is given well-typed arguments, but must insert a contract to verify
the function's result.
Lastly, the @tt{(any->bool)} column is a subset of the @tt{(any->T)} column.
It counts the total time spent checking the function contract @tt{(-> any/c boolean?)}, which
asserts that its value is a function taking one argument and returning a boolean.

@;bg: Should we explain that (any->T) is variable arity, but (any->bool) is strictly one argument?
@;    I think the message is clear without it

Note that many of these columns can overlap.
The @tt{mbta} benchmark in particular spends 65% of its contract-checking time verifying first-order library functions.
These checks are always triggered by a typed module on immutable arguments, so they also classify as @tt{(any->T)} contracts.
@; Non-overlapping pairs: (adaptor, library), (higher-order, any->T), (higher-order, any->bool), *(T->any, any->T)

The most striking result in this table is the @tt{(any->bool)} column.
It says that on average, 20% of the time our benchmarks spend checking contracts
goes towards verifying that predicate functions have the trivial @tt{(Any -> Boolean)} type.
Moreover, nearly all these predicates are generated by Racket structure definitions,
so their type correctness might be assumed.
Removing or optimizing these contracts seems like a clear place for Typed Racket to improve.

The adaptor and library columns, however, suggest that the apparently high cost
of predicate contracts may be a symptom of placing a typed/untyped boundary
between a data definition and functions closely associated with the data.
One example of this is the @tt{zo} analyzer; indeed, the purpose of that code
is to provide an interface to native compiler data structures.
Nearly all benchmarks using adaptor modules exhibit this same pattern,
where both the adaptor and @tt{(any->bool)} contracts account for a significant
proportion of all contracts.
The exceptions are @tt{synth} and @tt{quad}, the former because it more often
creates structured data from raw vectors than accessing data
and the latter because its core data is represented as fixed-length lists.
In fact, @tt{quad}'s list contracts do not fit well into any of our categories
because they do not require an adaptor module and cannot be optimized to @tt{any} contracts.
Nonetheless, recursive contracts for structured lists account for
93% of @tt{quad}'s contract-checking time.
@;bg; manual inspection, average of 10 runs

Regarding higher-order contracts, we see relatively few in our benchmark programs.
Only @tt{synth} and @tt{sieve} make heavy use of higher-order functions across
contract boundaries.
In these programs the cost of higher-order contracts is apparent, but
we were surprised to learn that so many of our benchmarks had abysmal performance
just from flat and first-order function contracts.

Finally, the @tt{(T->any)} and @tt{(any->T)} columns give a rough impression of whether
untyped or typed modules trigger most contract checks.
We confirmed these findings by inspecting the individual programs.
For most, we indeed found that the high-cost (or, high-frequency)
contracts were triggered from a typed module calling an untyped library or data definition.
This includes @tt{kcfa}, although half its calls from typed to untyped used
mutable arguments and hence could not be reduced to @tt{any/c}. 
The exceptions were @tt{lnm}, @tt{synth}, and @tt{quad}, which all suffered more
when untyped modules called typed ones.

