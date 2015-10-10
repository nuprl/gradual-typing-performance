#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:death"]{Quo Vadis Sound Gradual Typing?}

Unsound type systems are useful. They document the code, find bugs at
 compile-time, and enable the IDE to assist programmers. Sound type systems
 are useful @emph{and} meaningful. A soundly typed program cannot go wrong,
 up to a well-defined set of run-time exceptions@~cite[type-soundness].
 When a typed program raises an exception, the accompanying message usually
 pinpoints the location of the problem in the program source. 

From this description it is clear why programmers eventually wish to
 annotate programs in untyped languages with types and, ideally, with sound
 types. Types directly and indirectly increase a programmer's productivity,
 and sound types help with testing, debugging, and other maintenance tasks.
 In short, sound gradual typing seems to be a panacea. 

The problem is that, according to our measurements, the cost of enforcing
 soundness is overwhelming.  @Figure-ref["fig:lnm1" "fig:lnm2"] clarify
 just how few partially typed configurations are usable by developers or
 deliverable to customers. For almost all benchmarks, the lines are below
 the (red) horizontal line of acceptability. Even with extremely liberal
 settings for @math{N} and @math{M}, few configurations are
 @math{N}-deliverable or @math{N/M}-usable. Worse, investing more effort
 into type annotation does not seem to pay off. In practice, converting a
 module takes a good portion of a workday, meaning that setting @math{L} to
 @math{2} is again a liberal choice. But even allowing the conversion of
 two additional modules @emph{and} the unrealistic assumption that the
 developer picks two modules best-suited to improve performance does not
 increase the number of acceptable configurations by much. Put differently,
 the number of @math{L}-step @math{N/M}-acceptable configurations remains
 small with liberal choices for all three parameters.

Our use of the evaluation framework projects an extremely negative image of
 @emph{sound} gradual typing. While we are confident that the framework
 captures the proclaimed spirit of the goals of gradual typing, our
 particular application of the framework and its results must be put in
 perspective. @Secref{sec:threats} explains why the evaluation of Typed
 Racket---though not the framework itself---may look overly
 negative. @Secref{sec:postmortem} presents a detailed analysis of the
 worst elements in the twelve lattices and highlights those kinds of
 contracts that impose the most significant cost.

@; -----------------------------------------------------------------------------
@section[#:tag "sec:threats"]{Threats to Validity of Conclusion}

We have identified four threats to the validity of our measurements and our
 conclusions. 

First, our benchmarks are relatively small due to constraints on our
 computing infrastructure. The two largest ones consist of 13 and 16
 modules, respectively, and pose serious challenges because they require
 timing @math{2^13} and @math{2^16} configurations @math{30} times each.
 In order to obtain results for these large benchmarks in a reasonable
 amount of time, they are run using multiple cores and the configurations
 are divided amongst the cores. Each configuration is put into a single
 process running a separate instance of the Racket VM pinned to a single
 core.  This parallelism may introduce confounding variables due to, for
 example, shared caches or main memory. We have attempted to control for
 this case and, as far as we can tell, executing on an unloaded machine
 does not make a significant difference to our results.

Second, several of our benchmarks import some modules from Racket's suite of
 libraries that remain untyped throughout the process, including for the
 fully typed configuration. While some of these run-time libraries come in
 the trusted code base---meaning Typed Racket knows their types and the
 types are not compiled to contracts---others are third-party libraries
 that impose a cost on all configurations. In principle, these interfaces
 might substantially contribute to the running-time overhead of
 partially typed configurations. Regardless, given the low typed/untyped ratios, 
 these libraries are unlikely to affect our conclusions.

Third, our method imagines a particularly @emph{free} approach to
 annotating programs with types. By ``free'' we mean that we do not expect
 software engineers to add types to modules in any particular
 order. Although this freedom is representative of some kind of maintenance
 work---add types when bugs are repaired and only then---a team may decide
 to add types to an entire project in a focused approach. In this case, they
 may come up with a particular kind of plan that avoids all of these
 performance traps. Roughly speaking, such a plan would correspond to a
 specific path from the bottom element of the performance lattice to the top
 element.  Sadly our current measurements suggest that almost all
 bottom-to-top paths in our performance lattices go through
 performance bottlenecks.  As the  @tt{suffixtree} example
 demonstrates, a path-based approach depends very much on the structure of
 the module graph.  We therefore conjecture that some of the ideas offered
 in the conclusion section may help such planned, path-based approaches.

Fourth, we articulate our conclusions on the basis of current
 implementation technology. Typed Racket compiles to Racket, which uses
 rather conventional JIT compilation technology. It makes no attempt to
 reduce the overhead of contracts or to exploit contracts for
 optimizations. It remains to be seen whether contract-aware compilers can
 reduce the significant overhead that our evaluation shows. Nevertheless,
 we are convinced that even if the magnitude of the slowdowns are reduced,
 some pathologies will remain.

@; -----------------------------------------------------------------------------
@section[#:tag "sec:postmortem"]{What are the Bottlenecks?}

@; Note: these results are for v6.2. On HEAD things are 30% better; see `postmortem/profile/contract-speedup.org`

@(require racket/format)

@(define (T->any) @racket[(-> T any/c)])
@(define (any->T) @racket[(-> any/c T)])
@(define (any->bool) @racket[(-> any/c boolean?)])

@(define-syntax-rule 
   (row x y z w ...)
   @list[ @hspace[4]
	  (tt (~a 'x)) (math (~a y)) (math (format "(~a)" (number->string z))) (math (~a 'w)) ... 
	  @hspace[4]])

@figure*["fig:postmortem" "Profiling the worst-case contract overhead"]{
@tabular[
 #:sep @hspace[2]
 #:row-properties '(bottom-border ())
 #:column-properties '(left left right)

@list[
 @list[@hspace[4]
	"Project"    "%C" "(S.E.)" "adaptor" "higher-order" "library" @T->any[]  @any->T[] @any->bool[] 
       @hspace[4]]
 @row[ sieve          92    2.33         0           46       0         0        54           31]
 @row[ morse-code     29    6.80         0            0       0         0       100            0]
 @row[ mbta           39    3.65         0            0      65         0        65            0]
 @row[ zo             95    0.10         0           55      45         0        99           43]
 @row[ suffixtree     94    0.18        98           <1       0         2        94           18]
 @row[ lnm            81    0.73         0            9      99        91         0            0]
 @row[ kcfa           91    0.26       100            0       0         0        54           31]
 @row[ snake          98    0.21        93            0       0         1        99           49]
 @row[ tetris         96    0.35        89            0       0        11        89           44]
 @row[ synth          83    1.22        51           90       0        29        20            0]
 @row[ gregor         83    4.01        78            0       3         7        85           31]
 @row[ quad           80    0.96        <1            1       0         3        <1           <1]
]
]

}

To analyze the cost of dynamic contract checks, we used the
 feature-specific profiler@~cite[saf-cc-2015] on each benchmark's
 @emph{slowest} configuration in the lattice. @Figure-ref{fig:postmortem}
 summarizes our findings.  

The leftmost data column (%C) gives the percent of each benchmark's total
 running time that was spent checking contracts.  These numbers are the
 average of ten trials; the numbers in parentheses represent the standard
 error.  Except for the short-running benchmarks (@tt{gregor},
 @tt{morse-code}, and @tt{mbta}), we see little variability across trials.
 As expected, the programs spend a huge proportion of their running time checking contracts.

The remaining columns of @figure-ref{fig:postmortem} report what percentage
 of each benchmark's @emph{contract-checking} execution time is spent on a
 particular variety of contract:
@itemlist[

@item{Adaptor contracts separate a typed module from an untyped module
 with data structures.} 

@item{Higher-order contracts are function contracts with at least one
 function in their domain or co-domain.}

@item{Library contracts separate an untyped library from typed modules
 or vice versa (in the case of @tt{lnm}).}

@item{The shape @T->any[] refers to all those contracts with a protected
 argument and an unchecked co-domain.@note{In Racket, the @tt{any/c}
 contract is a no-op contract.}  Contracts of this shape typically guard
 typed functions called in untyped modules.}

@item{Conversely, @any->T[] guards functions with (any number of)
 unchecked arguments and protected co-domains.  For example, if a typed
 module calls an untyped function with immutable arguments, Typed Racket
 statically proves that the untyped function is given well-typed
 arguments but must insert a contract to verify the function's result.}

@item{The @any->bool[] column measures the time spent checking functions
 that take a single argument and returning a Boolean value. It is thus a
 subset of the @any->T[] column.}  
]
@;
Other columns overlap, too.  For example, the @tt{mbta} benchmark in
particular spends 65% of its contract-checking time on first-order
library functions. These checks are always triggered by a typed module on
immutable arguments, so Typed Racket optimizes them to @any->T[] contracts.

@; Non-overlapping pairs: (adaptor, library), (higher-order, any->T), (higher-order, any->bool), *(T->any, any->T)

Most strikingly, the @any->bool[] column suggests that on average
twenty percent (20%) of the time our benchmarks spend checking
contracts goes towards checking that predicate functions satisfy
the trivial @any->bool[] contract.  Moreover, nearly all of these
predicates are generated by Racket structure definitions, so their
type correctness might be assumed.  Removing these contracts or
optimizing the cost of indirection seems like a clear place for
Typed Racket to improve.

In contrast, the adaptor and library columns suggest that the
apparently high cost of predicate contracts may just be a symptom
of placing a typed/untyped boundary between a structure type
definition and functions closely associated with the data.  One
example of this is the @tt{zo} analyzer; indeed, the purpose of
that code is to provide an interface to native compiler data
structures.  In nearly all worst-case measurememts for benchmarks
using adaptor modules the adaptor and @any->bool[] contracts seem
to account for a huge proportion of all contracts.  The apparent
exceptions are @tt{synth} and @tt{quad}.  Only @tt{synth} is
a true exception, though; it spends much more time creating
structured data from raw vectors than accessing the data.  The
@tt{quad} benchmark in fact spends 93% of its contract-checking
time validating data structures, which are stored in fixed-length
lists rather than in structure types.  These lists do not require
an adaptor, but their types translate to contracts that are far
more expensive than plain structure type predicates.

Higher-order contracts show up in only a few of the benchmark
programs. Specifically, only @tt{synth}, @tt{sieve}, and
@tt{zordoz} make heavy use of higher-order functions across
contract boundaries.  Unlike the cost of first-order contracts,
the costs of these higher-order contracts is quite apparent in these programs.

Finally, the @T->any[] and @any->T[] columns give a rough
impression of whether untyped or typed modules trigger most
contract checks.  We confirmed these findings by inspecting the
individual programs.  For all but three benchmarks, the high-cost
contracts are triggered by calls from a typed module into an
untyped library or data definition.  This includes @tt{kcfa},
although half its calls from typed to untyped code used mutable
arguments and hence could not be reduced to @tt{any/c}.  The
exceptions are @tt{lnm}, @tt{synth}, and @tt{quad}, which all
suffer more when untyped modules imported definitions from typed
ones.

