#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:death"]{Quo Vadis Sound Gradual Typing?}

Unsound type systems are useful. They find bugs at compile-time, and an IDE can use them to
 assist programmers. Sound type systems are useful and meaningful. A
 soundly typed program cannot go wrong, up to a well-defined set of run-time
 exceptions@~cite[type-soundness]. When a typed program raises an
 exception, the exception message can (usually) pinpoint the location of
 the problem in the program source. Hence sound types are one of the best
 forms of documentation and specification around. In the context of a sound
 type system, the type of a well-named function or method often explains
 (almost) as much as an inspection of the code.

From this description it is clear why programmers eventually wish to
 annotate programs in untyped languages with types and, ideally, with sound
 types. Types increase a programmer's productivity, and sound types help
 with testing, debugging, and other maintenance tasks.  Hence sound gradual
 typing seems to be such a panacea. The problem is, however, that the cost
 of enforcing soundness appears to be overwhelming according to our
 measurements.

In general, the graphs in @figure-ref["fig:lnm1" "fig:lnm2"] clarify how
 few partially typed configurations are usable by developers or deliverable
 to customers. For almost all benchmarks, the lines are below the (red)
 horizontal line of acceptability. Even with extremely liberal settings for
 @math{N} and @math{M}, few configurations are @math{N}-deliverable or
 @math{N/M}-usable. Worse, investing more effort into type annotation does
 not seem to pay off. In practice, converting a module takes a good portion
 of a workday, meaning that setting @math{L} to @math{2} is again a liberal
 choice. But even allowing the conversion of two additional modules @emph{and}
 the unrealistic assumption that the developer picks two modules best-suited
 to improve performance does
 not increase the number of acceptable configurations by much. Put
 differently, the number of @math{L}-step @math{N/M}-acceptable
 configurations remains small with liberal choices for all three parameters.

@; -----------------------------------------------------------------------------
@section{Threats to Validity of Conclusion}

Our use of the evaluation framework projects an exceedingly negative
 image. While we are confident that the framework is a strong match for the
 goals of gradual typing, the application of the framework and its results
 must be put in perspective.

First, our benchmarks are relatively small. The two largest ones consist of
 13 and 16 modules, respectively. Even these benchmarks pose
 challenges to our computing infrastructure because they require timing
 @math{2^13} and @math{2^16} configurations @math{30} times each.
 Running experiments with programs that consist of many
 more modules would be impractical.

To make the experiment feasible for our chosen benchmark, the larger
 benchmarks are run using multiple cores and divide up the configurations
 amongst the cores. Each configuration is put into a single process running
 a separate instance of the Racket VM pinned to a single core.  This
 parallelism may introduce confounding variables due to, for example,
 shared caches or main memory. We have attempted to control for this case
 and, as far as we can tell, executing on an unloaded machine does not make
 a significant difference to our results.

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

Fourth, we articulate our results on the basis of current implementation
 technology. Typed Racket compiles to Racket, which uses rather conventional
 compilation technology. It makes no attempt to reduce the overhead of
 contracts or to exploit contracts for optimizations. It remains to be seen
 whether contract-aware compilers
 can reduce the significant overhead that our evaluation
 shows. Nevertheless, we are convinced that even if the magnitude of the
 slowdowns are reduced, some pathologies will remain.

@; -----------------------------------------------------------------------------
@section[#:tag "sec:postmortem"]{What are the Bottlenecks?}

@; Note: these results are for v6.2. On HEAD things are 30% better; see `postmortem/profile/contract-speedup.org`
@figure*["fig:postmortem" "Profiling the worst-case contract overhead"
@exact|{
\begin{tabular}{l r || r r r r r r}
Project         & \%C (S.E.) & adaptor & higher-order & library & \tt{(T->any)} & \tt{(any->T)} & \tt{(any->bool)} \\\hline
\tt{sieve}      & 92 (2.33)  &       0 &           46 &       0 &             0 &            54 &               31 \\
\tt{morse-code} & 29 (6.80)  &       0 &            0 &       0 &             0 &           100 &                0 \\
\tt{mbta}       & 39 (3.65)  &       0 &            0 &      65 &             0 &            65 &                0 \\
\tt{zo}         & 95 (0.10)  &       0 &           55 &      45 &             0 &            99 &               43 \\
\tt{suffixtree} & 94 (0.18)  &      98 &           <1 &       0 &             2 &            94 &               18 \\
\tt{lnm}        & 81 (0.73)  &       0 &            9 &      99 &            91 &             0 &                0 \\
\tt{kcfa}       & 91 (0.26)  &     100 &            0 &       0 &             0 &            54 &               31 \\
\tt{snake}      & 98 (0.21)  &      93 &            0 &       0 &             1 &            99 &               49 \\
\tt{tetris}     & 96 (0.35)  &      89 &            0 &       0 &            11 &            89 &               44 \\
\tt{synth}      & 83 (1.22)  &      51 &           90 &       0 &            29 &            20 &                0 \\
\tt{gregor}     & 83 (4.01)  &      78 &            0 &       3 &             7 &            85 &               31 \\
\tt{quad}       & 80 (0.96)  &      <1 &            1 &       0 &             3 &            <1 &               <1 \\
\bottomrule
\end{tabular}
}|
]

To analyze the cost of dynamic contract checks, we used the
 feature-specific profiler@~cite[saf-cc-2015] on each benchmark's
 @emph{slowest} configuration in the lattice. @Figure-ref{fig:postmortem}
 summarizes our findings.  

The leftmost data column (``%C'') gives the percent of each benchmark's
 total running time that was spent checking contracts.  These numbers are
 the average of ten trials; the numbers in parentheses represent the
 standard error.  Except for the short-running benchmarks,@note{The
 @tt{gregor}, @tt{morse-code}, and @tt{mbta} benchmarks finished in under 2
 seconds.  All other benchmarks ran for at least 12 seconds on their
 worst-case configuration.} we see little variability across trials.  As
 expected, the programs spend a huge amount of time checking contracts.

The remaining columns of @figure-ref{fig:postmortem} report what percentage
 of each benchmark's @emph{contract-checking} execution time is spent on a
 particular variety of contract.  Adaptor contracts separate a typed module
 from an untyped module with data structures.  Higher-order contracts are
 function contracts with at least one function in their domain or
 co-domain.  Library contracts separate an untyped library from typed
 modules, or in the case of @tt{lnm}, a typed library from untyped modules.
 The last three columns classify patterns of function contracts.  First,
 @tt{(T->any)} denotes function contracts with protected domains and
 unchecked co-domains.@note{In Racket, the @tt{any/c} contract is a no-op
 contract.}  Contracts of this shape guard typed functions called in
 untyped modules.  The shape @tt{(any->T)} is the opposite; these contracts
 guard functions with unchecked domains and protected co-domains.  For
 example, if a typed module calls an untyped function with immutable
 arguments, Typed Racket statically proves that the untyped function is
 given well-typed arguments but must insert a contract to verify the
 function's result.  Lastly, the @tt{(any->bool)} column is a subset of the
 @tt{(any->T)} column.  It counts the total time spent checking the
 function contract @tt{(-> any/c boolean?)}, which asserts that its value
 is a function taking one argument and returning a boolean.

@;bg: Should we explain that (any->T) is variable arity, but (any->bool) is strictly one argument?
@;    I think the message is clear without it

Note that many of these columns can overlap.
The @tt{mbta} benchmark in particular spends 65% of its contract-checking time on first-order library functions.
These checks are always triggered by a typed module on immutable arguments, so Typed Racket optimizes them to @tt{(any->T)} contracts.
@; Non-overlapping pairs: (adaptor, library), (higher-order, any->T), (higher-order, any->bool), *(T->any, any->T)

The most striking result in this table is the @tt{(any->bool)} column.
It says that on average, 20% of the time our benchmarks spend checking contracts
goes towards checking that predicate functions have the trivial @tt{(Any -> Boolean)} type.
Moreover, nearly all of these predicates are generated by Racket structure definitions,
so their type correctness might be assumed.
Removing these contracts or optimizing the cost of indirection seems like a clear place for Typed Racket to improve.

The adaptor and library columns, however, suggest that the apparently high cost
of predicate contracts may just be a symptom of placing a typed/untyped boundary
between a data definition and functions closely associated with the data.
One example of this is the @tt{zo} analyzer; indeed, the purpose of that code
is to provide an interface to native compiler data structures.
Nearly all benchmarks using adaptor modules exhibit this same pattern in these
worst-case results, where both the adaptor and @tt{(any->bool)} contracts
account for a significant proportion of all contracts.
The apparent exceptions are @tt{synth} and @tt{quad}.
Only @tt{synth} truly is an exception, and this is because it spends much more time
creating structured data from raw vectors than accessing the data.
The @tt{quad} benchmark in fact spends 93% of its contract-checking time validating
data structures, however this data is stored in fixed-length lists rather than
in structure types.
These lists do not require an adaptor, but produce contracts that are more
expensive than a type predicate and cannot be optimized away.

Regarding higher-order contracts, we see relatively few in our benchmark programs.
Only @tt{synth}, @tt{sieve}, and @tt{zordoz} make heavy use of higher-order functions across
contract boundaries.
@; Even then, @tt{zordoz} only returns thunks. It's barely using higher-order-ness
In these programs the cost of such contracts is apparent, but
we were more surprised to learn that so many of our benchmarks had abysmal performance
just from flat and first-order function contracts.

Finally, the @tt{(T->any)} and @tt{(any->T)} columns give a rough impression of whether
untyped or typed modules trigger most contract checks.
We confirmed these findings by inspecting the individual programs.
For all but 3 benchmarks, we indeed found that the high-cost (or, high-frequency)
contracts were triggered from a typed module calling an untyped library or data definition.
This includes @tt{kcfa}, although half its calls from typed to untyped code used
mutable arguments and hence could not be reduced to @tt{any/c}.
The exceptions were @tt{lnm}, @tt{synth}, and @tt{quad}, which all suffered more
when untyped modules imported definitions from typed ones.

