#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:death"]{Sound Gradual Typing is Dead}

Unsound type systems are useful. They find bugs at compile-time, and an IDE can use them to
 assist programmers. Sound type systems are meaningful in addition. A
 soundly typed program cannot go wrong, up to a well-defined set of run-time
 exceptions@~cite[type-soundness]. When a typed program raises an
 exception, the exception message can (usually) pinpoint the location of
 the problem in the program source. Hence sound types are one of the best
 forms of documentation and specification around. In the context of a sound
 type system, the type of a well-named function or method often explains
 (almost) as much as an inspection of the code.

From this description, it is clear why programmers eventually wish to
 annotate programs in untyped languages with types and, ideally, with sound
 types. Types increase a programmer's productivity, and sound types greatly
 help with testing, debugging, and other bug-related maintenance tasks.
 Hence sound gradual typing seems to be such a panacea. The problem is,
 however, that the cost of enforcing soundness appears to be overwhelming
 according to our measurements. 

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

Like Marc Antony, we come here to bury sound gradual typing, not to praise
 it. Sound gradual typing is dead.

@; -----------------------------------------------------------------------------
@section{Threats to Validity of Conclusion}

Our judgment is harsh and fails to acknowledge potential weaknesses in our
 evaluation method and in our results.

First, our benchmarks are relatively small. The two largest ones consist of
 13 and 16 modules, respectively. Even these benchmarks pose
 challenges to our computing infrastructure because they require timing
 @math{2^13} and @math{2^16} configurations @math{30} times each.
 Running experiments with modules that consist of many
 more modules would be impractical.
 Our results might be less valid
 in the context of large programs, though practical experience using
 Typed Racket suggests otherwise. @; FIXME: not totally sure I read this edit right

To make the experiment feasible even at the sizes we use in this paper, we run the larger
 benchmarks using multiple cores and divide up the configurations amongst
 the cores. Each configuration is run in a single process running a separate
 instance of the Racket VM pinned to a single core. However, this
 parallelism may introduce confounding variables due to, for example, shared
 caches or main memory. We have attempted to control for this case and, as
 far as we can tell, executing on an unloaded machine does not make a
 significant difference to our results.

Second, several of our benchmarks import some modules from Racket's suite of
 libraries that remain untyped throughout the process, including for the
 fully typed configuration. While some of these run-time libraries come in
 the trusted code base---meaning Typed Racket knows their types and the
 types are not compiled to contracts---others are third-party libraries
 that impose a cost on all configurations. In principle, these interfaces
 might substantially contribute to the running-time overhead of
 partially typed configurations. But, given the low typed/untyped ratios, 
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

Fourth, we state our judgment with respect to the current implementation
 technology. Typed Racket compiles to Racket, which uses rather conventional
 compilation technology. It makes no attempt to reduce the overhead of
 contracts or to exploit contracts for optimizations. It remains to be seen
 whether contract-aware compilers
 can reduce the significant overhead that our evaluation
 shows. Nevertheless, we are convinced that even if the magnitude of the
 slowdowns are reduced, some pathologies will remain.

@; -----------------------------------------------------------------------------
@section{Postmortem}

The future of sound gradual typing hinges on whether the run-time cost of
type soundness can be reduced.
As a preliminary step towards this goal, we have used St. Amour @etal's
feature-specific-profiler @~cite[saf-cc-2015] to measure the contract
overhead of each benchmark's slowest variation.@note{Sampling frequency: 0.005s.}
@Figure-ref{fig:postmortem} summarizes our findings.


@figure*["fig:postmortem" "Profiling the worst-case contract overhead"
@exact|{
\begin{tabular}{l r || r r r r r r}
Project         & C/R (S.E.) & adaptor & higher-order & library & \tt{(T->any)} & \tt{(any->T)} & \tt{(any->bool)} \\\hline
\tt{sieve}      & 92 (2.33)  &       0 &           46 &       0 &             0 &            54 &               31 \\
\tt{morse-code} & 29 (6.80)  &       0 &            0 &       0 &             0 &           100 &                0 \\
\tt{mbta}       & 39 (3.65)  &       0 &            0 &      65 &             0 &            65 &                0 \\
\tt{zo}         & 95 (0.10)  &       0 &            0 &      45 &             0 &            99 &               43 \\
\tt{suffixtree} & 94 (0.18)  &      98 &           <1 &       0 &             2 &            94 &               18 \\
\tt{lnm}        & 81 (0.73)  &       0 &            9 &      99 &            91 &             0 &                0 \\
\tt{kcfa}       & 91 (0.26)  &     100 &            0 &       0 &             0 &            54 &               31 \\
\tt{snake}      & 98 (0.21)  &      93 &            0 &       0 &             1 &            78 &               49 \\
\tt{tetris}     & 96 (0.35)  &      89 &            0 &       0 &            11 &            89 &               44 \\
\tt{synth}      & 83 (1.22)  &       0 &           90 &       0 &            29 &            20 &                0 \\
\tt{gregor}     & 83 (4.01)  &      78 &            0 &       3 &             7 &            85 &               31 \\
\tt{quad}       & 80 (0.96)  &      <1 &           <1 &       0 &             3 &            <1 &               <1 \\
\bottomrule
\end{tabular}
}|
]

@; TODO explain columns
@; TODO explain % (contract vs all else)
@; TODO 
@; TODO to verify, we instrumented the Racket contract system stop applying some contracts. Results as expected.
@; TODO note: on HEAD things are 30% better already, by our measures
@; synth, called within a macro!
@; TODO lessons
@; TODO we even removed data (on kcfa)


The first data column, ``Contract Overhead'', gives the percent of each project's
worst-case running time that was spent checking contracts.
For example, our sampling profiler estimates that approximately 80\% of @tt{quad}'s worst-case running time was spent checking contracts.
These proportions are the average of 10 runs of the sampling profiler, and the
Std. Dev. column confirms there was relatively little variability in the
time spent checking contracts.

The last three columns attempt to pinpoint the most expensive contracts.
Each column is a percentage of the Contract Overhead number.
The first, @tt{(Any -> Boolean)}, gives the percent of contract runtime
spent checking that untyped predicate functions truly return boolean values.
Despite being a simple contract, it was run very frequently.
Second, we give the percent of contract runtime spent verifying higher-order
contracts (i.e., function contracts with a nested function in a negative position).
These too account for a large chunk of the overhead.
Finally, the ``Library'' column tallies the overhead of boundaries to
third-party libraries.
Note that these last two columns are overlapping; in particular @tt{lnm}
uses a higher-order contract from the Racket's typed plotting library.
