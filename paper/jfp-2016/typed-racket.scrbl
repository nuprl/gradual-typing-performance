#lang scribble/base

@require[
  "common.rkt"
  "typed-racket.rkt"
]

@title[#:tag "sec:tr"]{Evaluating Typed Racket}

To validate the efficacy of our framework, we apply it to a suite of
 @id[NUM-BENCHMARKS] Typed Racket programs.
For each program we have collected running times over a full performance lattice.
In general the lattices are too large to print or analyze, so we present
 our results using a graphical shorthand quantifying the number of @step{}
 configurations.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:bm"]{The Benchmark Programs}

The benchmarks themselves are representative of actual user code yet
 small enough that exhaustive performance evaluation remains tractable.
In the following descriptions, we comment on the purpose
 of each benchmark and use a graph structure to represent the interactions
 of its modules.
Nodes in the graphs represent modules in the program that our experiment
 varies as typed or untyped.
Edges represent static import statements.
For example, the leftmost node in each graph represents the program's main module,
 which imports from other modules but is never itself imported.
Finally, we color and thicken each edge in proportion to the run-time cost
 associated with the edge.
@todo{what are colors/what mean?}

Most benchmarks are self-contained, but where relevant we note their external
 dependencies.
As a final note, our experiment runs the benchmarks using fixed inputs,
 but the results should be the same on different inputs.
We have in fact experimented with inputs of various size and content
 for select benchmarks but found
 the relative overheads due to type boundaries remained the same.
For the purpose of the experiment, the final input size we used
 was a compromise
 between having an untyped runtime long enough to be stable against
 operating system effects but short enough that the slowest
 configurations finished reasonably quickly.


@; -----------------------------------------------------------------------------
@subsection{Benchmark Descriptions}

@benchmark-descriptions[
@(benchmark
  #:name 'sieve
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin "Synthetic"
  #:purpose "Generate prime numbers"
  @elem{
    Demonstrates a scenario where user
     code closely interacts with higher-order library code---in this case,
     a stream library.
    When fully typed or untyped, @bm{sieve} computes quickly; however,
     introducing a type boundary between the two modules adds
     significant overhead.
  }
)
@(benchmark
  #:name 'morsecode
  #:author "John Clements and Neil Van Dyke"
  #:num-adaptor 0
  #:origin @hyperlink["https://github.com/jbclements/morse-code-trainer/tree/master/morse-code-trainer"]{Library}
  #:purpose "Morse code Trainer"

  @elem{
    The morse code benchmark is derived from training program that
     plays an audio clip, accepts keyboard input, and scores the input
     based on its Levenshtein distance from the correct answer.
    For our benchmark we remove the I/O features but otherwise convert and
     score a fixed list of simulated input words.
  }
)
@(benchmark
  #:name 'mbta
  #:author "Matthias Felleisen"
  #:num-adaptor 0
  #:origin "Educational"
  #:purpose "Interactive map"
  #:external-libraries (list @hyperlink["http://github.com/stchang/graph"]{graph})

  @elem{
    Builds a graph representation of Boston's subway system and
     answers a series of reachability queries.
    The original program ran an asynchronous client/server framework
     but our benchmark is single-threaded to cooperate with Racket's sampling
     profiler.
  }
)
@(benchmark
  #:name 'zordoz
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin @hyperlink["http://github.com/bennn/zordoz"]{Library}
  #:purpose "Explore Racket bytecode"
  #:external-libraries (list @hyperlink["http://docs.racket-lang.org/raco/decompile.html#%28mod-path._compiler%2Fdecompile%29"]{compiler-lib})

  @elem{
    Provides a shell-style interface for traversing
     Racket bytecode (@tt{.zo} files).
    Our benchmark decompiles its own bytecode and
     counts the number of branch instructions in the resulting tree structure.

    The Racket bytecode format changed between versions 6.2 and 6.3 with
     the release of the set-of-scopes macro expander@~cite[f-popl-2016].
    Consequently, our benchmark is slightly different
     after version 6.2; however, the relative difference between
     gradually typed configurations is the same across bytecode formats.
  }
)
@(benchmark
  #:name 'suffixtree
  #:author "Danny Yoo"
  #:num-adaptor 1
  #:origin @hyperlink["https://github.com/dyoo/suffixtree"]{Library}
  #:purpose "Ukkonen's suffix tree algorithm"

  @elem{
    Computes longest common subsequences by converting strings to a suffix
     tree representation and comparing the trees.
    The benchmark compares lines of English text; each line is no
     more than 80 characters long.
  }
)
@(benchmark
  #:name 'lnm
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin "Synthetic"
  #:purpose "Create L-NM graphs"
  #:external-libraries (list @hyperlink["https://docs.racket-lang.org/plot/"]{plot}
                             ", "
                             @hyperlink["https://docs.racket-lang.org/math/stats.html"]{racket/statistics})

  @elem{
    While writing this paper, we built a small library of scripts to analyze
     and graph the data shown in @Secref{sec:lnm-plot}.
    The @bm{lnm} benchmark creates one such graph for a 13-module benchmark.
    Interestingly, most of the computation time is spent in calls to Racket's
     typed statistics and plotting libraries, so performance improves as more
     @bm{lnm} modules are typed.
  }
)
@(benchmark
  #:name 'kcfa
  #:author "Matt Might"
  #:num-adaptor 4
  #:origin @hyperlink["http://matt.might.net/articles/implementation-of-kcfa-and-0cfa/"]{Blog post}
  #:purpose "Demo k-CFA algorithm"

  @elem{
    Simple, inefficient implementation of k-CFA@~cite[shivers-dissertation-1991].
    Our benchmark runs 1-CFA on a lambda calculus term
     that computes @racket[2*(1 + 3) = 2*1 + 2*3].
  }
)
@(benchmark
  #:name 'zombie
  #:author "David Van Horn"
  #:num-adaptor 1
  #:origin @hyperlink["https://github.com/philnguyen/soft-contract"]{Educational}
  #:purpose "Game"

  @elem{
    A game where the player must keep his marker away from
     computer-controlled "zombie" markers.
    We benchmark the game on a pre-defined sequence of commands and remove the
     I/O features.

    As noted by @PHIL{} @|etal|, the original
     program was implemented in an object-oriented style but converted
     to a functional encoding to assess soft contract verification@~cite[nthvh-icfp-2014].
    Our benchmark is a typed version of the functional game.
  }
)
@(benchmark
  #:name 'snake
  #:author "David Van Horn"
  #:num-adaptor 1
  #:origin @hyperlink["https://github.com/philnguyen/soft-contract"]{Educational}
  #:purpose "Game"

  @elem{
    Game in which a growing and moving snake avoids walls and its own tail.
    Our benchmark is a gradually typed version of the @bm{snake} game from
     @PHIL{} @|etal| and runs a pre-recorded sequence of state-changing moves
     simulating user input@~cite[nthvh-icfp-2014].
  }
)
@(benchmark
  #:name 'tetris
  #:author "David Van Horn"
  #:num-adaptor 1
  #:origin @hyperlink["https://github.com/philnguyen/soft-contract"]{Educational}
  #:purpose "Game"

  @elem{
    Implements the eponymous game.
    The benchmark runs a deterministic sequence of moves and is
     adapted from @PHIL{} @|etal|@~cite[nthvh-icfp-2014].
  }
)
@(benchmark
  #:name 'synth
  #:author "Vincent St. Amour and Neil Toronto"
  #:num-adaptor 1
  #:origin @hyperlink["http://github.com/stamourv/synth"]{Library}
  #:purpose "Music synthesis DSL"

  @elem{
    Converts a description of notes and drum beats to a playable @tt{.wav} format.
    The original program was known to suffer overhead from a type boundary
     to Typed Racket's @hyperlink["https://docs.racket-lang.org/math/array.html"]{@exact{\RktMeta{math/array}}}
     library@~cite[saf-cc-2015].
    Our benchmark incorporates the relevant library modules to form a
     self-contained program.
    @; TODO if the limitation doesn't belong here, where should it go?
    Notably, we had to monomorphize the core array data structure
     because opaque, polymorphic data structures may not be sent across type boundaries
     in Typed Racket.
  }
)
@(benchmark
  #:name 'gregor
  #:author "Jon Zeppieri"
  #:num-adaptor 2
  #:origin @hyperlink["https://docs.racket-lang.org/gregor/index.html"]{Library}
  #:purpose "Date and time library"
  #:external-libraries
    (list @hyperlink["https://docs.racket-lang.org/cldr-core/index.html"]{cldr}
          ", "
          @hyperlink["https://docs.racket-lang.org/tzinfo/index.html"]{tzinfo})

  @elem{
    Provides tools for manipulating date objects.
    For the benchmark, we build a range of date values and use them to run
     unit tests.
    Notably, the benchmark does not test @bm{gregor}'s string-parsing
     functions because they rely on an untyped library for ad-hoc polymorphism
     that is not yet supported by Typed Racket.
  }
)
@(benchmark
  #:name 'forth
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin @hyperlink["http://docs.racket-lang.org/forth/index.html"]{Library}
  #:purpose "Forth interpreter"

  @elem{
    Object-oriented calculator for Forth programs.
    The calculator maintains an environment of first-class objects representing
     commands.
    If this environment repeatedly crosses type boundaries it accumulates
     higher-order contract wrappers.
    These wrappers lead to an exponential slowdown in some configurations.
  }
)
@(benchmark
  #:name 'acquire
  #:author "Matthias Felleisen"
  #:num-adaptor 2
  #:origin @hyperlink["https://github.com/mfelleisen/Acquire"]{Educational}
  #:purpose "Game"

  @elem{
    Simulates a board game where players invest in real estate.
    The program is written in a stateful, object-oriented style.
    For the benchmark, we run a game between AI players.
  }
)
@(benchmark
  #:name 'fsm
  #:author "Matthias Felleisen"
  #:num-adaptor 1
  #:origin @hyperlink["https://github.com/mfelleisen/sample-fsm"]{Educational}
  #:purpose "Economy Simulator"

  @elem{
    Simulates the interactions of a population of finite-state automata.
    We measure two versions of this benchmark, one functional (@bm{fsm}) and
     one object-oriented (@tt{fsmoo}).
    The object-oriented verion frequently sends first-class
     objects across type boundaries; the functional version does the same
     with a mutable vector.
  }
)
@(benchmark
  #:name 'quad
  #:author "Matthew Butterick"
  #:num-adaptor 2
  #:origin @hyperlink["https://github.com/mbutterick/quad"]{Library}
  #:purpose "Typesetting"
  #:external-libraries (list @hyperlink["https://github.com/mbutterick/csp"]{csp})

  @elem{
    Converts S-expression source code to @tt{pdf} format.
    We have two versions of @bm{quad}:
     the first, @tt{quadMB}, uses fully-untyped and fully-typed configurations
     provided by the original author.
    This version has a high typed/untyped ratio because it uses the type system
     to enforce more datatype invariants than the untyped program---the Typed version is
     slower because it does more work.
    Our second version, @tt{quadBG}, uses types as weak as the untyped
     program and is therefore suitable for judging the implementation
     of Typed Racket rather than the user experience of Typed Racket.@note{Our
       conference paper gave data only for @tt{quadMB}@~cite[tfgnvf-popl-2016]}

    To give a concrete example of different types, here are the definitions
     for the core @tt{Quad} datatype from both @tt{quadMB} and @tt{quadBG}.

    @racket[(define-type QuadMB (Pairof Symbol (Listof QuadMB)))]

    @racket[(define-type QuadBG (Pairof Symbol (Listof Any)))]

    The former is a homogenous, recursive type.
    As such, the predicate asserting that an untyped value has type @tt{QuadMB}
     is a linear-time tree traversal.
    On the other hand, the predicate for @tt{QuadBG} is simply a constant-time
     combination of the built-in @racket[list?] and @racket[symbol?] predicates.
  }
)
]


@; -----------------------------------------------------------------------------
@subsection{Static Benchmark Characteristics}

@;

The table in @figure-ref{fig:bm} gives static characteristics
 of our benchmark programs.
Program size is measured by the lines of code (LOC) and number of modules.
In the ``Type Ann. LOC'' column, we give an upper bound on the number of
 annotations needed to fully type the program.
This upper bound supposes that every import statement is fully annotated with
 types for each imported identifier; in practice, only untyped identifiers
 imported by typed modules need annotations.
The number of modules is slightly more pragmatic measure of size as it
 also determines the size of our experiment.
A project with @exact{$N$} modules has @exact{$2^N$} gradually typed configurations.
The number of edges is taken from our module graphs.
Lastly, the ``Chaperones'' column is @todo{finish}.

@figure*["fig:bm" "Static characteristics of the benchmarks"
  @(benchmark-characteristics)
]


@; -----------------------------------------------------------------------------
@section[#:tag "sec:protocol"]{Experimental Protocol}

Our experiment measured the running time of all
 configurations in each benchmark's performance lattice.
This experiment was on three versions of Racket: version 6.2,
 version 6.3, and a development build of version 6.4.@todo{cite commit}.
@; {In particular,
@;  commit @hyperlink["https://github.com/racket/racket/commit/86a9c2e493d2b6ad70b3a80fef32a9e810c4e2db"]{86a9c2e4} from January 26, 2016.}
The machine we used to generate these numbers was a Linux machine with
 32 physical AMD Opteron 6376 2.3GHz cores and 128GB RAM.
We dedicated 29 of the machine's cores to running our experiment;
 each configuration was pinned to a single core and each benchmark program
 was run to completion before starting configurations for the next benchmark.

Timing information for a single configuration was obtained by compiling the
 code ahead of time and then running the configuration's main module.
Each run used a fresh instance of the Racket VM with the JIT compiler
 enabled.
After discarding one preliminary run, we collected timings
 for 10 runs of the configuration.
If these 10 timings were not normally distributed, we ran an additional 20
 timings and reported all 30 runs.
Otherwise, we reported the 10 runs and began the next configuration.
The means and standard errors in our analysis are computed from these sequences
 of 10 or 30 timings.

The scripts we used to run our experiments and the data we collected
 are available in the online supplement to this paper: @todo{update artifact}.


@; -----------------------------------------------------------------------------
@subsection[]{Detecting Stable Measurements with the Anderson-Darling test}

The running time of a configuration depends on many factors, ranging
 from heuristics in the Racket JIT compiler to machine-level caching and
 environment variable layout @todo{cite}.
    @; (not sure) http://plt.eecs.northwestern.edu/racket-machine/racket-machine.pdf
    @; http://www-plan.cs.colorado.edu/diwan/asplos09.pdf
    @; http://janvitek.org/pubs/r3.pdf
    @; https://people.cs.umass.edu/~emery/pubs/Stabilizer-UMass-CS-TR2011-43.pdf
We hope to mitigate these confounding effects by running our experiments on
 a single machine and taking the average of repeated runs.
To be precise, we assume that back-to-back runs of a configuration pinned to
 a single core are independent samples from a normal distribution.
We further assume by the law of large numbers that our sample mean after
 30 trials is close to the configuration's true average runtime.
    @; LLN : Jakob Bernoulli,
    @;       Ars Conjectandi: Usum & Applicationem Praecedentis Doctrinae in
    @;       Civilibus, Moralibus & Oeconomicis,
    @;       1713, Chapter 4, (Translated into English by Oscar Sheynin)
None of these assumptions are clearly valid@~cite[kj-tr-2013], but we believe the
 relative differences we observed between configurations in a lattice are correct,
 especially since we have observed similar differences on other machines @todo{cite popl}.

Running even 30 iterations, however, is prohibitive given the size of our
 experiment.
In total, we measured @todo{total} configurations on three versions of Racket.
To finish the experiment in a timely manner, we applied the Anderson-Darling
 normality test @todo{cite} after taking 10 measurements with a critical value
   @; http://www.hep.caltech.edu/~fcp/statistics/hypothesisTest/PoissonConsistency/AndersonDarling1954.pdf
 from Stephens @todo{cite}.
   @; http://www.math.utah.edu/~morris/Courses/6010/p1/writeup/ks.pdf
The judgment we made was about the likelihood of seeing a particular sequence
 of 10 runtimes assuming the data were from a normal distribution.
If the odds were less than @math{1%}, we ran an additional 20 iterations.
This led us to skip @todo{total} runs in total and led to no statistically
 significant differences in benchmarks that we tested exhaustively.
 @todo{which exhaustive?}

In order to explain our methodology precisely, we now summarize the key points from
 Stephens @todo{cite} regarding the Anderson-Darling test.
Our underlying distribution @math{F} is the distribution of runtimes obtained
 for one configuration run repeatedly on a single core.
We assume that @math{F} is normally distributed with an unknown mean
 and variance.
Let @exact|{$\vec{x}$}| denote our vector of 10 runtimes, sorted in increasing
 order.
We approximate the true mean @exact|{$\mu$}| and variance @exact|{$\sigma^2$}|
 of @math{F} by the sample mean and variance:

   @exact|{$$
     \myhat{\mu} = \Sigma_{i=0}^{9} \vec{x}_i / 10
     \hspace{2cm}
     \myhat{\sigma}^2 = \Sigma_{i=0}^{9} (x_i - \myhat{\mu})^2 / 9
   $$}|

Next we take the samples' z-scores and
 compute a probability vector @exact|{$\vec{h}$}| by mapping the standard
 normal CDF @exact|{$\Phi$}| over the z-scores.

    @exact|{$$\vec{h}_i = \Phi(\frac{x_i - \myhat{\mu}}{\myhat{\sigma}})$$}|
    @; Not \sigma^2

Intuitively, this vector is a histogram generated by our samples.
Each @exact|{$\vec{h}_i$}| is the probability that a new sample from the
 sample distribution we have observed from @exact|{$\vec{x}$}|
 will have a z-score less than or equal
 to the z-score of @exact|{$\vec{x}_i$}|.
The Anderson-Darling statistic is expressed in terms of @exact|{$\vec{h}$}|
 and essentially measures the symmetry of @exact|{$\vec{h}$}| with
 emphasis on the smallest and largest buckets.

    @exact|{$$
      A^2 = \frac{- [\Sigma_{i=0}^{9}
                      (2(i + 1) - 1)
                      * \ln{\vec{h}_i}
                      * \ln{(1 - \vec{h}_{9 - i})}
                    ] }{10} - 10
    $$}|

Following Stephens, we modify @exact|{$A^2$}| to compensate for the fact that
 @exact|{$\mu$}| and @exact|{$\sigma^2$}| are unknown @todo{cite}.

    @exact|{$$ A^{2\,'} = A^2 * (1 + \frac{4}{n} - \frac{25}{n^2}) $$}|

Finally, we declare the samples non-normal if @exact|{$A^{2\,'}$}| is greater than 1.
The value 1 was determined experimentally by Stephens for a @math{p}-value of
 @math{1%} given 10 samples and an unknown underlying mean and variance @todo{cite}.


@; -----------------------------------------------------------------------------
@section[]{Results}

This section presents the results of our experiment in terms of the
 measurements described in @Secref{sec:measurements}.
For two small benchmarks we explore full performance lattices
 but our main results are the plots in @todo{figure-ref}.

@todo{choosing LNM subsection}


@; -----------------------------------------------------------------------------
@subsection{Lattice-Based Evaluation}

@Secref{sec:fsm} described a 4-module benchmark, @bm{fsm}, and remarked that
 although the fully-typed configuration ran faster than the untyped program,
 a configuration with one typed module experienced an @todo{8,500x} slowdown.
The modules in @bm{fsm} were named @tt{automata}, @tt{main},
 @tt{population}, and @tt{utilities};
 the above-noted slow configuration assigned types only in @tt{population}.
Henceforth, we will represent configurations of @bm{fsm} as 4-bit binary
 strings corresponding to the module names in alphabetic order.
Using this notation, the configuration where only @tt{population} typed
 has the bitstring @tt{0010}.

@figure*["fig:fsm-lattice-6.2"
  @list{Annotated performance lattice for @bm{fsm}}
  @todo{(data-lattice 'fsm "6.2")}
]

@Figure-ref{fig:fsm-lattice-6.2} is the full performance lattice for @bm{fsm}
 run on Racket version 6.2.
Each configuration is represented by a sequence of colored shapes,
 corresponding to its bitstring.
A black shape represents a typed module and a white shape is an untyped one;
 the shape at index @math{i} from the left is colored
 iff the bit at index @math{i} from the left is 1,
 meaning the @tt{automata} module is typed.
Nodes are labeled with the configuration's overhead---computed
 as the configuration's mean runtime divided by the fully-untyped
 configuration's mean runtime---and
 the standard error of our timings for that configuration.
Configurations @tt{0010}, and @todo{others} suffer from a boundary between
 @tt{main} and @tt{population}.
These are by far the slowest configurations and no path through the lattice
 can avoid all of them.
But the other 8 configurations are at worst slightly slower than untyped,
 and two of these improve on the baseline performance.


@; -----------------------------------------------------------------------------
@; @subsubsection{Comparing Lattices}

Given that only half of @bm{fsm}'s configurations are slow, and that furthermore
 the slow configurations can be avoided by adding types to either @tt{main}
 (~25 lines) or @tt{population} (~50 lines), the @tt{fsm} benchmark suggests
 that Typed Racket's gradual typing is performant as of the v6.2 release.@note{
   Besides, of course, the overwhelming cost of repeatedly wrapping a vector.}
There are two ways to further validate the performance of Typed Racket:
 by comparing with other gradual type systems and by testing more programs (@Secref{sec:lnm}).

While not technically a competing implementation of Typed Racket v6.2,@note{
   Pycket @todo{cite} is a competing implementation of gradual typing for Typed Racket.
   At the time of writing Pycket could not run all our benchmark programs, but
    the Pycket authors will soon publish their own analysis.}
 we can compare our lattice against results for versions 6.3 and 6.4.
These lattices are shown in @Figure-ref{fig:fsm-lattice-6.3}.

@todo{describe}

@figure*["fig:fsm-lattice-6.3"
  @list{Annotated performance lattice for @bm{fsm} v6.2}
  @todo{(hc-append 8 ...)}
  @todo{(data-lattice 'fsm "6.3")}
  @todo{(data-lattice 'fsm "6.4.0.5")}
]

This is a sad story for Typed Racket, but we promise to improve for version 6.5.
@todo{say more}


@; -----------------------------------------------------------------------------
@subsection{Limitations of Lattice-Based Evaluation}

Inspecting the annotated performance lattice for @bm{fsm} is feasible and
 even gives insight as to why the worst configurations are slow.
At a glance, it is fairly easy to see that the 8 slow modules match the pattern
 @todo{pattern},
 @;@tt{*01*} or @tt{*10*},
 corresponding to a type boundary between @tt{main}
 and @tt{population}. @todo{use shapes?}
The number of nodes in a lattice, however, is exponential in the number of
 modules in a program.
Visual inspection quickly becomes impossible.

@todo{FILL IN HERE}

@; -----------------------------------------------------------------------------
@subsection{L-N/M Plots}
@figure["fig:lnm" "L-N/M plots"
  @(lnm-plots "6.2" "6.3" "6.4.0.5")
]

@; TODO
@; - maybe best to ease in to L-NM plots with an example; don't draw all lines yet
@; - change name from L-NM, add "backwards compat. section

The @id[NUM-BENCHMARKS] rows of cumulative distribution functions in @todo{Figure-ref}
 summarize the results from exhaustively exploring the performance lattices of
 our benchmarks on three versions of Racket.
In each graph, the @math{x}-axis represents a slowdown relative to the untyped program
 ranging from 1x to @id[MAX-OVERHEAD]x.
The @math{y}-axis is a count of the number of configurations,
 from @math{0} to @math{2^n}, scaled so that all graphs are the same height.
A point at @math{x=3} and @math{y=50} means that fifty of a program's
 configurations run with at most 3x overhead.
By definition, one of these 50 is the untyped configuration.

Each plot charts three series of points corresponding to three versions of
 Racket.
To ground our comparison of these series, we select values for @math{N}, @math{M}, and @math{L}.
Rather than re-use the parameters from our prior work@~cite[tfgnvf-popl-2016],
 we choose @math{N=0.2} and @math{M=5}.
The former is inspired by a remark about the acceptable overhead of
 garbage collection technology@~cite[u-sde-1984].
Of course, these parameters are just examples.
Application-specific requirements will dictate levels of acceptable and
 usable overhead in practice.

We present three columns of plots, representing values of @math{L} ranging
 between 0 and 2.
If @math{L} is zero, the curves represents the total number of configurations
 with performance no worse than the overhead on the x-axis.
For arbitrary @math{L}, the curves give the number of configurations that
 can reach a configuration with performance no worse than the overhead on the
 x-axis in at most @math{L} conversion steps.

A useful way to read these figures is to first pick an overhead value,
 say @id[EXAMPLE-OVERHEAD], then follow the y-axis until it intersects
 one of the curves.
Taking @id[EXAMPLE-BENCHMARK] for example, the leftmost plot shows that
 @todo{how many} configurations run within a @id[EXAMPLE-OVERHEAD] slowdown
 over the untyped configuration on Racket version 6.2.
Upgrading to Racket version @todo{6.4.0.5} gives @todo{modest}.


@; -----------------------------------------------------------------------------
@; @section[#:tag "sec:all-results"]{Interpretation}

The ideal curves would be flat lines at a graph's top.
Such a result would mean that all configurations were as fast as
 (or faster than) the untyped one on Racket v6.2 and performance did not
 degrade in more recent versions.
The worst scenario would be flat lines at the graph's bottom,
 indicating that all configurations are more than 20x slower than the untyped one
 even in the most recent Racket release.

Of course, the ideal shape is difficult to achieve because of the overwhelming
 cost of the dynamic checks inserted at the boundaries between typed and untyped code.
The next-best shape is a nearly-vertical line that reaches the top at a low x-value.
All else being equal, a steep slope anywhere on the graph is desirable because
 the number of acceptable programs quickly increases at some point below the
 20x slowdown mark.

For each benchmark, we evaluate the actual graphs against these expectations.
Our approach is to focus on the left column, where @math{L}=0, and to consider the
 center and right column as rather drastic countermeasures to recover
 performance.@note{Increasing @math{L} should remove pathologically-bad cases.} 
In @todo{secref} we explain the changes between different versions of Racket
 and the pathologies in each benchmark.

@lnm-descriptions[
  @lnm['sieve]{
    The flat line at @math{L}=0 shows that half of all configurations suffer
    unacceptable overhead. As there are only 4 configurations in the lattice
    for @tt{sieve}, increasing @math{L} improves performance.
  }

  @lnm['morsecode]{
    The steep lines show that a few configurations suffer modest overhead (below 2x),
    otherwise @tt{morse-code} performs well.
    Increasing @math{L} improves the worst cases.
  }

  @lnm['mbta]{
    These lines are also steep, but flatten briefly at 2x.
    This coincides with the performance of the fully-typed
    configuration.
    As one would expect, freedom to type additional modules adds configurations
    to the @deliverable{2} equivalence class.
  }

  @lnm['acquire]{
    Nope!
  }

  @lnm['fsm]{
    Nope!
  }

  @lnm['fsmoo]{
    Nope!
  }

  @lnm['forth]{
    Nope!
  }

  @lnm['zordoz.6.2]{
    Nope!
  }

  @lnm['zordoz.6.3]{
    Plots here are similar to @tt{mbta}.
    There is a gap between the performance of the fully-typed
    configuration and the performance of the next-fastest lattice point.
  }

  @lnm['suffixtree]{
    The wide horizontal areas are explained by the performance lattice in
    @figure-ref{fig:suffixtree}: configurations' running times are not evenly
    distributed but instead vary drastically when certain boundaries exist.
    Increasing @math{L} significantly improves the number of acceptable configuration
    at 10x and even 3x overhead.
  }

  @lnm['lnm]{
    These results are ideal.
    Note the large y-intercept at @math{L}=0.
    This shows that very few configurations suffer any overhead.
  }

  @lnm['kcfa]{
    The most distinctive feature at @math{L}=0 is the flat portion between 1x
    and 6x. This characteristic remains at @math{L}=1, and overall performance
    is very good at @math{L}=2.
  }

  @lnm['snake]{
    The slope at @math{L}=0 is very low.
    Allowing @math{L}=1 brings a noticeable improvement above the 5x mark,
    but the difference between @math{L}=1 and @math{L}=2 is small.
  }

  @lnm['tetris]{
    Each @tt{tetris} plot is essentially a flat line.
    At @math{L}=0 roughly 1/3 of configurations lie below the line.
    This improves to 2/3 at @math{L}=1 and only a few configurations suffer overhead
    when @math{L}=2.
  }

  @lnm['synth]{
    Each slope is very low.
    Furthermore, some configurations remain unusable even at @math{L}=2.
    These plots have few flat areas, which implies that overheads are spread
    evenly throughout possible boundaries in the program.
  }

  @lnm['gregor]{
    These steep curves are impressive given that @tt{gregor} has 13 modules.
    Increasing @math{L} brings consistent improvements.
  }

  @lnm['zombie]{
  }

  @lnm['quadBG]{
  }

  @lnm['quadMB]{
    The @bm{quadMB} plots follow the same pattern as @bm{mbta} and @bm{zordoz}, despite being visually distinct.
    In all three cases, there is a flat slope for overheads below the typed/untyped ratio and a steep increase just after.
    The high typed/untyped ratio is explained by small differences in the original author-supplied variants.
  }
]


@; -----------------------------------------------------------------------------
@subsection{Summary Tables}
@; Does this need a subsection?

@figure*["fig:lnm-summary" "Summary Statistics"
  @(lnm-summary "6.2" "6.3" "6.4.0.5")
]

The table in @Figure-ref{fig:lnm-summary} gives a second perspective on our
 datasets, giving a typed/untyped ratio, mean, max, and @usable["N" "M"]
 percentages for each tested version of Racket.

The typed/untyped ratio is the slowdown or speedup of fully typed code
 over untyped code.
Values smaller than @math{1.0} indicate a speedup due to Typed Racket optimizations.
Values larger than @math{1.0} are slowdowns caused by interaction with untyped
 libraries or untyped parts of the underlying Racket runtime.
The ratios range between @todo{min} and @todo{max}.

The maximum overhead is computed by finding the running time of the slowest
 configuration and dividing it by the running time of the untyped configuration.
The average overhead is obtained by computing the average over all
 configurations (excluding the fully-typed and untyped configurations) and
 dividing it by the running time of the untyped configuration.
Maximum overheads range from @todo{min} to @todo{max}.
Average overheads range from @todo{min} to @todo{max}.

The @deliverable{3} and @usable["3" "10"] counts are computed for @math{L=0}.
In parentheses, we express these counts as a percentage of all configurations
 for the benchmark.
