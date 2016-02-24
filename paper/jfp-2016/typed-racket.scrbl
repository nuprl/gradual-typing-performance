#lang scribble/base


@require[
  "common.rkt"
  "typed-racket.rkt"
]

@title[#:tag "sec:tr"]{Evaluating Typed Racket}

For our evaluation of Typed Racket, we use a suite of
 @id[NUM-BENCHMARKS] programs
 and generate timings over the whole performance lattice for each.
As lattices for projects with more than 6 modules are too large to analyze at
 a glance (or fit within 8 inch margins), we present our results in terms of
 the proportion of @step["L" "N" "M"] configurations.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:bm"]{The Benchmark Programs}

The benchmarks themselves are representative of actual user code yet
 small enough that exhaustive performance evaluation remains tractable.
Where relevant, we include hyperlinks to external libraries used by a benchmark.
Other benchmarks are self-contained, aside from dependencies on core Racket
 libraries.

Although we give specific descriptions of the inputs we ran each benchmark on,
 these inputs are more-or-less arbitrary.
That is, we have experimented with inputs of various size and content but found
 the relative overheads due to type boundaries remained the same.
In most cases our documented input size is a compromise between having an
 untyped runtime long enough to be stable against operating system effects
 but short enough that the slowest typed/untyped configurations finished
 reasonably quickly.


@; -----------------------------------------------------------------------------
@subsection{Benchmark Descriptions}
@todo{why do snake & tetris have different num. of moves?}
@todo{descriptions look very bad}


@benchmark-descriptions[
@(benchmark
  #:name 'sieve
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin "Synthetic"
  #:purpose "Finds prime numbers using the Sieve of Eratosthenes."

  @elem{
  We created the @bm{sieve} benchmark to demonstrate a scenario where user
   code closely interacts with higher-order library code---in this case, a stream
   library.
  When fully typed or untyped, @bm{sieve} quickly computes the ten-thousandth
   prime number.}
)

@(benchmark
  #:name 'morsecode
  #:author "John Clements & Neil Van Dyke"
  #:num-adaptor 0
  #:origin @hyperlink["https://github.com/jbclements/morse-code-trainer/tree/master/morse-code-trainer"]{Library}
  #:purpose "Generate morse code strings, compare against user input"

  @elem{
  The original program is a plays an audio clip, waits for keyboard input,
   then scores the input based on its Levenshtein distance from the
   correct answer.
  Our benchmark takes the cartesian product of 300 common English words,
   translates each pair to morse code, and finds the Levenshtein distance
   between words.}
)

@(benchmark
  #:name 'mbta
  #:author "Matthias Felleisen"
  #:num-adaptor 0
  #:origin "Educational"
  #:purpose "Answer reachability queries about Boston's transit system"
  #:external-libraries (list @elem{graph@note{@url["http://github.com/stchang/graph"]}})

  @elem{
  Builds a graph representation of Boston's subway system and
   answers a series of reachability queries.
  The original program ran an asynchronous client/server framework
   but our benchmark is single-threaded to cooperate with Racket's sampling
   profiler, which we use in @Secref{sec:postmortem} to analyze the cause
   of performance overhead.
  }
)

@(benchmark
  #:name 'zordoz
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin @hyperlink["http://github.com/bennn/zordoz"]{Library}
  #:purpose "Explore bytecode (.zo) files"
  #:external-libraries (list @elem{compiler-lib@note{@url["http://docs.racket-lang.org/raco/decompile.html#%28mod-path._compiler%2Fdecompile%29"]}})

  @elem{
  This program gives a shell-style interface for incrementally traversing
   Racket bytecode.
  Our benchmark decompiles its own bytecode files and
   counts the number of branch instructions in the result.

  The Racket bytecode format changed between versions 6.2 and 6.3 with
   the release of the set-of-scopes macro expander@~cite[f-popl-2016].
  Consequently, our @bm{zordoz} benchmark is slightly different before and
   after version 6.2; however, the relative difference between configurations
   in the performance lattice is the same across bytecode formats.
  }
)

@(benchmark
  #:name 'suffixtree
  #:author "Danny Yoo"
  #:num-adaptor 1
  #:origin @hyperlink["https://github.com/dyoo/suffixtree"]{Library}
  #:purpose "Implement Ukkonen's suffix tree algorithm"

  @elem{
    We use a longest-common-subsequence algorithm provided by this library
     to compare one million pairs of English words.}
)

@(benchmark
  #:name 'lnm
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin "Synthetic"
  #:purpose "Produce L-N/M plots"
  #:external-libraries (list @elem{plot@note{@url["https://docs.racket-lang.org/plot/"]}}
                             @elem{@tt{racket/statistics}@note{@url["https://docs.racket-lang.org/math/stats.html"]}})

  @elem{
    We developed the @bm{lnm} program to summarize data lattices and generate
     the figures in @Secref{sec:lnm-plot}.
    Our benchmark creates, but does not render, plots for the @bm{gregor} benchmark.}
)

@(benchmark
  #:name 'kcfa
  #:author "Matt Might"
  #:num-adaptor 4
  #:origin @hyperlink["http://matt.might.net/articles/implementation-of-kcfa-and-0cfa/"]{Blog post}
  #:purpose "Demo of the k-CFA algorithm"

  @elem{
    Simple, inefficient implementation of k-CFA.
    Our benchmark runs the analysis on a lambda calculus term
     that computes @tt{2 * (1 + 3) = 2 * 1 + 2 * 3}.
  }
)

@(benchmark
  #:name 'zombie
  #:author "David Van Horn"
  #:num-adaptor 1
  #:origin @hyperlink["https://github.com/philnguyen/soft-contract"]{Educational}
  #:purpose "Game"

  @elem{
    In this game, the player must keep his marker away from
     computer-controlled "zombie" markers.
    We run the game on a pre-defined list of @todo{how many?} commands.

    As noted by Nguyễn @|etal|@~cite[nthvh-icfp-2014], the original
     program was implemented in an object-oriented style but converted
     to a functional version to evaluate soft contract verification.
    Our benchmark is based on a typed version of their functional game.
  }
)

@(benchmark
  #:name 'snake
  #:author "David Van Horn"
  #:num-adaptor 1
  #:origin @hyperlink["https://github.com/philnguyen/soft-contract"]{Educational}
  #:purpose "Game"

  @elem{
  Implements a small game where a growing and moving snake avoids walls and
   its own tail.
  Our benchmark runs a pre-recorded history of 50,000 moves.
  These moves update the game state, but do not produce GUI output.
  Our benchmark is a gradually typed version of the @bm{snake} game from
   Nguyễn @|etal|@~cite[nthvh-icfp-2014].
  }
)

@(benchmark
  #:name 'tetris
  #:author "David Van Horn"
  #:num-adaptor 1
  #:origin @hyperlink["https://github.com/philnguyen/soft-contract"]{Educational}
  #:purpose "Game"

  @elem{
    This version of tetris is also adapted from Nguyễn @|etal|@~cite[nthvh-icfp-2014].
    Our benchmark run a pre-recorded set of 5,000 moves and does not include
     a GUI.}
)

@(benchmark
  #:name 'synth
  #:author "Vincent St. Amour and Neil Toronto"
  #:num-adaptor 1
  #:origin @hyperlink["http://github.com/stamourv/synth"]{Library}
  #:purpose "Music synthesis DSL"

  @elem{
    Converts a description of notes and drum beats to a playable @tt{.wav} format;
     specifically, a 10-second clip from @hyperlink["https://en.wikipedia.org/wiki/Funkytown"]{Funkytown}.
    The original program was known to suffer overhead from a type boundary
     to Typed Racket's @hyperlink["https://docs.racket-lang.org/math/array.html"]{math/array}
     library, so our benchmark incorporates 5 modules from
     the library.
    Notably, we had to monomorphize these math library modules because of
     restrictions sending polymorphic data structures across type boundaries.
    Otherwise, this benchmark is the same used by St. Amour @|etal|@~cite[saf-cc-2015].}
)

@(benchmark
  #:name 'gregor
  #:author "Jon Zeppieri"
  #:num-adaptor 2
  #:origin @hyperlink["https://docs.racket-lang.org/gregor/index.html"]{Library}
  #:purpose "Date & time library"
  #:external-libraries
    (list @elem{cldr@note{@url["https://docs.racket-lang.org/cldr-core/index.html"]}}
          @elem{tzinfo@note{@url["https://docs.racket-lang.org/tzinfo/index.html"]}})

  @elem{
    The @hyperlink["https://docs.racket-lang.org/gregor/index.html"]{gregor}
     library provides a variety of tools for working with date objects.
    Our benchmark creates a list of 40 dates---half historic, half arbitrary---and
     runs comparison and conversion operators on each.
    We omit @bm{gregor}'s string-parsing utilities because they use an
     untyped mechanism for ad-hoc polymorphism that is not supported by
     Typed Racket.}
)

@(benchmark
  #:name 'forth
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin @hyperlink["http://docs.racket-lang.org/forth/index.html"]{Library}
  #:purpose "Forth interpreter"

  @elem{
    This Forth interpreter began as a purely functional calculator
     that let the user define new commands at run-time.
    We converted the program to an object-oriented style and found the
     cost of sharing first-class objects over a type boundary prohibitive.
    In fact, our benchmark runs only @todo{how many?} commands---the worst
     configurations in Racket version 6.2 are exponentially slower as
     this number increases.
  }
)

@(benchmark
  #:name 'acquire
  #:author "Matthias Felleisen"
  #:num-adaptor 2
  #:origin @hyperlink["https://github.com/mfelleisen/Acquire"]{Educational}
  #:purpose "Board Game"

  @elem{
    You know it
  }
)

@(benchmark
  #:name 'fsm
  #:author "Matthias Felleisen"
  #:num-adaptor 1
  #:origin @hyperlink["https://github.com/mfelleisen/sample-fsm"]{Educational}
  #:purpose "Economy Simulator"

  @elem{
    Simulates the interactions of a group of automata.
    Each participant employs a pre-determined strategy to maximize its
     payoff in a sequence of interaction rounds.

    Our benchmark uses a population of 100 automata and simulates 1000 rounds.
    We measure two versions of this benchmark, one functional (@bm{fsm}) and
     one object-oriented (@tt{fsmoo}).
    Like our @bm{forth} benchmark, the object-oriented verion uses first-class
     classes across a type boundary.
  }
)

@(benchmark
  #:name 'quad
  #:author "Matthew Butterick"
  #:num-adaptor 2
  #:origin @hyperlink["https://github.com/mbutterick/quad"]{Library}
  #:purpose "Typesetting"
  #:external-libraries (list @elem{csp@note{@url["https://github.com/mbutterick/csp"]}})

  @elem{
    @hyperlink["http://github.com/mbutterick/quad"]{Quad} is an experimental
     document processor.
    It converts S-expression source code to @tt{pdf}.

    We measure two versions of @bm{quad}.
    The first, @tt{quadMB}, uses fully-untyped and fully-typed configurations
     provided by the original author.
    This version has a high typed/untyped ratio because it uses the type system
     enforces more properties than the untyped program---the Typed version is
     slower because it is doing more work.
    Hence our second version, @tt{quadBG}, which uses types as weak as the untyped
     program and is therefore suitable for judging the @emph{implementation}
     of Typed Racket rather than the @emph{user experience} of Typed Racket.@note{Our
       conference paper gave data only for @tt{quadMB}@~cite[tfgnvf-popl-2016]}

    To give a concrete example of different types, here are the definitions
     for the core @tt{Quad} datatype from both @tt{quadMB} and @tt{quadBG}.

    @;@racketblock[
    @tt{ (define-type QuadMB (Pairof Symbol (Listof Quad))) }

    @tt{ (define-type QuadBG (Pairof Symbol (Listof Any))) }
    @;]

    The former is a homogenous, recursive type.
    As such, the predicate asserting that an untyped value has type @tt{Quad}
     is a linear-time tree traversal.
    On the other hand, the predicate for @tt{QuadBG} is simply the composition
     of the built-in @racket[list?] and @racket[symbol?] functions.@note{In particular,
       @racket[(lambda (v) (and (list? v) (symbol? (car v))))].}
  }
)]


@; -----------------------------------------------------------------------------
@subsection{Benchmark Characteristics}

The table in @figure-ref{fig:bm} lists and summarizes our benchmark programs.
For each, we give an approximate measure of the program's size and
 a diagram of its module structure.

Size is measured by the number of modules and lines of code (LOC) in a program.
The number of modules also determines the number of gradually-typed
 configurations to be run when testing the benchmark, as a program with @math{n} modules
 can be gradually-typed in @exact{$2^n$} possible configurations.
Lines of code is less important for evaluating macro-level gradual typing,
 but gives a sense of the overall complexity of each benchmark.
Moreover, the Type Annotations LOC numbers are an upper bound on the annotations required
 at any stage of gradual typing because each typed module in our experiment
 fully annotates its import statements.
In reality, imports from other typed modules are not annotated.

The column labeled ``Other LOC'' measures the additional infrastructure required
 to run each project for all typed-untyped configurations.
This count includes project-wide type definitions, typed interfaces to
 untyped libraries, and any so-called type adaptor modules (@todo{secref})
 we used in our experiment.

The module structure graphs show a dot for each module in the program.
An arrow is drawn from module A to module B when module A imports definitions
 from module B.
When one of these modules is typed and the other untyped, the imported definitions
 are wrapped with a contract to ensure type soundness.
@todo{colors}
@;To give a sense of how ``expensive'' the contracts at each boundary are, we color
@; arrows to match the absolute number of times contracts at a given boundary
@; are checked. These numbers are independent from the actual configurations.
@;The colors fail to show the cost of checking data structures
@;imported from another library or factored through an adaptor module.
@;For example, the @bm{kcfa} graph has many thin black edges because the modules
@;only share data definitions. The column labeled ``Adaptors + Libraries''
@;reports the proportion of observed contract checks due to adaptor modules and
@;libraries.

@figure*["fig:bm" "Characteristics of the benchmarks"
  @(benchmark-characteristics)
]


@; -----------------------------------------------------------------------------
@section[#:tag "sec:tr"]{Experimental Protocol}

Our experiment measures the running time of all
 configurations in each benchmark's performance lattice.
This experiment was repeated for three versions of Racket: the 6.2 release,
 the 6.3 release, and a pre-release candidate for version 6.4.@note{In particular,
  commit @hyperlink["https://github.com/racket/racket/commit/86a9c2e493d2b6ad70b3a80fef32a9e810c4e2db"]{86a9c2e4} from January 26, 2016.}
The machine we used to generate these numbers was a 64GB RAM Linux machine with
 32 physical AMD Opteron 6376 2.3GHz cores.
We dedicated 29 of the machine's cores to running our experiment;
 each configuration was pinned to a single core and each benchmark program
 was run to completion before starting configurations for the next benchmark.

Timing information for a single configuration was obtained by compiling the
 code ahead of time and then running the configuration's main module repeatedly.
Each run took place on a new instance of the Racket VM with the JIT compiler
 enabled.@note{The exact command we used was @tt{racket main}}.
After discarding one preliminary run, we collected timings
 for 10 runs of the configuration.
If these 10 timings were not normally distributed, we ran an additional 20
 timings and reported all 30 runs.
Otherwise, we reported the 10 runs and began the next configuration.
The means and standard errors in our analysis are computed from these sequences
 of 10 or 30 timings.

The scripts we used to run our experiments and the data we collected
 are available in the online supplement to this article: @todo{update artifact}.


@; -----------------------------------------------------------------------------
@subsection[]{Detecting Stable Measurements with the Anderson-Darling test}

The running time of a configuration is dependent on many factors, ranging
 from heuristics in the Racket JIT compiler to machine-level caching and
 environment variable layout.
    @; http://plt.eecs.northwestern.edu/racket-machine/racket-machine.pdf
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
Frankly none of these assumptions are obviously valid, but we believe the
 relative differences we observed between configurations in a lattice are correct,
 especially since we have observed similar differences on other machines @todo{cite popl}.

Running even 30 iterations, however, is prohibitive given the size of our
 experiment.
In total, we measured @todo{total} configurations on three versions of Racket.
To finish the experiment in a timely manner, we applied the Anderson-Darling
 test @todo{cite} after taking 10 measurements with a critical value
   @; http://www.hep.caltech.edu/~fcp/statistics/hypothesisTest/PoissonConsistency/AndersonDarling1954.pdf
 from Stephens @todo{cite}.
   @; http://www.math.utah.edu/~morris/Courses/6010/p1/writeup/ks.pdf
The judgment we made was about the likelihood of seeing a particular sequence
 of 10 runtimes assuming the data were from a normal distribution.
If the odds were less than @math{1%}, we ran an additional 20 iterations.
This led us to skip @todo{total} runs in total and led to no statistically
 significant differences in benchmarks that we tested exhaustively.
 @todo{which exhaustive?}

For readers hoping to reproduce our setup, we now summarize the key points from
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
     \myhat{\sigma}^2 = \Sigma_{i=0}^{9} (\mu_i - \myhat{\mu})^2 / 9
   $$}|

Next we take the samples' z-scores and
 compute a probability vector @exact|{$\vec{h}$}| by mapping the standard
 normal CDF @exact|{$\Phi$}| (i.e. with parameters @exact|{$\mu = 0$}| and
 @exact|{$\sigma = 1$}|) over the z-scores.

    @exact|{$$\vec{h}_i = \Phi(\frac{\myhat{x}_i - \myhat{\mu}}{\myhat{\sigma}})$$}|
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

    @exact|{$$ A^{*\,2} = A^2 * (1 + \frac{4}{n} - \frac{25}{n^2}) $$}|

Finally, we declare the samples non-normal if @exact|{$A^{*\,2}$}| is greater than 1.
The value 1 was determined experimentally by Stephens for a @math{p}-value of
 @math{1%} given 10 samples and an unknown underlying mean and variance @todo{cite}.


@; -----------------------------------------------------------------------------
@section[]{Results}

This section presents the results of our experiment in terms of the
 measurements described in @Secref{sec:measurements}.
For two small benchmarks we explore full performance lattices
 but our main results are the plots in @todo{figure-ref}.


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
 @tt{*01*} or @tt{*10*}, corresponding to a type boundary between @tt{main}
 and @tt{population}. @todo{use shapes?}
The number of nodes in a lattice, however, is exponential in the number of
 modules in a program.
Visual inspection quickly becomes impossible.

To demonstrate, @Figure-ref{fig:suffixtree-lattice-6.2} is the annotated
 performance lattice for @bm{suffixtree}.
As background, @bm{suffixtree} consists of six modules:
 @tt{data} defines label and tree nodes,
 @tt{label} defines functions on suffixtree node labels,
 @tt{lcs} computes longest common substrings,
 @tt{main} provides input to @tt{lcs},
 @tt{node} creates and traverses suffix tree nodes @todo{do rename},
 and @tt{ukkonen} builds suffix trees via Ukkonen's algorithm.
Therefore, the configuration with only @tt{main} and @tt{ukkonen} typed
 has bitstring @tt{000101}.

@figure*["fig:suffixtree-lattice-6.2"
  @list{Annotated performance lattice for @bm{suffixtree} v6.2}
  @todo{(data-lattice 'suffixtree "6.2")}
]

Basic questions about the performance lattice are much harder
 to answer for these 64 configurations.
Which configurations are the slowest?
Why might they be slow?
    @; Readers with a magnifying glass can answer, just as readers with
    @; poster-sized paper can answer questions about a 10-module lattice.
We now explain key points in this lattice, but this is the last such explanation
 we will give before introducting a graphical analogue to performance lattices.
@todo{2 lattices?}

@todo{BEGIN check numbers}

The fully typed configuration is again @emph{faster} than the fully untyped
 configuration.
The difference is approximately 30%, which puts the typed/untyped ratio at 0.7.
This improvement is due to specialization of arithmetic operations and
 field accesses by Typed Racket's optimizer @~cite[thscff-pldi-2011].
When the optimizer is turned off, the ratio goes back up to 1.
Sadly, the performance improvement of the typed configuration is the
 only good part of this benchmark.
@itemlist[
  @item{
   Almost all partially typed configurations exhibit large slowdowns,
    the worst begin 105x.
  }
  @item{
   Adding types to any of the workhorse modules---@tt{data}, @tt{label},
    or @tt{structs}---while leaving all other modules untyped causes slowdown of
    at least 35x.
   Not surprisingly, these modules are tightly coupled in the program.
  }
  @item{
    Not a single path from untyped to typed converting one module at a time
     avoids worst-case overheads within 20x.
  }
  @item{
   The five slowest configurations are @todo{list}.
   These all have a type boundary between the first two modules: @tt{data} and
    @tt{label}.
   Configurations where @tt{data} and @tt{label} have the same type
    have an average overhead of @todo{X}.
  }
]

The performance lattice for @tt{suffixtree} is bad news Typed Racket v6.2.
It exhibits many performance ``valleys'' in which a group of similar configurations
 all suffer large performance overhead.
Consider starting with the untyped program and choosing
 to add types to @tt{label}.
The program slows down by a factor of 88x.
Without any guidance, a developer may then choose to add types to @tt{structs};
 now the program slows to 104x.
After that, typing @tt{main} (104x), @tt{ukkonen} (99x), and @tt{lcs} (103x)
 do little to improve performance.
It is only when all the modules are typed that performance becomes acceptable
 again (0.7x), at which point it is probably too late to convince the programmer
 that gradual typing is useful.
@; Useful for more than the static check of making sure intermediate programs type-check

@todo{END check numbers}


@; -----------------------------------------------------------------------------
@subsection{L-N/M Plots}
@(lnm-plots "6.2" "6.3" "6.4.0.5")

The @id[NUM-BENCHMARKS] rows of cumulative distribution functions in @todo{Figure-ref}
 summarize the results from exhaustively exploring the performance lattices of
 our benchmarks on three versions of Racket.
For this experiment we have chosen values of 3x and 10x for @math{N} and
 @math{M}, respectively, and allow up to @math{L = 2} additional type conversion
 steps.
Hence we draw vertical lines representing @math{N} and @math{M} and organize
 our plots in 3 columns, corresponding to values of @math{L} between 0 and 2.
These values are rather liberal,@note{We would expect that most production
  contexts would not tolerate anything higher than 2x, if that much.}
  but serve to ground our discussion.
The three curves give results for Racket versions 6.2, 6.3, and 6.4.0.5.

In each graph, the x-axis represents a slowdown relative to the untyped program
 (from 1x to @id[MAX-OVERHEAD]x).
The y-axis is a count of the number of configurations
 (from @math{0} to @math{2^n}) scaled so that all graphs are the same height.
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
@subsection{So Much Data}

@todo{Motivational text}

@figure*["fig:lnm-summary" "Summary Statistics"
  @(lnm-summary "6.2" "6.3" "6.4.0.5")
]

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

@todo{Each benchmark gets 3 columns for the 3 versions of Racket.}
