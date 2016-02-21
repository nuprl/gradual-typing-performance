#lang scribble/base


@require["common.rkt" "typed-racket.rkt"]

@title[#:tag "sec:tr"]{Evaluating Typed Racket}

For our evaluation of Typed Racket, we use a suite of
 @id[NUM-BENCHMARKS] programs
 and generate timings over the whole performance lattice for each.
As lattices for projects with more than 6 modules are too large to analyze at
 a glance, we present our results in terms of the proportion of
 @step["L" "N" "M"] configurations.

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

@subsection{Benchmark Descriptions}
@todo{why do snake & tetris have different num. of moves?}
@todo{descriptions look very bad}


@(benchmark-descriptions
@(benchmark
  #:name 'sieve
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin "Synthetic"
  #:purpose "Finds prime numbers using the Sieve of Eratosthenes."

  @elem{
  We created the @tt{sieve} benchmark to demonstrate a scenario where user
   code closely interacts with higher-order library code---in this case, a stream
   library.
  When fully typed or untyped, @tt{sieve} quickly computes the ten-thousandth
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
  Consequently, our @tt{zordoz} benchmark is slightly different before and
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
    We developed the @tt{lnm} program to summarize data lattices and generate
     the figures in @Secref{sec:lnm-plot}.
    Our benchmark creates, but does not render, plots for the @tt{gregor} benchmark.}
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
  Our benchmark is a gradually typed version of the @tt{snake} game from
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
    We omit @tt{gregor}'s string-parsing utilities because they use an
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
    We measure two versions of this benchmark, one functional (@tt{fsm}) and
     one object-oriented (@tt{fsmoo}).
    Like our @tt{forth} benchmark, the object-oriented verion uses first-class
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

    We measure two versions of @tt{quad}.
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
))

@subsection{Benchmark Characteristics}

The table in @figure-ref{fig:bm} lists and summarizes our @id[NUM-BENCHMARKS]
 benchmark programs.
For each, we give an approximate measure of the program's size and
 a diagram of its module structure.

Size is measured by the number of modules and lines of code (LOC) in a program.
Crucially, the number of modules also determines the number of gradually-typed
 configurations to be run when testing the benchmark, as a program with @math{n} modules
 can be gradually-typed in @exact{$2^n$} possible configurations.
Lines of code is less important for evaluating macro-level gradual typing,
 but gives a sense of the overall complexity of each benchmark.
Moreover, the Type Annotations LOC numbers are an upper bound on the annotations required
 at any stage of gradual typing because each typed module in our experiment
 fully annotates its import statements.

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
@;For example, the @tt{kcfa} graph has many thin black edges because the modules
@;only share data definitions. The column labeled ``Adaptors + Libraries''
@;reports the proportion of observed contract checks due to adaptor modules and
@;libraries.

@figure*["fig:bm" "Characteristics of the benchmarks"
  @(benchmark-characteristics)
]


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
 code ahead of time and then running the configuration's @tt{main} function repeatedly.
Each run took place on a new instance of the Racket VM.@note{The exact command
 we used was @tt{racket main.rkt}}.
After running and ignoring one preliminary run, we would collect timings
 for 10 runs of the configuration.
If these 10 timings were not normally distributed, we ran an additional 20
 timings and reported all 30 runs.
Otherwise, we reported the 10 runs and began the next configuration.
The means and standard errors in our analysis are computed from these sequences
 of 10 or 30 timings.

The scripts we used to run our experiments and the data we collected
 are available in the online supplement to this article: @todo{update artifact}.


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
 relative differences we observed between configurations in a lattice are correct.

Running even 30 iterations, however, is prohibitive given the size of our
 experiment.
In total, we measured @todo{total} configurations each on three versions of Racket.
To finish the experiment in a timely manner, we applied the Anderson-Darling
 test @todo{cite} after taking 10 measurements with a critical value experimentally
   @; http://www.hep.caltech.edu/~fcp/statistics/hypothesisTest/PoissonConsistency/AndersonDarling1954.pdf
 shown by Stephens @todo{cite} to give a @math{p} value of @math{1%}.
   @; http://www.math.utah.edu/~morris/Courses/6010/p1/writeup/ks.pdf
The judgment we made was about the likelihood of seeing a particular sequence
 of 10 runtimes assuming the data were from a normal distribution.
If the odds were less than @math{1%}, we ran an additional 20 iterations.
This led us to skip @todo{total} runs in total and led to no observable
 differences in the overall timings taken from the same machine.

For readers hoping to reproduce our setup, we now summarize the key points from
 Stephens @todo{cite}.
Our underlying distribution @math{F} is the distribution of runtimes obtained
 for one configuration run repeatedly on a single core.
We assume that @math{F} is normally distributed, but do not assume its mean
 or variance.
Let @exact|{$\vec{x}$}| denote our vector of 10 runtimes, sorted in increasing
 order.
We approximate the true mean @exact|{$\mu$}| and variance @exact|{$\sigma^2$}|
 of @math{F} by the statistics:

   @exact|{$$
     \hat{\mu} = \Sigma_{i=0}^{9} \vec{x}_i / 10
     \hspace{2cm}
     \hat{\sigma}^2 = \Sigma_{i=0}^{9} (\mu_i - \hat{\mu})^2 / 9
   $$}|

Next we take the samples' z-scores and
 compute a probability vector @exact|{$\vec{h}$}| by mapping the standard
 normal CDF @exact|{$\Phi$}| (i.e. with parameters @exact|{$\mu = 0$}| and
 @exact|{$\sigma = 1$}|) over the z-scores.

    @exact|{$$\vec{h}_i = \Phi(\frac{\hat{x}_i - \hat{\mu}}{\hat{\sigma}})$$}|
    @; Not \sigma^2

Intuitively, this vector is a histogram generated by our samples.
Each @exact|{$\vec{h}_i$}| is the probability that a new sample from the
 sample distribution we have observed will have a z-score less than or equal
 to the z-score of @exact|{$\vec{x}_i$}|.
The Anderson-Darling statistic is expressed in terms of @exact|{$\vec{h}$}|
 and essentially measures how symmetric @exact|{$\vec{h}$}| is with
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


@section[]{Example: suffixtree}


@; -----------------------------------------------------------------------------
@; @section{Suffixtree in Depth}
@; 
@; To illustrate the key points of the evaluation, this section describes
@; one of the benchmarks, @tt{suffixtree}, and explains the setup and
@; its timing results in detail.
@; 
@; @tt{Suffixtree} consists of six modules: @tt{data} to define label and
@; tree nodes, @tt{label} with functions on suffixtree node labels,
@; @tt{lcs} to compute longest common substrings, @tt{main} to apply
@; @tt{lcs} to @tt{data}, @tt{structs} to create and traverse suffix tree nodes,
@; @tt{ukkonen} to build suffix trees via Ukkonen's algorithm. Each
@; module is available with and without type annotations.  Each configuration
@; thus links six modules, some of them typed and others untyped.
@; 
@; @; @figure["fig:purpose-statements" "Suffixtree Modules"
@; @; @tabular[#:sep @hspace[2]
@; @; (list (list @bold{Module} @bold{Purpose})
@; @; (list @tt{data.rkt}    "Label and tree node data definitions")
@; @; (list @tt{label.rkt}   "Functions on suffixtree node labels")
@; @; (list @tt{lcs.rkt}     "Longest-Common-Subsequence implementation")
@; @; (list @tt{main.rkt}    "Apply lcs to benchmark data")
@; @; (list @tt{structs.rkt} "Create and traverse suffix tree nodes")
@; @; (list @tt{ukkonen.rkt} "Build whole suffix trees via Ukkonen's algorithm"))]]
@; 
@; 
@; @figure*["fig:suffixtree" 
@;           @list{Performance lattice (labels are speedup/slowdown factors)}
@;   @(let* ([vec (file->value SUFFIXTREE-DATA)]
@;           [vec* (vector-map (λ (p) (cons (mean p) (stddev p))) vec)])
@;      (make-performance-lattice vec*))
@; ]
@; 
@; Typed modules require type annotations on their data definitions and functions.
@; Modules provide their exports with types, so that the
@; type checker can cross-check modules. A typed module may import
@; values from an untyped module, which forces the
@; corresponding @racket[require] specifications to come with
@; types. Consider this example:
@; @;%
@; @(begin
@; #reader scribble/comment-reader
@; (racketblock
@; (require (only-in "label.rkt" make-label ...))
@; ))
@; @;%
@; The server module is called @tt{label.rkt}, and the client imports specific
@;  values, e.g., @tt{make-label}.  This specification is replaced with a
@;  @racket[require/typed] specification where each imported identifier is
@;  typed:
@; @;%
@; @(begin
@; #reader scribble/comment-reader
@; (racketblock
@; (require/typed "label.rkt" 
@;  [make-label
@;   (-> (U String (Vectorof (U Char Symbol))) Label)]
@;  ...)
@; ))
@; @; 
@; 
@; The types in a
@; @racket[require/typed] form are compiled into contracts for
@; the imported values. For example, if some
@; imported variable is declared to be a @tt{Char}, the check @racket[char?]
@; is performed as the value flows across the module boundary. Higher-order
@; types (functions, objects, or classes) become contracts that wrap
@; the imported value and which check future interactions of this
@; value with its context.
@; 
@; The performance costs of gradual typing thus consist of wrapper allocation
@; and run-time checks. Moreover, the compiler must assume that
@; any value could be wrapped, so it cannot generate direct field access code
@; as would be done in a statically typed language.
@; 
@; Since our evaluation setup calls for linking typed modules to both typed
@; and untyped server modules, depending on the configuration, we replace
@; @racket[require/typed] specifications with @racket[require/typed/check]
@; versions. This new syntax can determine whether the server module is typed
@; or untyped. It installs contracts if the server module
@; is untyped, and it ignores the annotation if the server module is typed.
@; As a result, typed modules function independently of the rest of the
@; modules in a configuration.
@; 
@; 
@; @; -----------------------------------------------------------------------------
@; @parag{Performance Lattice.}
@; 
@; @Figure-ref{fig:suffixtree} shows the performance lattice annotated with the
@;   timing measurements. The lattice displays each of the modules in the
@;   program with a shape.  A filled black shape means the module is typed, an
@;   open shape means the module is untyped. The shapes are ordered from left
@;   to right and correspond to the modules of @tt{suffixtree} in alphabetical
@;   order: @tt{data}, @tt{label}, @tt{lcs}, @tt{main}, @tt{structs}, and
@;   @tt{ukkonen}.
@; 
@;  For each configuration in the lattice, the ratio is
@;  computed by dividing the average timing of the typed program by
@;  the untyped average. The figure omits standard deviations
@;  as they are small enough to not affect the discussion.
@; 
@; The fully typed configuration (top) is @emph{faster} than the fully untyped
@;  (bottom) configuration by around 30%, which puts the typed/untyped ratio at 0.7. This can
@;  be explained by Typed Racket's optimizer, which performs specialization of
@;  arithmetic operations and field accesses, and can eliminate some
@;  bounds checks@~cite[thscff-pldi-2011]. When the optimizer is turned off,
@;  the ratio goes back up to 1. 
@; 
@; 
@; Sadly, the performance improvement of the typed configuration is the
@;  only good part of this benchmark. Almost all partially typed configurations
@;  exhibit slowdowns of up to 105x. Inspection of the lattice
@;  suggests several points about these slowdowns: @itemlist[
@; 
@; @item{Adding type annotations to the @tt{main} module neither subtracts nor
@;  adds overhead because it is a driver module.}
@; 
@; 
@; @item{Adding types to any of the workhorse modules---@tt{data}, @tt{label},
@;  or @tt{structs}---while leaving all other modules untyped causes slowdown of
@;  at least 35x. This group of modules are tightly coupled.
@;  Laying down a type-untyped boundary to separate
@;  elements of this group causes many crossings of values, with associated
@;  contract-checking cost.}
@; 
@; @item{Inspecting @tt{data} and @tt{label} further reveals that the latter
@;  depends on the former through an adaptor module. This adaptor introduces a
@;  contract boundary when either of the two modules is untyped. When both
@;  modules are typed but all others remain untyped, the slowdown is reduced
@;  to about 13x.
@; 
@;  The @tt{structs} module depends on @tt{data} in the same fashion and
@;  additionally on @tt{label}. Thus, the configuration in which both
@;  @tt{structs} and @tt{data} are typed still has a large slowdown. When all
@;  three modules are typed, the slowdown is reduced to 5x.}
@; 
@; @item{Finally, the configurations close to the worst slowdown case are
@;  those in which the @tt{data} module is left untyped but several of the
@;  other modules are typed. This makes sense given the coupling noted
@;  above; the contract boundaries induced between the untyped @tt{data} and
@;  other typed modules slow down the program.  The module structure diagram
@;  for @tt{suffixtree} in @figure-ref{fig:bm} corroborates the presence of
@;  this coupling. The rightmost node in that diagram corresponds to the
@;  @tt{data} module, which has the most in-edges in that particular
@;  graph. We observe a similar kind of coupling in the simpler @tt{sieve}
@;  example, which consists of just a data module and its client.}
@; ]
@; 
@; The performance lattice for @tt{suffixtree} is bad news for gradual typing.
@; It exhibits performance ``valleys'' in which a maintenance programmer can get stuck.
@; Consider starting with the untyped program, and for some reason choosing
@; to add types to @tt{label}. The program slows down by a factor of 88x. Without any
@; guidance, a developer may choose to then add types to @tt{structs} and see the
@; program slow down to 104x.  After that, typing @tt{main} (104x), @tt{ukkonen}
@; (99x), and @tt{lcs} (103x) do little to improve performance. It is only
@; when all the modules are typed that performance becomes acceptable again (0.7x).
@; 
@; 
@; @figure*["fig:lnm1"
@;   @list{@step["L" "N" "M"] results for the first six benchmarks}
@;   @(let* ([data `(("sieve"        ,SIEVE-DATA)
@;                   ("morse-code"   ,MORSECODE-DATA)
@;                   ("mbta"         ,MBTA-DATA)
@;                   ("zordoz"       ,ZORDOZ-DATA)
@;                   ("suffixtree"   ,SUFFIXTREE-DATA)
@;                   ("lnm"          ,LNM-DATA)
@;                   )])
@;      (data->pict data #:tag "1"))
@; ]
@; 
@; @figure*["fig:lnm2"
@;   @list{@step["L" "N" "M"] results for the remaining benchmarks}
@;   @(let* ([data `(("kcfa"       ,KCFA-DATA)
@;                   ("snake"      ,SNAKE-DATA)
@;                   ("tetris"     ,TETRIS-DATA)
@;                   ("synth"      ,SYNTH-DATA)
@;                   ("gregor"     ,GREGOR-DATA)
@;                   ("quad"       ,QUAD-DATA))])
@;      (data->pict data #:tag "2"))
@; ]
@; 
@; 
@; @; -----------------------------------------------------------------------------
@; @section{Reading the Figures}
@; 
@; Our method defines the number of @step["L" "N" "M"] configurations as the key metric for measuring the quality of a gradual type system.
@; For this experiment we have chosen values of 3x and 10x for @math{N} and @math{M}, respectively, and allow up to 2 additional type conversion steps.
@; These values are rather liberal,@note{We would expect that most production contexts would not tolerate anything higher than 2x, if that much.} but serve to ground our discussion.
@; 
@; The twelve rows of graphs in @Figure-ref["fig:lnm1" "fig:lnm2"] summarize the results from exhaustively exploring the performance lattices of our benchmarks.
@; Each row contains a table of summary statistics and one graph for each value of @math{L} between 0 and 2.
@; 
@; The typed/untyped ratio is the slowdown or speedup of fully typed code over untyped code.
@; Values smaller than @math{1.0} indicate a speedup due to Typed Racket optimizations.
@; Values larger than @math{1.0} are slowdowns caused by interaction with untyped libraries or untyped parts of the underlying Racket runtime.
@; The ratios range between 0.28x (@tt{lnm}) and 3.22x (@tt{zordoz}).
@; 
@; The maximum overhead is computed by finding the running time of the slowest configuration and dividing it by the running time of the untyped configuration.
@; The average overhead is obtained by computing the average over all configurations (excluding the fully-typed and untyped configurations) and dividing it by the running time of the untyped configuration.
@; Maximum overheads range from 1.25x (@tt{lnm}) to 168x (@tt{tetris}).
@; Average overheads range from 0.6x (@tt{lnm}) to 68x (@tt{tetris}).
@; 
@; The @deliverable{3} and @usable["3" "10"] counts are computed for @math{L=0}.
@; In parentheses, we express these counts as a percentage of all configurations for the program.
@; 
@; The three cumulative performance graphs are read as follows.
@; The x-axis represents the slowdown over the untyped program (from 1x to @id[PARAM-MAX-OVERHEAD]x).
@; The y-axis is a count of the number of configurations (from @math{0} to @math{2^n}) scaled so that all graphs are the same height.
@; If @math{L} is zero, the blue line represents the total number of configurations with performance no worse than the overhead on the x-axis.
@; For arbitrary @math{L}, the blue line gives the number of configurations that can reach a configuration with performance no worse than the overhead on the x-axis in at most @math{L} conversion steps.
@; 
@; The ideal result would be a flat line at a graph's top.
@; Such a result would mean that all configurations are as fast as (or faster than) the untyped one.
@; The worst scenario is a flat line at the graph's bottom, indicating that all configurations are more than 20x slower than the untyped one.
@; For ease of comparison between graphs, a dashed (@exact{\color{red}{red}}) horizontal line indicates the 60% point along each project's y-axis.
@; 
@; 
@; @; -----------------------------------------------------------------------------
@; @section[#:tag "sec:all-results"]{Interpretation}
@; 
@; The ideal shape is difficult to achieve because of the overwhelming cost of the
@; dynamic checks inserted at the boundaries between typed and untyped code.
@; The next-best shape is a nearly-vertical line that reaches the top at a low x-value.
@; All else being equal, a steep slope anywhere on the graph is desirable because
@; the number of acceptable programs quickly increases at that point.
@; 
@; For each benchmark, we evaluate the actual graphs against these expectations.
@; Our approach is to focus on the left column, where @math{L}=0, and to consider the
@; center and right column as rather drastic countermeasures to recover
@; performance.@note{Increasing @math{L} should remove pathologically-bad cases.} 
@; 
@; @parag{Sieve}
@; The flat line at @math{L}=0 shows that half of all configurations suffer
@; unacceptable overhead. As there are only 4 configurations in the lattice
@; for @tt{sieve}, increasing @math{L} improves performance.
@; 
@; @parag{Morse code}
@; The steep lines show that a few configurations suffer modest overhead (below 2x),
@; otherwise @tt{morse-code} performs well.
@; Increasing @math{L} improves the worst cases.
@; 
@; @parag{MBTA}
@; These lines are also steep, but flatten briefly at 2x.
@; This coincides with the performance of the fully-typed
@; configuration.
@; As one would expect, freedom to type additional modules adds configurations
@; to the @deliverable{2} equivalence class.
@; 
@; @parag{Zordoz}
@; Plots here are similar to @tt{mbta}.
@; There is a gap between the performance of the fully-typed
@; configuration and the performance of the next-fastest lattice point.
@; 
@; @parag{Suffixtree}
@; The wide horizontal areas are explained by the performance lattice in
@; @figure-ref{fig:suffixtree}: configurations' running times are not evenly
@; distributed but instead vary drastically when certain boundaries exist.
@; Increasing @math{L} significantly improves the number of acceptable configuration
@; at 10x and even 3x overhead.
@; 
@; @parag{LNM}
@; These results are ideal.
@; Note the large y-intercept at @math{L}=0.
@; This shows that very few configurations suffer any overhead.
@; 
@; @parag{KCFA}
@; The most distinctive feature at @math{L}=0 is the flat portion between 1x
@; and 6x. This characteristic remains at @math{L}=1, and overall performance
@; is very good at @math{L}=2.
@; 
@; @parag{Snake}
@; The slope at @math{L}=0 is very low.
@; Allowing @math{L}=1 brings a noticeable improvement above the 5x mark,
@; but the difference between @math{L}=1 and @math{L}=2 is small.
@; 
@; @parag{Tetris}
@; Each @tt{tetris} plot is essentially a flat line.
@; At @math{L}=0 roughly 1/3 of configurations lie below the line.
@; This improves to 2/3 at @math{L}=1 and only a few configurations suffer overhead
@; when @math{L}=2.
@; 
@; @parag{Synth}
@; Each slope is very low.
@; Furthermore, some configurations remain unusable even at @math{L}=2.
@; These plots have few flat areas, which implies that overheads are spread
@; evenly throughout possible boundaries in the program.
@; 
@; @parag{Gregor}
@; These steep curves are impressive given that @tt{gregor} has 13 modules.
@; Increasing @math{L} brings consistent improvements.
@; 
@; @parag{Quad}
@; The @tt{quad} plots follow the same pattern as @tt{mbta} and @tt{zordoz}, despite being visually distinct.
@; In all three cases, there is a flat slope for overheads below the typed/untyped ratio and a steep increase just after.
@; The high typed/untyped ratio is explained by small differences in the original author-supplied variants.
