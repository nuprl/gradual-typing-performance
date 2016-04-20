#lang scribble/base

@require[
  "common.rkt"
  "typed-racket.rkt"
  (except-in gtp-summarize/lnm-parameters defparam)
]

@title[#:tag "sec:tr"]{Evaluating Typed Racket}

To validate our framework, we apply it to a suite of
 @id[(count-benchmarks)] Typed Racket programs.
For each program we have collected running times over a full performance lattice.
In general the lattices are too large to print or analyze, so we present
 our results using a graphical shorthand quantifying the number of deliverable
 and usable configurations.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:bm"]{The Benchmark Programs}

The benchmarks themselves are representative of actual user code yet
 small enough that exhaustive performance evaluation remains tractable.
The following descriptions briefly explain the purpose and history of
 each benchmark.
Most benchmarks are self-contained, but where relevant we note their external
 dependencies.


@; -----------------------------------------------------------------------------
@subsection{Benchmark Descriptions}

@render-benchmark-descriptions[
@(benchmark
  #:name 'sieve
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin "Synthetic"
  #:purpose "Generate prime numbers"
  @elem{
    Demonstrates a scenario where user
     code closely interacts with higher-order library code.
    In this case, the library implements a stream data structure.
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
    The morse code benchmark is derived from a training program that
     converts a random word to morse code, gives the codeword to the user,
     accepts keyboard input, then prints the Levenshtein distance of the
     input from the original word.
    For our benchmark we remove the I/O and random features but otherwise
     compute morse code and distances for a fixed sequence of word pairs.
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
    Consequently, our benchmark is slightly modified
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
  #:purpose "Graphing"
  #:external-libraries (list @hyperlink["https://docs.racket-lang.org/plot/"]{plot}
                             ", "
                             @hyperlink["https://docs.racket-lang.org/math/stats.html"]{math/statistics})

  @elem{
    While writing this paper, we built a small library of scripts to analyze
     and graph the data shown in @Secref{sec:lnm-plot}.
    The @bm{lnm} benchmark creates one such graph for a 13-module benchmark.
    Most of the computation time is spent in calls to Racket's
     typed statistics and plotting libraries, so performance typically
     improves as more @bm{lnm} modules are typed.
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
     that computes @exact|{~$\RktMeta{2*(1+3) = 2*1 + 2*3}$}|.
  }
)
@(benchmark
  #:name 'zombie
  #:author "David Van Horn"
  #:num-adaptor 1
  #:origin @hyperlink["https://github.com/philnguyen/soft-contract"]{Educational}
  #:purpose "Game"

  @elem{
    A game where players must keep their marker away from
     computer-controlled "zombie" markers.
    We benchmark the game on a pre-defined sequence of commands and remove the
     I/O features.

    The original program was implemented in an object-oriented style but converted
     to a functional encoding as a test case for soft contract verification@~cite[nthvh-icfp-2014].
    We benchmark a typed version of the functional game.
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
     self-contained program; however,
    @; TODO if the limitation doesn't belong here, where should it go?
     we monomorphized the core array data structure
     because opaque, polymorphic structures may not be sent across
     type boundaries in Typed Racket.
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
     functions because those functions rely on an untyped library for
     ad-hoc polymorphism that is not yet supported by Typed Racket.
  }
)
@(benchmark
  #:name 'dungeon
  #:author "Vincent St. Amour"
  #:num-adaptor 0
  #:origin "Game"
  #:purpose "Maze generator"
  @elem{
    Builds a grid of wall and floor objects by selecting first-class classes
     from a map of ``template'' pieces.
    @todo{keep math/array?}
    @todo{keep dict?}
  }
)
@(benchmark
  #:name 'take5
  #:author "Matthias Felleisen"
  #:num-adaptor 1
  #:origin "Game"
  #:purpose "Card game"
  @elem{
    Object-oriented implementation of a classic German card game.
    The AI players we use implement a greedy strategy, playing locally optimal
     cards in each turn.
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
    The interpreter maintains an environment of first-class objects representing
     calculator commands.
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
    Converts S-expression source code to @tt{.pdf} format.
    We have two versions of @bm{quad}:
     the first, @tt{quadMB}, uses fully-untyped and fully-typed configurations
     provided by the original author.
    This version has a high typed/untyped ratio because it uses the type system
     to enforce more datatype invariants than the untyped program.
    In other words, the typed version is slower because it does more work.
    Our second version, @tt{quadBG}, uses types as weak as the untyped
     program and is therefore suitable for judging the implementation
     of Typed Racket rather than the user experience of Typed Racket.
    The conference version of this paper gave data only for
     @tt{quadMB}@~cite[tfgnvf-popl-2016].

    To give a concrete example of different types, here are the definitions
     for the core @tt{Quad} datatype from both @tt{quadMB} and @tt{quadBG}.

    @racket[(define-type QuadMB (Pairof Symbol (Listof QuadMB)))]

    @racket[(define-type QuadBG (Pairof Symbol (Listof Any)))]

    The former is a homogenous, recursive type.
    As such, the predicate asserting that an untyped value has type @racket[QuadMB]
     is a linear-time tree traversal.
    On the other hand, the predicate for @racket[QuadBG] is significantly faster.
  }
)
]


@; -----------------------------------------------------------------------------
@subsection{Static Benchmark Characteristics}

@;

@Figure-ref{fig:bm} gives static characteristics
 of our benchmark programs as a coarse measure of their size and diversity.
Program size is measured by the lines of code (LOC) and number of modules.
Of these two measures, the number of modules is a slightly better indicator
 as it also determines the size of our gradual typing experiment:
 given @exact{$N$} modules, there are @exact{$2^N$} configurations.
Adaptor modules (discussed in @Secref{sec:adaptor}) roughly correspond
 to the number of user-defined datatypes in each benchmark.
Regarding lines of code, the ``Annotation'' column is an
 upper bound on the number of type annotations needed to fully type each program.
This column is an over-approximation because it includes type annotatons for
 each import in a benchmark; in practice,
 only imports from untyped modules into typed modules need annotations.
Lastly, the ``Boundaries'' and ``Exports'' columns describe the graph
 structure of each benchmark.
Boundaries are import statements from one module in the benchmark to another.
This count does not include external boundaries.
The exports count the total number of unique identifiers that cross any
 of a benchmark's boundaries.

@figure*["fig:bm" "Static characteristics of the benchmarks"
  @render-benchmarks-table{}
]


@; -----------------------------------------------------------------------------
@section[#:tag "sec:protocol"]{Experimental Protocol}

Our experiment measured the running time of all
 configurations in each benchmark's performance lattice.
We performed the same experiment on @integer->word[(length (*RKT-VERSIONS*))]
 versions of Racket: version 6.2,
 version 6.3, and a development build of version 6.4.
@; {In particular,
@;  commit @hyperlink["https://github.com/racket/racket/commit/86a9c2e493d2b6ad70b3a80fef32a9e810c4e2db"]{86a9c2e4} from January 26, 2016.}
The machine we used to take measurements was a Linux machine with
 physical two AMD Opteron 6376 2.3GHz processors and 128GB RAM.
Each processor has 16 cores, giving us a total of 32.
We dedicated at most 29 of the machine's cores to running our experiment;
 each configuration was pinned to a single core and each benchmark program
 was run to completion before starting the next benchmark.

Timing information for a single configuration was obtained by compiling the
 code ahead of time and then running the configuration's main module repeatedly.
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
 from heuristics in the Racket JIT compiler@~cite[kff-hosc-2013] to machine-level caching and
 environment variable layout@~cite[cb-asplos-2013 kj-ismm-2013]
We hope to mitigate these confounding effects by running our experiments on
 a single core and taking the average of repeated runs.
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
 especially since we have observed similar differences on other machines@~cite[tfgnvf-popl-2016].

Running even 30 iterations, however, is prohibitive given the size of our experiment.
In total, we measured @add-commas[(count-all-configurations)] configurations
 under each version of Racket.
To finish the experiment in a timely manner, we applied the Anderson-Darling
 normality test@~cite[ad-asa-1954] after taking 10 measurements with a critical value
 determined experimentally by Stephens@~cite[s-asa-1974].
The judgment we made was about the likelihood of seeing a particular sequence
 of 10 runtimes assuming the data were from a normal distribution.
If the odds were less than @math{1%}, we ran an additional 20 iterations.
@(let* ([s+t (count-savings)]
        [num-skip-runs (car s+t)]
        [num-runs (cadr s+t)])
  @elem{
    This led us to skip @add-commas[num-skip-runs] runs in total
     (@id[(round (* 100 (/ (- num-runs num-skip-runs) num-runs)))]% of all runs)
     and led to no statistically
     significant differences in benchmarks that we tested exhaustively.
  })

In order to explain our methodology precisely, we now summarize the key points from
 Stephens@~cite[s-asa-1974] regarding the Anderson-Darling test.
Our underlying distribution @math{F} is the distribution of runtimes obtained
 for one configuration run repeatedly on a single core.
We assume that @math{F} is normally distributed with an unknown mean
 and variance.
Let @exact|{$\vec{x}$}| denote our vector of 10 runtimes, sorted in increasing
 order.
We approximate the true mean @exact|{$\mu$}| and variance @exact|{$\sigma^2$}|
 of @math{F} by the sample mean and variance:

   @exact|{$$
     \myhat{\mu} = \Sigma_{i=0}^{9} \vec{x}_i~/ 10
     \hspace{2cm}
     \myhat{\sigma}^2 = \Sigma_{i=0}^{9} (x_i - \myhat{\mu})^2~/ 9
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
 the true @exact|{$\mu$}| and @exact|{$\sigma^2$}| are unknown@~cite[s-asa-1974].

    @exact|{$$ A^{2}_{+} = A^2 * (1 + \frac{4}{n} - \frac{25}{n^2}) $$}|

Finally, we declare the samples non-normal if @exact|{$A^{2}_{+}$}| is greater than 1.
The value 1 was determined experimentally by Stephens for a @math{p}-value of
 @math{1%} given 10 samples and an unknown underlying mean and variance.


@; -----------------------------------------------------------------------------
@section[]{Results}

@(render-lnm-plot
  (lambda (pict*)
    (define name*
      (for/list ([p (in-list pict*)]
                 [i (in-naturals)])
        (format "fig:lnm:~a" i)))
    (define get-caption
      (let ([N (length name*)])
        (lambda (i) (format "Performance Graphs (~a/~a)" i N))))
    (define NUMV (integer->word (length (*RKT-VERSIONS*))))
    (cons
      @elem{
        @; -- Quickly, just the basics
        @(apply Figure-ref name*) present our experimental results in
         a series of performance graphs.
        Each graph is a cumulative distribution function showing the number
         of @deliverable{D} configurations for real-valued @math{D} between
         1 and @id[(*MAX-OVERHEAD*)].
        Specifically, the x-axes represent overhead factors
         relative to the untyped configuration of each benchmark.
        The y-axes count the percentage of each benchmark's configurations
         that run within the overhead shown on the x-axes.
        On each plot we give @id[NUMV]
         lines corresponding to the @id[NUMV]
         versions of Racket we tested; finally, figures are partitioned
         across two columns to compare the number of @step["0" "D" "U"]
         configurations against the number @step["1" "D" "U"] configurations.

        @; -- choice of x-axis, log scale, bode diagrams, picking D/U
        The range of values on the x-axes were chosen as plausible bounds
         for the overhead users of gradual type systems are willing to
         accept.
        Granted, there may be software teams that require overhead
         under 1x---that is, a speedup relative to the untyped program---or
         can work with slowdowns exceeding 20x, but we expect most users
         will tolerate a small performance overhead.
        As such we use a log scale on the x-axis to emphasize the practical
         value of low overheads.
        Minor tick lines are drawn at 1.2x, 1.4x, etc and again at 4x, 6x, etc.
         for ease of reference.
        For example, the number of @deliverable{1.2} configurations can be found
         by studying the first minor tick and the number of @usable["1.2" "1.4"]
         configurations
         by subtracting the number of @deliverable{1.2} configurations from
         the number of configurations deliverable at the second minor tick.

        @; -- choice of y-axis
        To encourage comparisons across benchmarks, the y-axes show
         the percentage of deliverable configurations rather than an absolute count.
        For readers interested in the number of configurations a given percentage
         represents, we list the total configurations in each benchmark
         along the right column of the figures.

        @; -- data lines
        A data point @math{(X,Y)} along any of the @id[NUMV] curves in a plot
         along the left column
         represents the percentage @math{Y} of configurations
         that run at most @math{X} times slower than the benchmark's
         untyped configuration.
        Taking @bm{sieve} as an example, 50% of configurations run at most
         2x slower than the untype configuration.
        On Racket versions 6.2 and 6.3, the same 50% of configurations
         are all that run within a 20x overhead.
        The situation is improved in Racket 6.4, where 75% of configurations
         exhibit less than 20x overhead.

        The right column of plots shows the effect of adding types
         to @math{k=1} additional untyped modules, chosen angelically.
        Again using @bm{sieve} as the example, 100% of configurations can
         reach a configuration with at most 1x overhead after at most one
         type conversion step.
        This is because the fully-typed configuration happens to be
         @deliverable{1} and both of the gradually typed configurations
         become fully-typed after one conversion step.
        As a larger example, consider the plots for @bm{mbta}.
        Freedom to type one extra module has no effect on the number of
         @deliverable{1.2} configurations.
        In other words, even if the programmer at a @deliverable{1.2} configuration
         happens to convert the untyped module best-suited to improve performance,
         their next configuration will be no better than @deliverable{1.2}.
        @; Add teaser for Sec 6?

        @; -- all about data, ideal shape
        Ideally, every curve in the left column would be a flat line at the
         top of the plot, meaning that all configurations on all tested versions
         of Racket run no slower than each benchmark's untyped configuration.
        If this were true, the right column would be identical to the left
         because every @deliverable{1} configuration can reach a @deliverable{1}
         configuration (itself) in at most one type conversion step.
        Conversely, the worst scenario would have flat lines at the bottom of every
         plot, indicating that any configuration with at least one typed module
         is more than 20x slower than the untyped configuration.
        Here too, freedom to type an additional module will not change performance.

        The reality is that each benchmark determines a unique curve.
        Using a different version of Racket or moving from @math{k=0} to @math{k=1}
         shifts the curve horizontally, but does not change the overall shape
         i.e. the relative cost of type boundaries in a program.
        Keep in mind that a steep slope implies a good tradeoff between
         accepting a larger overhead and increasing the number of
         deliverable configurations.
        A shallow slope or flat line is a poor tradeoff and evidence that
         the majority of configurations suffer very large performance overhead.
        Where large overheads are due to a pathological boundary between two
         tightly-coupled modules, the associated @math{k=1} graph should
         exhibit much better performance, as is the case for @bm{sieve}.
      }
      (for/list ([p (in-list pict*)]
                 [name (in-list name*)]
                 [i (in-naturals 1)])
        (figure name (get-caption i) p)))))


@; -----------------------------------------------------------------------------
@subsection{Aggregate Statistics}

@(let*-values (((name* ratio** average** max**) (apply values (get-lnm-table-data)))
               ((min/max)
                (lambda (x*)
                  (for/fold ([lo #f] [hi #f])
                            ([x (in-list x*)])
                    (values (if lo (min lo x) x) (if hi (max hi x) x)))))
               ((val->name)
                (lambda (x** v)
                  (for/or ([name (in-list name*)]
                           [x*   (in-list x**)]
                           #:when (member v x*))
                    (bm name))))
               ((max-at?) (lambda (x* i) (= (apply max x*) (list-ref x* i))))
               ((num-improved) (for/sum ([avg* (in-list average**)]
                                         [max* (in-list max**)])
                                 (if (and (max-at? avg* 2) (max-at? max* 2))
                                   1 0)))
               ((min-ratio max-ratio) (min/max (apply append ratio**)))
               ((min-ratio-name max-ratio-name) (values (val->name ratio** min-ratio)
                                                        (val->name ratio** max-ratio)))
               ((min-average max-average)   (min/max (apply append average**)))
               ((min-average-name max-average-name) (values (val->name average** min-average)
                                                      (val->name average** max-average)))
               ((min-max max-max)     (min/max (apply append max**)))
               ((min-max-name max-max-name) (values (val->name max** min-max)
                                                    (val->name max** max-max))))
  @elem{
    The bar charts in @Figure-ref{fig:lnm-table} give the typed/untyped ratio,
     average overhead, and maximum overhead for our benchmarks.
    The x-axis of each chart spans our @id[(count-benchmarks)] benchmark programs;
     a key mapping lowercase letters to benchmark names is at the bottom of
     @Figure-ref{fig:lnm-table}.
    Each benchmark on each chart is represented with
     @integer->word[(length (*RKT-VERSIONS*))] bars.
    From left to right, these bars represent data for Racket v6.2, v6.3, and v6.4.
    All charts are plotted on a log scale y-axis to accomodate and acknowledge
     the wide variations across benchmarks.

    The typed/untyped ratio is the slowdown or speedup of the fully-typed configuration
     relative to the untyped configuration.
    Values smaller than @math{1.0} indicate a speedup due to Typed Racket optimizations.
    Values larger than @math{1.0} are slowdowns caused by interaction with untyped
     libraries or untyped parts of the underlying Racket runtime.
    The ratios range between @id[(rnd min-ratio)] (@elem[min-ratio-name])
     and @id[(rnd max-ratio)] (@elem[max-ratio-name]).
    On the y-axis, we draw ticks at even-numbered intervals after the
     previous major tick.

    Average and maximum overheads are computed over the entire performance
     lattice.
    The average is the arithmetic mean of each configuration's running time
     and ranges between @id[(add-commas (rnd min-average))]x (@elem[min-average-name])
     and @id[(add-commas (rnd max-average))]x (@elem[max-average-name]).
    The max is simply the overhead of the slowest-running configuration and
     ranges from @id[(add-commas (rnd min-max))]x (@elem[min-max-name])
     to @id[(add-commas (rnd max-max))]x (@elem[max-max-name]) across benchmarks.
    Neither of these statistics are very helpful for language users, but designers
     of gradually typed languages can use them to measure the effect of performance
     optimizations.
    Case in point, the figure shows that Typed Racket has improved
     the average-case and worst-case overhead in
     @id[num-improved] benchmarks.
    The performance regressions are mainly due to a bugfix related to the
     implementation of class contracts (see @Secref{sec:threats} for details).
  }
)

@figure*["fig:lnm-table" "Coarse-Grained Performance Statistics"
  @(render-lnm-table)
]


