#lang scribble/base

@require[
  "common.rkt"
  "typed-racket.rkt"
  (except-in gtp-summarize/lnm-parameters defparam)
]

@profile-point{sec:tr}
@title[#:tag "sec:tr"]{Evaluating Typed Racket}

To validate our evaluation framework, we apply it to a suite of
 @id[(count-benchmarks)] Typed Racket programs.
For each program we have collected running times over a full performance lattice.
In general the lattices are too large to print or analyze, so we present
 our results using so-called @emph{overhead graphs} to count the number of
 deliverable and usable configurations.


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

@profile-point{sec:tr:descriptions}
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
     reads keyboard input, then prints the Levenshtein distance of the
     input from the original word.
    For our benchmark we remove the I/O and random features but otherwise
     compute morse code translations and Levenshtein distances for a fixed
     sequence of word pairs.
  }
)
@(benchmark
  #:name 'mbta
  #:author "Matthias Felleisen"
  #:num-adaptor 0
  #:origin "Educational"
  #:purpose "Interactive map"
  #:external-libraries (list @hyperlink["http://github.com/stchang/graph"]{@library{graph}})

  @elem{
    Builds a map of Boston's subway system and
     answers a series of reachability queries.
    The map is represented as an object and encapsulates a boundary to Racket's untyped
     @library{graph} library; when the map is typed, the boundary to @library{graph}
     causes noticable overhead.
    Although the original program ran an asynchronous client/server framework,
     our benchmark is single-threaded to cooperate with Racket's sampling
     profiler.
  }
)
@(benchmark
  #:name 'zordoz
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin @hyperlink["http://github.com/bennn/zordoz"]{Library}
  #:purpose "Explore Racket bytecode"
  #:external-libraries (list @hyperlink["http://docs.racket-lang.org/raco/decompile.html#%28mod-path._compiler%2Fdecompile%29"]{@library{compiler-lib}})

  @elem{
    Provides a shell-style interface for traversing
     Racket bytecode (@tt{.zo} files).
    Our benchmark decompiles its own bytecode and
     counts the number of branch instructions in the resulting tree structure.

    The Racket bytecode format changed between versions 6.2 and 6.3 with
     the release of the set-of-scopes macro expander@~cite[f-popl-2016]
     and again between versions 6.4 and 6.5.
    Consequently, our benchmark is slightly modified after versions 6.2 and 6.4
     to accomodate the new formats.
    @todo{how does this affect results?}
    @;As it turns out, the change from 6.2 to 6.3 improved the typed/untyped ratio
    @; from
    @; @add-commas[(rnd (typed/untyped-ratio 'zordoz "6.2"))]x in v6.2 to
    @; @add-commas[(rnd (typed/untyped-ratio 'zordoz "6.3"))]x in v6.3 because
    @; the more recent bytecode structures generate less expensive type contracts.
    @;The ratio for the newest bytecode format is
    @; @add-commas[(rnd (typed/untyped-ratio 'zordoz "6.5"))]x.
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
    The benchmark compares lines of text; each line is no
     more than 80 characters long.

    All @bm{suffixtree} datatype definitions are in a single module, separate
     from the functions that manipulate and traverse the data.
    The data are also mutable, therefore the frequently-crossed type boundary
     between data definitions and their core functionality is protected by expensive
     contracts.
  }
)
@(benchmark
  #:name 'lnm
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin "Synthetic"
  #:purpose "Graphing"
  #:external-libraries (list @hyperlink["https://docs.racket-lang.org/plot/"]{@library{plot}}
                             ", "
                             @hyperlink["https://docs.racket-lang.org/math/stats.html"]{math/statistics})

  @elem{
    While writing this paper, we built a small library of scripts to analyze
     and graph the data shown in @Secref{sec:plots}.
    The @bm{lnm} benchmark creates one such graph for the @bm{gregor} benchmark.
    Most of the computation time is spent in calls to Racket's
     typed statistics and plotting libraries, so performance
     greatly improves as more @bm{lnm} modules are typed.
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
    The performance overhead in this benchmark comes primarily from contracts
     protecting the binding environment, which is implemented as a hashtable
     and threaded across the program.
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
    We benchmark a typed version of the functional game on a small input
     (100 lines) because repeatedly sending encoded objects across
     a type boundary leads to exponential slowdowns.

    @; --- 2016-04-22 : this type is a little too awkward to talk about
    @;As an example of the encoding, the following type signature implements a
    @; point object.
    @;By supplying a symbol, clients get access to a (tagged) method.
    @;
    @;@codeblock{
    @;  (define-type Point
    @;    ((U 'x 'y 'move)
    @;     ->
    @;     (U (Pairof 'x (-> Real))
    @;        (Pairof 'y (-> Real))
    @;        (Pairof 'move (Real Real -> Point)))))
    @;}
    @;Intersection
    @; types would be more straightforward than the tagged codomains used here,
    @; but Typed Racket does not yet support intersections.

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
     adapted from work on soft contract verification @|etal|@~cite[nthvh-icfp-2014].
    Most of the overhead in @bm{tetris}, and also @bm{snake}, is due
     to repeatedly passing the game state and subsidiary data structures
     across type boundaries.
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
     to Typed Racket's @hyperlink["https://docs.racket-lang.org/math/array.html"]{@library{math/array}}
     library@~cite[saf-cc-2015].
    Our benchmark incorporates the relevant library modules to form a
     self-contained program; however,
     we monomorphized the core array data structure because Typed Racket v6.2
     could not convert its type to a contract.
  }
)
@(benchmark
  #:name 'gregor
  #:author "Jon Zeppieri"
  #:num-adaptor 2
  #:origin @hyperlink["https://docs.racket-lang.org/gregor/index.html"]{Library}
  #:purpose "Date and time library"
  #:external-libraries
    (list @hyperlink["https://docs.racket-lang.org/cldr-core/index.html"]{@library{cldr}}
          ", "
          @hyperlink["https://docs.racket-lang.org/tzinfo/index.html"]{@library{tzinfo}})

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
    Originally, the program used two external libraries: the math library
     for array operations and @library{racket/dict} for a generic dictionary interface.
    We removed both these dependencies.
    Replacing the array library with vectors was necessary because Typed Racket
     v6.2 could not compile the type @racket[(Mutable-Array (Class))] to a contract.
    Replacing the dict interface was a choice made so that the results for
     @bm{dungeon} describe internal type boundaries rather than the type
     boundary to the untyped dict interface.
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
    Much of the functionality is encapsulated in objects that seldom
     communicate, so gradual typing imposes fairly low overhead.
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
    The interpreter maintains a dynamically-extensible list of first-class
     objects representing calculator commands.
    If this list repeatedly crosses type boundaries it accumulates
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
    Overhead is low because only small, first-order values cross type
     boundaries.
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
  #:external-libraries (list @hyperlink["https://github.com/mbutterick/csp"]{@library{csp}})

  @elem{
    Converts S-expression source code to @tt{.pdf} format.
    We have two versions of @bm{quad}:
     the first, @tt{quadMB}, uses fully-untyped and fully-typed configurations
     provided by the original author.
    This version has a high typed/untyped ratio
     (@add-commas[(rnd (typed/untyped-ratio 'quadMB "6.2"))]x in v6.2)
     because it explicitly compiles types to runtime predicates
     and uses these predicates to eagerly check data invariants.
    In other words, the typed version is slower because it does more work.
    Our second version, @tt{quadBG}, uses the same code but weakens
     types to match the untyped
     program and is therefore suitable for judging the implementation
     of Typed Racket rather than the user experience of Typed Racket.
    The conference version of this paper gave data only for
     @tt{quadMB}@~cite[tfgnvf-popl-2016].

    To give a concrete example of different types, here are the definitions
     for the core @tt{Quad} datatype from both @tt{quadMB} and @tt{quadBG}.

    @racket[(define-type QuadMB (Pairof Symbol (Listof QuadMB)))]

    @racket[(define-type QuadBG (Pairof Symbol (Listof Any)))]

    The former is a homogenous, recursive type.
    As such, the predicate asserting that a value has type @racket[QuadMB]
     is a linear-time tree traversal, whether or not the value is statically typed.
    On the other hand, the predicate for @racket[QuadBG] is significantly faster.
  }
)
]


@; -----------------------------------------------------------------------------
@profile-point{sec:tr:characteristics}
@subsection{Static Benchmark Characteristics}

@Figure-ref{fig:bm} gives static characteristics
 of our benchmark programs as a coarse measure of their size and diversity.
Program size is measured by the lines of code (LOC) and number of modules.
Of these two measures, the number of modules is a slightly better indicator
 as it also determines the size of our gradual typing experiment;
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
This count omits external boundaries.
The Exports are the total number of unique identifiers that cross any
 of a benchmark's boundaries.

@figure*["fig:bm" "Static characteristics of the benchmarks"
  @render-benchmarks-table{}
]


@; -----------------------------------------------------------------------------
@profile-point{sec:tr:protocol}
@section[#:tag "sec:protocol"]{Experimental Protocol}

@todo{update with cluster stats/protocol}

Our experiment measured the running time of all
 configurations in each benchmark's performance lattice.
We performed the same experiment on @integer->word[(length (*RKT-VERSIONS*))]
 versions of Racket: version 6.2,
 version 6.3, and a development build of version 6.4.
@; {In particular,
@;  commit @hyperlink["https://github.com/racket/racket/commit/86a9c2e493d2b6ad70b3a80fef32a9e810c4e2db"]{86a9c2e4} from January 26, 2016.}
The machine we used to take measurements was a Linux machine with
 two physical AMD Opteron 6376 2.3GHz processors and 128GB RAM.
Each processor has 16 cores, giving us a total of 32 cores.
We dedicated at most 29 of the machine's cores to running our experiment;
 each configuration was pinned to a single core and each benchmark program
 was run to completion before starting the next benchmark.

Timing information for a single configuration was obtained by compiling the
 code ahead of time and then running the configuration's main module repeatedly.
Each run used a fresh instance of the Racket VM with the JIT compiler
 enabled.
@todo{how many iterations?}
All scripts we used to run our experiments and the data we collected
 are available in the online supplement to this paper.
For threats to validity regarding our experimental protocol,
 see @Secref{sec:threats:protocol}.


@; -----------------------------------------------------------------------------
@profile-point{sec:tr:plots}
@section[#:tag "sec:plots"]{Results}

@(render-lnm-plot
  (lambda (pict*)
    (define name*
      (for/list ([p (in-list pict*)]
                 [i (in-naturals)])
        (format "fig:lnm:~a" i)))
    (define get-caption
      (let ([N (length name*)])
        (lambda (i) (format "Overhead Graphs (~a/~a)" i N))))
    (define NUMV (integer->word (length (*RKT-VERSIONS*))))
    (cons
      @elem{
        @; -- Quickly, just the basics
        @(apply Figure-ref name*) present our experimental results in
         a series of overhead graphs.
        Each graph is a cumulative distribution function showing the number
         of @deliverable{D} configurations for real-valued @math{D}
         between 1x and @id[(*MAX-OVERHEAD*)]x.
        Specifically, the x-axes represent overhead factors
         relative to the untyped configuration of each benchmark.
        The y-axes count the percentage of each benchmark's configurations
         that run within the overhead shown on the x-axes.
        On each plot we give @id[NUMV]
         lines corresponding to the @id[NUMV]
         versions of Racket we tested; newer versions have thicker lines.
        Plots in the left column of each figure show @step["0" "D" "U"]
         configurations and plots in the right column show @step["1" "D" "U"] configurations.

        @; -- choice of x-axis, log scale, bode diagrams, picking D/U
        The range of values on the x-axes are plausible bounds on the
         overhead users of gradual type systems will accept.
        Granted, there may be software teams that require overhead
         under 1x---that is, a speedup relative to the untyped program---or
         can work with slowdowns exceeding 20x, but we expect most users
         will tolerate only a small performance overhead.
        To emphasize the practical value of low overheads
         we use a log scale on the x-axis with minor tick lines at
         1.2x, 1.4x, etc. and again at 4x, 6x, etc.
        For example, the y-value at the first minor tick gives the number
         of @deliverable{1.2} configurations.
        To derive the number of @usable["1.2" "1.4"] configurations,
         subtract the number of @deliverable{1.2} configurations from
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
         2x slower than the untyped configuration.
        On all Racket versions, the same 50% of configurations
         are the only ones that run within a 20x overhead.
        @;bg; NEW DATA

        The right column of plots shows the effect of adding types
         to at most @math{k=1} additional untyped modules.
        A point @math{(X,Y)} on these curves represents the percentage @math{Y}
         of configurations @exact{$c_1$} such that there exists a configuration
         @exact{$c_2$} where @exact{$c_1 \rightarrow_1 c_2$} and @exact{$c_2$}
         runs at most @math{X} times slower than the untyped configuration.
        Note that @exact{$c_1$} and @exact{$c_2$} may be the same;
         they are certainly the same when @exact{$c_1$} is the fully-typed configuration.
        Again using @bm{sieve} as an example, 100% of configurations can
         reach a configuration with at most 1x overhead after at most one
         type conversion step.
        This is because the fully-typed configuration happens to be
         @deliverable{1} and both of the gradually typed configurations
         become fully-typed after one conversion step.
        For a larger example, consider the plots for @bm{mbta}.
        Freedom to type one extra module has no effect on the number of
         @deliverable{1.2} configurations.
        In other words, even if the programmer at a @deliverable{1.2} configuration
         happens to convert the untyped module best-suited to improve performance,
         their next configuration will be no better than @deliverable{1.2}.
        @; Theoretically, a developer might try every way of typing the next
        @;  one module, but at that point they have all the annotations to go
        @;  anywhere in the lattice.

        @; -- all about data, ideal shape
        The ideal gradual type system would introduce zero overhead, thus every
         curve in the left column would be a flat line at the
         top of the plot, meaning that all configurations on all tested versions
         of Racket run no slower than each benchmark's untyped configuration.
        If this were true, the right column would be identical to the left
         because every @deliverable{1} configuration can reach a @deliverable{1}
         configuration (itself) in at most one type conversion step.
        Conversely, a worst-case gradual type system would exhibit flat lines
         at the bottom of every plot, indicating that any configuration with at
         least one typed module is more than 20x slower than the untyped configuration.
        Here too, freedom to type an additional module will not change performance
         and so the @math{k=0} and @math{k=1} plots will be identical.

        The reality is that each benchmark determines a unique curve.
        Using a different version of Racket or moving from @math{k=0} to @math{k=1}
         typically shifts the curve horizontally but does not change the overall
         shape i.e. the relative cost of type boundaries in a program.
        Regarding shapes, a steep slope implies a good tradeoff between
         accepting a larger overhead and increasing the number of
         deliverable configurations.
        A shallow slope or low, flat line is a poor tradeoff and implies
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
@subsection[#:tag "subsec:compare"]{Discussion}

@todo{prose, once we have reliable data}

Lessons from graphs:
@itemize[
  @item{
    Serious overheads, even on newest version.
    For those interested, the 5 benchmarks with the WORST overheads are @todo{list}.
    These pathologies are bad.

    @(let ([num-bad (length '(sieve forth fsm fsmoo zombie suffixtree tetris synth quadMB))])
      @elem{
        Even worse, nearly half the configurations in @id[num-bad] benchmarks
         (@id[(* 100 (rnd (/ num-bad (count-benchmarks))))]%) have over 20x overhead.
      })
  }
  @item{
    At @math{k=1}, only @bm{synth} and @bm{quadMB} have any configurations that
     cannot reach a @deliverable{20} configuration.
    This suggests that many of the absolute worst overheads are due to one or
     two problematic type boundaries.
  }
  @item{
    That said, the @math{k=1} graphs are disturbingly similar to the @math{k=0}
     graphs, especially at low overheads.
    Even a 3x overhead will not be ``deliverable'' for most programmers.
    Thus, incrementally converting a module or two to Typed Racket is apparently
     a poor strategy for reducing overhead introduced by gradual typing.
  }
  @item{
    All benchmarks, even @bm{snake} and @bm{synth}, have a handful of configurations
     with less than 2x overhead.
    Can we guide developers to those configurations?
  }
  @item{
    Generally, small improvements with newer versions of Racket.
    Caveats:
    @itemize[
      @item{
        Forth worse after 6.2 because @todo{why?}
      }
      @item{
        zombie @math{k=1} is strange
      }
      @item{
        tetris at 1.2x @math{k=1} is strange
      }
    ]
  }
]


@; -----------------------------------------------------------------------------
@section[#:tag "sec:compare"]{Comparing Typed Rackets}

Overhead plots give high-level differences.
Need also scatterplots to really compare versions (with @emph{confidence}).
@todo{add plots}

Note: The small but statistically significant variations in the untyped running
 mean that overheads from one version of Racket are
 not directly comparable to another for those benchmarks,
 but such are the hazards of benchmarking a large system.
At any rate, we hold that users' decision to invest in gradual typing will
 be influenced mostly by the overhead they witness---regardless of whether
 that overhead is due to enforcing type soundness or because untyped Racket
 is magically faster than all of Typed Racket.
For completeness we list all untyped runtimes in Appendix @todo{ref}.

