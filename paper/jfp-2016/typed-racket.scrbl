#lang scribble/base

@; To address this issue, the implementors of Typed Racket  included a small
@;  number of run-time libraries as trusted code with unchecked type environments.
@; The next section explains what ``completely typed'' means for the individual
@;  benchmarks.

@require["common.rkt"]

@title[#:tag "sec:tr"]{Evaluating Typed Racket}

For our evaluation of Typed Racket, we use a suite of @(id (length benchmarks)) programs
 and generate timings over the whole performance lattice for each.
As lattices for projects with more than 6 modules are too large to analyze at
 a glance, we present our results in terms of @step["L" "N" "M"].
The benchmarks themselves are representative of actual user code yet
 small enough that exhaustive performance evaluation remains tractable.


@section[#:tag "sec:bm"]{The Benchmark Programs}

@benchmark[
  #:name "sieve"
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin "Synthetic"
  #:purpose "Finds prime numbers using the Sieve of Eratosthenes."
]{
  We created the @tt{sieve} benchmark to demonstrate a scenario where user
   code closely interacts with higher-order library code---in this case, a stream
   library.
  When fully typed or untyped, @tt{sieve} runs quickly.
  Otherwise performance is significantly worse.
}

@benchmark[
  #:name "morsecode"
  #:author "John Clements & Neil Van Dyke"
  #:num-adaptor 0
  #:origin "Library"
  #:purpose "Generate morse code strings, compare against user input"
]{
  The original program plays an audio clip, waits for keyboard input,
   then scores the input based on its Levenshtein distance from the
   correct answer.
  Our benchmark uses a list of common English words as input.
}

@benchmark[
  #:name "mbta"
  #:author "Matthias Felleisen"
  #:num-adaptor 0
  #:origin "Educational"
  #:purpose "Answer reachability queries about Boston's transit system"
]{
  Builds a graph representation of Boston's public transit system and
   answers a series of reachability queries.
  The original program ran an asynchronous server, but our benchmark is
   single-threaded to cooperate with Racket's profiling tools.
}

@benchmark[
  #:name "zordoz"
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin "Library"
  #:purpose "Explore bytecode (.zo) files"
]{
  Decompiles a Racket bytecode file and counts the frequency of AST nodes
   in the result.
  The decompile API is untyped, so @tt{zordoz} suffers overhead when completely
   typed.
}

@benchmark[
  #:name "suffixtree"
  #:author "Danny Yoo"
  #:num-adaptor 1
  #:origin "Library"
  #:purpose "Implement Ukkonen's suffix tree algorithm"
]{
  Runs a longest-common-substring algorithm on all pairs of words in a small
   text file.
}

@benchmark[
  #:name "lnm"
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin "Synthetic"
  #:purpose "Produce L-N/M plots"
  #:external-libraries '("plot" "racket/statistics")
]{
  Creates the @math{L-N/M} plots for the @tt{gregor} benchmark.
  This program is an early revision of the script we use to compile this article.
}

@benchmark[
  #:name "kcfa"
  #:author "Matt Might"
  #:num-adaptor 4
  #:origin "Blog post"
  #:purpose "Demo of k-CFA algorithm"
]{
  Runs a na\"ive control-flow analysis on a small lambda calculus term.
}

@benchmark[
  #:name "snake"
  #:author "David Van Horn"
  #:num-adaptor 1
  #:origin "Educational"
  #:purpose "Game"
]{
  Implements a small game where a growing and moving snake avoids walls and
   its own tail.
  Our benchmark runs a pre-recorded history of moves, altering the game state
   but not producing GUI output.
  Based on a contract verificationbenchmark@note{@url["http://github.com/philnguyen/soft-contract"]}
   by Nguyễn @|etal|@~cite[nthvh-icfp-2014].
}

@benchmark[
  #:name "tetris"
  #:author "David Van Horn"
  #:num-adaptor 1
  #:origin "Educational"
  #:purpose "Game"
]{
  Implements a game of tetris.
  Like @tt{snake}, runs a pre-recorded set of moves.
}

@benchmark[
  #:name "synth"
  #:author "Vincent St. Amour and Neil Toronto"
  #:num-adaptor 1
  #:origin @hyperlink["http://github.com/stamourv/synth"]{Library}
  #:purpose "Music synthesis DSL"
]{
  Converts a description of music to a playable @tt{.wav} file.
  This benchmark incorporates 5 modules from Racket's typed @tt{math/array} library,
   which is known to cause performance issues for untyped clients.
}

@benchmark[
  #:name "gregor"
  #:author "Jon Zeppieri"
  #:num-adaptor 2
  #:origin @hyperlink["https://docs.racket-lang.org/gregor/index.html"]{Library}
  #:purpose "Date & time library"
  #:external-libraries '("cldr")
]{
  The original library an untyped mechanism for ad-hoc polymorphism
   that is not supported by Typed Racket.
  Our adaptation instead uses monomorphic types and removes a string parsing
   front-end.
}

@benchmark[
  #:name "quad"
  #:author "Matthew Butterick"
  #:num-adaptor 2
  #:origin "Library"
  #:purpose @hyperlink["https://github.com/mbutterick/quad"]{Typesetting library}
]{
  
  Our evalutation uses two v
  (Hm, just make 2 descriptions?)
}

@; TODO forth, fsm, etc

@;@benchmark[
@;  #:name ""
@;  #:author ""
@;  #:num-adaptor 0
@;  #:origin ""
@;  #:purpose ""
@;]{
@;  yolo
@;}


@; The table in @figure-ref{fig:bm} lists and summarizes our twelve benchmark programs.
@; For each, we give an approximate measure of the program's size,
@;  a diagram of its module structure, and @todo{colors?}.
@; 
@; Size is measured by the number of modules and lines of code (LOC) in a program.
@; Crucially, the number of modules also determines the number of gradually-typed
@; configurations to be run when testing the benchmark, as a program with @math{n} modules
@; can be gradually-typed in @exact{$2^n$} possible configurations.
@; Lines of code is less important for evaluating macro-level gradual typing,
@; but gives a sense of the overall complexity of each benchmark.
@; Moreover, the Type Annotations LOC numbers are an upper bound on the annotations required
@; at any stage of gradual typing because each typed module in our experiment
@; fully annotates its import statements.
@; 
@; The column labeled ``Other LOC'' measures the additional infrastructure required
@; to run each project for all typed-untyped configurations. This count includes
@; project-wide type definitions, typed interfaces to untyped libraries, and
@; any so-called type adaptor modules (see below).
@; 
@; The module structure graphs show a dot for each module in the program.
@; An arrow is drawn from module A to module B when module A imports definitions
@; from module B.
@; When one of these modules is typed and the other untyped, the imported definitions
@; are wrapped with a contract to ensure type soundness. To give a sense of how
@; ``expensive'' the contracts at each boundary are, we color
@; arrows to match the absolute number of times contracts at a given boundary
@; are checked. These numbers are independent from the actual configurations.
@; 
@; The colors fail to show the cost of checking data structures
@; imported from another library or factored through an adaptor module.
@; For example, the @tt{kcfa} graph has many thin black edges because the modules
@; only share data definitions. The column labeled ``Adaptors + Libraries''
@; reports the proportion of observed contract checks due to adaptor modules and
@; libraries.
@; 
@; @; This stupid figure placement is necessary to make it show up in the right place in paper
@; @figure-here["fig:adaptor" "Inserting a type adaptor"
@; @exact|{
@; \input{fig-adaptor.tex}
@; }|
@; ]
@; 
@; 
@; 
@; @figure*["fig:bm" "Characteristics of the benchmarks"
@;   @exact|{\input{fig-characteristics.tex}}|
@; ]
@; 
@; @section{Adaptor Modules}
@; 
@; A quirk in Racket's structure-type definitions calls for one twist to
@; an otherwise straightforward setup of benchmark configurations. Consider
@; the following structure-type definition from @tt{gregor}, one of the
@; benchmark programs: 
@; @;%
@; @(begin
@; #reader scribble/comment-reader
@; (racketblock
@; (struct DateTime [date time jd])
@; ))
@; @;%
@; Its evaluation introduces a new class of data structures via a constructor
@; (@racket[DateTime]), a predicate (@racket[DateTime?]), and a number of
@; selectors. A second evaluation creates a disjoint
@; class of structures, meaning the selectors for the
@; first class do not work on the second and vice versa.
@; 
@; If a structure-type definition is exported, a configuration may place the definition
@; in an untyped module and its clients into the typed portion of the program.
@; As explained below, importing a @racket[struct] demands that each client
@; assigns a type to the structure-type definition. Now, when these typed
@; clients wish to exchange instances of these structure types, the type
@; checker must prove that the static types match. But due to the above
@; quirk, the type system assigns generative static types to imported structure
@; types. Thus, even if the developers who annotate the two clients with types choose
@; the same names for the imported structure types, the two clients actually have
@; mutually incompatible static types.
@; 
@; @Figure-ref{fig:adaptor} illuminates the problems with the left-hand
@; diagram. An export of a structure-type definition from the untyped module
@; (star-shaped) to the two typed clients (black squares) ensures that the
@; type checker cannot equate the two assigned static types. The right-hand
@; side of the figure explains the solution. We manually add a @emph{type
@; adaptor module}. Such adaptor modules are specialized typed interfaces to
@; untyped code. The typed clients import structure-type definitions and the
@; associated static types exclusively from the type adaptor, ensuring that
@; only one canonical type is generated for each structure type.  Untyped
@; clients remain untouched and continue to use the original untyped file.
@; 
@; Adaptor modules also reduce the number of type annotations needed at
@; boundaries because all typed clients can reference a single point of
@; control.@note{In our experimental setup, type adaptors are available to
@; all configurations as library files.}  Therefore we expect type adaptor
@; modules to be of independent use to practitioners, rather than just a
@; synthetic byproduct of our setup.
@; 
@; @section{Program Descriptions}
@; 
@; This section briefly describes each benchmark, noting the dependencies and required adaptor
@; modules.  Unless otherwise noted, the benchmarks rely
@; only on core Racket libraries and do not use adaptor modules.
@; We credit program authors in parentheses; except for @tt{sieve}, all programs
@; are independently useful.
@; 
@; @parag{Sieve (Ben Greenman)}
@; This program finds prime numbers using the Sieve of Eratosthenes and is our
@; smallest benchmark. It contains two modules: a streams library and the
@; sieve code.
@; We wrote this benchmark to illustrate the pitfalls of sound gradual typing.
@; 
@; @parag{Morse code (John Clements & Neil Van Dyke)}
@; This script is adapted from a morse code training program.@note{@url["http://github.com/jbclements/morse-code-trainer"]}
@; The original program plays a morse code audio clip, 
@; reads keyboard input, and scores the input based on its
@; Levenshtein distance from the correct answer.
@; Our benchmark setup generates morse code strings and runs the
@; Levenshtein algorithm on a list of frequently used words.
@; 
@; @parag{MBTA (Matthias Felleisen)}
@; The @tt{mbta} program builds a representation of Boston's public transit system
@; and answers reachability queries.
@; It relies on an untyped graph library.
@; The original program responded asynchronously to queries with a server thread.
@; We instead measure a synchronous version of the program to ensure compatibility
@; with Racket's stack-based profiling tools.
@; 
@; @parag{Zordoz (Ben Greenman)}
@; This tool is used for exploring and counting the frequency of
@; Racket bytecode structures.
@; It operates on the Racket compiler's untyped zo data structures.
@; Since these data structures are not natively supported in Typed Racket, even the
@; completely typed program incurs some dynamic overhead.
@; 
@; @parag{Suffixtree (Danny Yoo)}
@; This library implements a longest common substring algorithm
@; using Ukkonen's suffix tree algorithm. While the library has
@; minimal external dependencies, it calls for one adaptor module for the
@; algorithm's internal data structures.
@; 
@; @parag{LNM (Ben Greenman)}
@; This script analyzes the measurements included in this paper
@; and generates figures 4 and 5. @; @figure-ref{fig:lnm1} and @figure-ref:lnm2}.
@; Most of this benchmark's running time is spent generating figures using Typed Racket's @tt{plot} library, so the @emph{untyped} version of this program is noticeably less performant.
@; This program relies on an untyped image rendering library and uses two adaptor modules.
@; 
@; @parag{KCFA (Matt Might)}
@; The @tt{kcfa} program implements a simple control flow analysis for a
@; lambda calculus.
@; The language definitions and analysis are spread across seven modules, four of
@; which require adaptors because they introduce new datatypes.
@; 
@; @parag{Snake (David Van Horn)} This program is based on a contract verification
@; benchmark@note{@url["http://github.com/philnguyen/soft-contract"]} by
@; Nguyễn @|etal|@~cite[nthvh-icfp-2014].  It implements a game where a growing
@; and moving snake tries to eat apples while avoiding walls and its own tail.
@; Our benchmark runs a pre-recorded history of moves altering
@; the game state and does not display a GUI.  We use one adaptor module to
@; represent the game datatypes, but otherwise the program is self-contained.
@; 
@; @parag{Tetris (David Van Horn)}
@; This program is taken from the same benchmark suite as @tt{snake}@~cite[nthvh-icfp-2014]
@; and implements the eponymous game.
@; Like @tt{snake}, the benchmark runs a pre-recorded set of moves. Using it here requires
@; one adaptor module.
@; 
@; @parag{Synth (Vincent St-Amour & Neil Toronto)}
@; The @tt{synth} benchmark@note{@url["http://github.com/stamourv/synth"]}
@; is a sound synthesis example from St-Amour @|etal|'s work on
@; feature-specific profiling@~cite[saf-cc-2015].
@; The program consists of nine modules, half of which are from Typed Racket's array library.
@; In order to run these library modules in all typed-untyped configurations we create an adaptor module
@; for the underlying array data structure.
@; 
@; @parag{Gregor (Jon Zeppieri)}
@; This benchmark consists of thirteen modules and stress-tests a date and time library.
@; The original library uses a
@; library for ad-hoc polymorphism that is not supported by Typed Racket.
@; Our adaptation instead uses a mono-typed variant of this code and removes
@; the string parsing component.
@; The benchmark uses two adaptor modules and relies on a small, untyped library for
@; acquiring data on local times.
@; 
@; @parag{Quad (Matthew Butterick)}
@; This project implements a type-setting library.
@; It depends on an external constraint satisfaction solver
@; library (to divide lines of text across multiple columns) and uses two adaptor modules.
@; The original author provided both untyped and fully typed variants.
@; 
@; @; -----------------------------------------------------------------------------
@; 
@; @; @title[#:tag "sec:tr"]{Evaluating Typed Racket} @; , Classical
@; @; 
@; @; Measuring the running time for the performance lattices of our
@; @; benchmarks means compiling, running, and timing thousands of
@; @; configurations. Each configuration is run 30 times to ensure that the
@; @; @;
@; @; @;@margin-note*{cite the random number paper?}
@; @; @;
@; @; timing is not affected by random factors; some configurations take minutes
@; @; to run.
@; @; 
@; @; Here we present our measurements in terms of the metrics of section@secref["sec:fwk"].
@; @; The first subsection discusses one benchmark in detail, demonstrating how we
@; @;  create the configurations, how the boundaries affect the performance of
@; @;  various configurations, and how the Typed Racket code base limits the
@; @;  experiment.  The second subsection explains our findings.
@; @;  The last subsection interprets them.
@; @; 
@; @; @parag{Experimental setup}
@; @; Due to the high resource requirements of evaluating 
@; @; the performance lattices, experiments were run on multiple machines.
@; @; Machine A with 12 physical Xeon E5-2630 2.30GHz cores and 64GB RAM, Machine B
@; @; with 4 physical Core i7-4790 3.60GHz cores and 16GB RAM, Machine C with
@; @; with 4 physical Core i7-3770K 3.50GHz cores and 32GB RAM,
@; @; and a set of Machines D
@; @; @;from the Northeastern University Discovery Cluster@note{@url{nuweb12.neu.edu/rc/?page_id=27}}
@; @; with identical configurations of 20 physical Xeon E5-2680 2.8GHz cores
@; @; with 64GB RAM. All machines run a variant of Linux and all benchmarks were run
@; @; on Racket v6.2.
@; @; The following benchmarks were run on machine A: @tt{sieve}, @tt{kcfa}, and @tt{gregor}.
@; @; On machine B: @tt{suffixtree}, @tt{morse-code}, @tt{mbta}, and @tt{lnm}.
@; @; On machine C: @tt{zordoz} and @tt{quad}.
@; @; On machine D: @tt{snake}, @tt{synth}, and @tt{tetris}.
@; @; For each configuration we report the average of 30 runs.
@; @; All of our runs use a single core for each configuration.
@; @; We performed sanity checks to validate that performance differentials reported
@; @; in the paper were not affected by the choice of machine.@;
@; @; @note{The scripts that we use to run the experiments are available in
@; @; our artifact: @url{http://www.ccs.neu.edu/racket/pubs/#popl15-tfgnvf}}
@; @; 
@; @; @; -----------------------------------------------------------------------------
@; @; @section{Suffixtree in Depth}
@; @; 
@; @; To illustrate the key points of the evaluation, this section describes
@; @; one of the benchmarks, @tt{suffixtree}, and explains the setup and
@; @; its timing results in detail.
@; @; 
@; @; @tt{Suffixtree} consists of six modules: @tt{data} to define label and
@; @; tree nodes, @tt{label} with functions on suffixtree node labels,
@; @; @tt{lcs} to compute longest common substrings, @tt{main} to apply
@; @; @tt{lcs} to @tt{data}, @tt{structs} to create and traverse suffix tree nodes,
@; @; @tt{ukkonen} to build suffix trees via Ukkonen's algorithm. Each
@; @; module is available with and without type annotations.  Each configuration
@; @; thus links six modules, some of them typed and others untyped.
@; @; 
@; @; @; @figure["fig:purpose-statements" "Suffixtree Modules"
@; @; @; @tabular[#:sep @hspace[2]
@; @; @; (list (list @bold{Module} @bold{Purpose})
@; @; @; (list @tt{data.rkt}    "Label and tree node data definitions")
@; @; @; (list @tt{label.rkt}   "Functions on suffixtree node labels")
@; @; @; (list @tt{lcs.rkt}     "Longest-Common-Subsequence implementation")
@; @; @; (list @tt{main.rkt}    "Apply lcs to benchmark data")
@; @; @; (list @tt{structs.rkt} "Create and traverse suffix tree nodes")
@; @; @; (list @tt{ukkonen.rkt} "Build whole suffix trees via Ukkonen's algorithm"))]]
@; @; 
@; @; 
@; @; @figure*["fig:suffixtree" 
@; @;           @list{Performance lattice (labels are speedup/slowdown factors)}
@; @;   @(let* ([vec (file->value SUFFIXTREE-DATA)]
@; @;           [vec* (vector-map (λ (p) (cons (mean p) (stddev p))) vec)])
@; @;      (make-performance-lattice vec*))
@; @; ]
@; @; 
@; @; Typed modules require type annotations on their data definitions and functions.
@; @; Modules provide their exports with types, so that the
@; @; type checker can cross-check modules. A typed module may import
@; @; values from an untyped module, which forces the
@; @; corresponding @racket[require] specifications to come with
@; @; types. Consider this example:
@; @; @;%
@; @; @(begin
@; @; #reader scribble/comment-reader
@; @; (racketblock
@; @; (require (only-in "label.rkt" make-label ...))
@; @; ))
@; @; @;%
@; @; The server module is called @tt{label.rkt}, and the client imports specific
@; @;  values, e.g., @tt{make-label}.  This specification is replaced with a
@; @;  @racket[require/typed] specification where each imported identifier is
@; @;  typed:
@; @; @;%
@; @; @(begin
@; @; #reader scribble/comment-reader
@; @; (racketblock
@; @; (require/typed "label.rkt" 
@; @;  [make-label
@; @;   (-> (U String (Vectorof (U Char Symbol))) Label)]
@; @;  ...)
@; @; ))
@; @; @; 
@; @; 
@; @; The types in a
@; @; @racket[require/typed] form are compiled into contracts for
@; @; the imported values. For example, if some
@; @; imported variable is declared to be a @tt{Char}, the check @racket[char?]
@; @; is performed as the value flows across the module boundary. Higher-order
@; @; types (functions, objects, or classes) become contracts that wrap
@; @; the imported value and which check future interactions of this
@; @; value with its context.
@; @; 
@; @; The performance costs of gradual typing thus consist of wrapper allocation
@; @; and run-time checks. Moreover, the compiler must assume that
@; @; any value could be wrapped, so it cannot generate direct field access code
@; @; as would be done in a statically typed language.
@; @; 
@; @; Since our evaluation setup calls for linking typed modules to both typed
@; @; and untyped server modules, depending on the configuration, we replace
@; @; @racket[require/typed] specifications with @racket[require/typed/check]
@; @; versions. This new syntax can determine whether the server module is typed
@; @; or untyped. It installs contracts if the server module
@; @; is untyped, and it ignores the annotation if the server module is typed.
@; @; As a result, typed modules function independently of the rest of the
@; @; modules in a configuration.
@; @; 
@; @; 
@; @; @; -----------------------------------------------------------------------------
@; @; @parag{Performance Lattice.}
@; @; 
@; @; @Figure-ref{fig:suffixtree} shows the performance lattice annotated with the
@; @;   timing measurements. The lattice displays each of the modules in the
@; @;   program with a shape.  A filled black shape means the module is typed, an
@; @;   open shape means the module is untyped. The shapes are ordered from left
@; @;   to right and correspond to the modules of @tt{suffixtree} in alphabetical
@; @;   order: @tt{data}, @tt{label}, @tt{lcs}, @tt{main}, @tt{structs}, and
@; @;   @tt{ukkonen}.
@; @; 
@; @;  For each configuration in the lattice, the ratio is
@; @;  computed by dividing the average timing of the typed program by
@; @;  the untyped average. The figure omits standard deviations
@; @;  as they are small enough to not affect the discussion.
@; @; 
@; @; The fully typed configuration (top) is @emph{faster} than the fully untyped
@; @;  (bottom) configuration by around 30%, which puts the typed/untyped ratio at 0.7. This can
@; @;  be explained by Typed Racket's optimizer, which performs specialization of
@; @;  arithmetic operations and field accesses, and can eliminate some
@; @;  bounds checks@~cite[thscff-pldi-2011]. When the optimizer is turned off,
@; @;  the ratio goes back up to 1. 
@; @; 
@; @; 
@; @; Sadly, the performance improvement of the typed configuration is the
@; @;  only good part of this benchmark. Almost all partially typed configurations
@; @;  exhibit slowdowns of up to 105x. Inspection of the lattice
@; @;  suggests several points about these slowdowns: @itemlist[
@; @; 
@; @; @item{Adding type annotations to the @tt{main} module neither subtracts nor
@; @;  adds overhead because it is a driver module.}
@; @; 
@; @; 
@; @; @item{Adding types to any of the workhorse modules---@tt{data}, @tt{label},
@; @;  or @tt{structs}---while leaving all other modules untyped causes slowdown of
@; @;  at least 35x. This group of modules are tightly coupled.
@; @;  Laying down a type-untyped boundary to separate
@; @;  elements of this group causes many crossings of values, with associated
@; @;  contract-checking cost.}
@; @; 
@; @; @item{Inspecting @tt{data} and @tt{label} further reveals that the latter
@; @;  depends on the former through an adaptor module. This adaptor introduces a
@; @;  contract boundary when either of the two modules is untyped. When both
@; @;  modules are typed but all others remain untyped, the slowdown is reduced
@; @;  to about 13x.
@; @; 
@; @;  The @tt{structs} module depends on @tt{data} in the same fashion and
@; @;  additionally on @tt{label}. Thus, the configuration in which both
@; @;  @tt{structs} and @tt{data} are typed still has a large slowdown. When all
@; @;  three modules are typed, the slowdown is reduced to 5x.}
@; @; 
@; @; @item{Finally, the configurations close to the worst slowdown case are
@; @;  those in which the @tt{data} module is left untyped but several of the
@; @;  other modules are typed. This makes sense given the coupling noted
@; @;  above; the contract boundaries induced between the untyped @tt{data} and
@; @;  other typed modules slow down the program.  The module structure diagram
@; @;  for @tt{suffixtree} in @figure-ref{fig:bm} corroborates the presence of
@; @;  this coupling. The rightmost node in that diagram corresponds to the
@; @;  @tt{data} module, which has the most in-edges in that particular
@; @;  graph. We observe a similar kind of coupling in the simpler @tt{sieve}
@; @;  example, which consists of just a data module and its client.}
@; @; ]
@; @; 
@; @; The performance lattice for @tt{suffixtree} is bad news for gradual typing.
@; @; It exhibits performance ``valleys'' in which a maintenance programmer can get stuck.
@; @; Consider starting with the untyped program, and for some reason choosing
@; @; to add types to @tt{label}. The program slows down by a factor of 88x. Without any
@; @; guidance, a developer may choose to then add types to @tt{structs} and see the
@; @; program slow down to 104x.  After that, typing @tt{main} (104x), @tt{ukkonen}
@; @; (99x), and @tt{lcs} (103x) do little to improve performance. It is only
@; @; when all the modules are typed that performance becomes acceptable again (0.7x).
@; @; 
@; @; 
@; @; @figure*["fig:lnm1"
@; @;   @list{@step["L" "N" "M"] results for the first six benchmarks}
@; @;   @(let* ([data `(("sieve"        ,SIEVE-DATA)
@; @;                   ("morse-code"   ,MORSECODE-DATA)
@; @;                   ("mbta"         ,MBTA-DATA)
@; @;                   ("zordoz"       ,ZORDOZ-DATA)
@; @;                   ("suffixtree"   ,SUFFIXTREE-DATA)
@; @;                   ("lnm"          ,LNM-DATA)
@; @;                   )])
@; @;      (data->pict data #:tag "1"))
@; @; ]
@; @; 
@; @; @figure*["fig:lnm2"
@; @;   @list{@step["L" "N" "M"] results for the remaining benchmarks}
@; @;   @(let* ([data `(("kcfa"       ,KCFA-DATA)
@; @;                   ("snake"      ,SNAKE-DATA)
@; @;                   ("tetris"     ,TETRIS-DATA)
@; @;                   ("synth"      ,SYNTH-DATA)
@; @;                   ("gregor"     ,GREGOR-DATA)
@; @;                   ("quad"       ,QUAD-DATA))])
@; @;      (data->pict data #:tag "2"))
@; @; ]
@; @; 
@; @; 
@; @; @; -----------------------------------------------------------------------------
@; @; @section{Reading the Figures}
@; @; 
@; @; Our method defines the number of @step["L" "N" "M"] configurations as the key metric for measuring the quality of a gradual type system.
@; @; For this experiment we have chosen values of 3x and 10x for @math{N} and @math{M}, respectively, and allow up to 2 additional type conversion steps.
@; @; These values are rather liberal,@note{We would expect that most production contexts would not tolerate anything higher than 2x, if that much.} but serve to ground our discussion.
@; @; 
@; @; The twelve rows of graphs in @Figure-ref["fig:lnm1" "fig:lnm2"] summarize the results from exhaustively exploring the performance lattices of our benchmarks.
@; @; Each row contains a table of summary statistics and one graph for each value of @math{L} between 0 and 2.
@; @; 
@; @; The typed/untyped ratio is the slowdown or speedup of fully typed code over untyped code.
@; @; Values smaller than @math{1.0} indicate a speedup due to Typed Racket optimizations.
@; @; Values larger than @math{1.0} are slowdowns caused by interaction with untyped libraries or untyped parts of the underlying Racket runtime.
@; @; The ratios range between 0.28x (@tt{lnm}) and 3.22x (@tt{zordoz}).
@; @; 
@; @; The maximum overhead is computed by finding the running time of the slowest configuration and dividing it by the running time of the untyped configuration.
@; @; The average overhead is obtained by computing the average over all configurations (excluding the fully-typed and untyped configurations) and dividing it by the running time of the untyped configuration.
@; @; Maximum overheads range from 1.25x (@tt{lnm}) to 168x (@tt{tetris}).
@; @; Average overheads range from 0.6x (@tt{lnm}) to 68x (@tt{tetris}).
@; @; 
@; @; The @deliverable{3} and @usable["3" "10"] counts are computed for @math{L=0}.
@; @; In parentheses, we express these counts as a percentage of all configurations for the program.
@; @; 
@; @; The three cumulative performance graphs are read as follows.
@; @; The x-axis represents the slowdown over the untyped program (from 1x to @id[PARAM-MAX-OVERHEAD]x).
@; @; The y-axis is a count of the number of configurations (from @math{0} to @math{2^n}) scaled so that all graphs are the same height.
@; @; If @math{L} is zero, the blue line represents the total number of configurations with performance no worse than the overhead on the x-axis.
@; @; For arbitrary @math{L}, the blue line gives the number of configurations that can reach a configuration with performance no worse than the overhead on the x-axis in at most @math{L} conversion steps.
@; @; 
@; @; The ideal result would be a flat line at a graph's top.
@; @; Such a result would mean that all configurations are as fast as (or faster than) the untyped one.
@; @; The worst scenario is a flat line at the graph's bottom, indicating that all configurations are more than 20x slower than the untyped one.
@; @; For ease of comparison between graphs, a dashed (@exact{\color{red}{red}}) horizontal line indicates the 60% point along each project's y-axis.
@; @; 
@; @; 
@; @; @; -----------------------------------------------------------------------------
@; @; @section[#:tag "sec:all-results"]{Interpretation}
@; @; 
@; @; The ideal shape is difficult to achieve because of the overwhelming cost of the
@; @; dynamic checks inserted at the boundaries between typed and untyped code.
@; @; The next-best shape is a nearly-vertical line that reaches the top at a low x-value.
@; @; All else being equal, a steep slope anywhere on the graph is desirable because
@; @; the number of acceptable programs quickly increases at that point.
@; @; 
@; @; For each benchmark, we evaluate the actual graphs against these expectations.
@; @; Our approach is to focus on the left column, where @math{L}=0, and to consider the
@; @; center and right column as rather drastic countermeasures to recover
@; @; performance.@note{Increasing @math{L} should remove pathologically-bad cases.} 
@; @; 
@; @; @parag{Sieve}
@; @; The flat line at @math{L}=0 shows that half of all configurations suffer
@; @; unacceptable overhead. As there are only 4 configurations in the lattice
@; @; for @tt{sieve}, increasing @math{L} improves performance.
@; @; 
@; @; @parag{Morse code}
@; @; The steep lines show that a few configurations suffer modest overhead (below 2x),
@; @; otherwise @tt{morse-code} performs well.
@; @; Increasing @math{L} improves the worst cases.
@; @; 
@; @; @parag{MBTA}
@; @; These lines are also steep, but flatten briefly at 2x.
@; @; This coincides with the performance of the fully-typed
@; @; configuration.
@; @; As one would expect, freedom to type additional modules adds configurations
@; @; to the @deliverable{2} equivalence class.
@; @; 
@; @; @parag{Zordoz}
@; @; Plots here are similar to @tt{mbta}.
@; @; There is a gap between the performance of the fully-typed
@; @; configuration and the performance of the next-fastest lattice point.
@; @; 
@; @; @parag{Suffixtree}
@; @; The wide horizontal areas are explained by the performance lattice in
@; @; @figure-ref{fig:suffixtree}: configurations' running times are not evenly
@; @; distributed but instead vary drastically when certain boundaries exist.
@; @; Increasing @math{L} significantly improves the number of acceptable configuration
@; @; at 10x and even 3x overhead.
@; @; 
@; @; @parag{LNM}
@; @; These results are ideal.
@; @; Note the large y-intercept at @math{L}=0.
@; @; This shows that very few configurations suffer any overhead.
@; @; 
@; @; @parag{KCFA}
@; @; The most distinctive feature at @math{L}=0 is the flat portion between 1x
@; @; and 6x. This characteristic remains at @math{L}=1, and overall performance
@; @; is very good at @math{L}=2.
@; @; 
@; @; @parag{Snake}
@; @; The slope at @math{L}=0 is very low.
@; @; Allowing @math{L}=1 brings a noticeable improvement above the 5x mark,
@; @; but the difference between @math{L}=1 and @math{L}=2 is small.
@; @; 
@; @; @parag{Tetris}
@; @; Each @tt{tetris} plot is essentially a flat line.
@; @; At @math{L}=0 roughly 1/3 of configurations lie below the line.
@; @; This improves to 2/3 at @math{L}=1 and only a few configurations suffer overhead
@; @; when @math{L}=2.
@; @; 
@; @; @parag{Synth}
@; @; Each slope is very low.
@; @; Furthermore, some configurations remain unusable even at @math{L}=2.
@; @; These plots have few flat areas, which implies that overheads are spread
@; @; evenly throughout possible boundaries in the program.
@; @; 
@; @; @parag{Gregor}
@; @; These steep curves are impressive given that @tt{gregor} has 13 modules.
@; @; Increasing @math{L} brings consistent improvements.
@; @; 
@; @; @parag{Quad}
@; @; The @tt{quad} plots follow the same pattern as @tt{mbta} and @tt{zordoz}, despite being visually distinct.
@; @; In all three cases, there is a flat slope for overheads below the typed/untyped ratio and a steep increase just after.
@; @; The high typed/untyped ratio is explained by small differences in the original author-supplied variants.
