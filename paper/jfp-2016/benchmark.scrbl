#lang scribble/base

@require[
  "benchmark.rkt"
  "common.rkt"
  "typed-racket.rkt"
  "util.rkt"
  (only-in gtp-summarize/path-util add-commas)
]

@profile-point{sec:bm}
@title[#:tag "sec:bm"]{The @|GTP| Benchmark Programs}

The @integer->word[(count-benchmarks)] @emph{gradual typing performance} (@|GTP|) benchmark programs are representative of actual user code yet small enough to make exhaustive performance evaluation tractable.
The following descriptions, arranged from smallest number of experimental modules to largest, briefly summarize each benchmark.
Each description comes with four fields:
 @emph{Origin} indicates the benchmark's source,
 @emph{Purpose} describes what it computes,
 @emph{Author} credits the original author,
 and @emph{Depends} lists any libraries of fixed modules that the benchmark depends on.
This section concludes with a table summarizing the static characteristics of each benchmark.


@; -----------------------------------------------------------------------------
@; @subsection{Benchmark Descriptions}

@profile-point{sec:tr:descriptions}
@parag[]
@render-benchmark-descriptions[
@(cons sieve
  @elem{
    Demonstrates a scenario where client code is tightly coupled to higher-order library code.
    The library implements a stream data structure; the client builds a stream of prime numbers.
    Introducing a type boundary between these modules leads to significant overhead.
  })
(cons forth
  @elem{
    Interprets Forth programs.
    The interpreter represents calculator commands as a list of first-class objects.
    These objects accumulate proxies as they cross type boundaries.
  })
(cons (list fsm fsmoo)
  @elem{
    Simulates the interactions of economic agents via finite-state automata@~cite[n-mthesis-2014].
    This benchmark comes in two flavors: @bm[fsm] stores the agents in a mutable vector and whereas @bm[fsmoo] uses a first-class object.
  })
(cons mbta
  @elem{
    Builds a map of Boston's subway system and answers reachability queries.
    The map encapsulates a boundary to Racket's untyped @library{graph} library; when the map is typed, the (type) boundary to @library{graph} is a performance bottleneck.
  })
(cons morsecode
  @elem{
    Computes Levenshtein distances and morse code translations for a fixed sequence of pairs of words.
    Every function that crosses a type boundary in @bm[morsecode] operates on strings and integers, thus dynamically type-checking these functions' arguments is relatively cheap.
  })
(cons zombie
  @elem{
    Implements a game where players dodge computer-controlled ``zombie'' tokens.
    Curried functions over symbols implement game entities and repeatedly cross type boundaries.

    @;@racket[
    @;  (define-type Point
    @;    ((U 'x 'y 'move)
    @;     ->
    @;     (U (Pairof 'x (-> Real))
    @;        (Pairof 'y (-> Real))
    @;        (Pairof 'move (Real Real -> Point)))))
    @;]

  })
(cons dungeon
  @elem{
    Builds a grid of wall and floor objects by choosing first-class classes from a list of ``template'' pieces.
    This list accumulates proxies when it crosses a type boundary.

    @;Originally, the program imported the Racket @library{math} library
    @; for array operations and @library{racket/dict} for a generic dictionary interface.
    @;The benchmark uses Racket's vectors instead of the @library{math} library's arrays
    @; because Typed Racket v6.2 could not compile the type @racket[(Mutable-Array (Class))] to a contract.
    @;The benchmark does not use @library{racket/dict} so that the results for
    @; @bm[dungeon] describe internal type boundaries rather than the type
    @; boundary to the untyped dict interface.
  })
(cons zordoz
  @elem{
    Traverses Racket bytecode (@tt{.zo} files).
    The @library{compiler-lib} library defines the bytecode data structures.
    Typed code interacting with the library suffers overhead.

    @emph{Note}:
     the Racket bytecode format changed between versions 6.2 and 6.3 with
     the release of the set-of-scopes macro expander@~cite[f-popl-2016].
    This change significantly reduced the overhead of @bm[zordoz].

    @;As it turns out, the change from 6.2 to 6.3 improved the typed/untyped ratio
    @; from
    @; @add-commas[(rnd (typed/untyped-ratio (benchmark-rktd zordoz "6.2")))]x in v6.2 to
    @; @add-commas[(rnd (typed/untyped-ratio (benchmark-rktd zordoz "6.3")))]x in v6.3 because
    @; the more recent bytecode structures generate less expensive type contracts.
    @;The ratio for the newest bytecode format is
    @; @add-commas[(rnd (typed/untyped-ratio (benchmark-rktd zordoz "6.5")))]x.
  })
(cons lnm
  @elem{
    Renders overhead graphs@~cite[tfgnvf-popl-2016].
    Two modules are tightly-coupled to Typed Racket libraries; typing both modules improves performance.
  })
(cons suffixtree
  @elem{
    Computes longest common subsequences between strings.
    The largest performance overheads are due to a boundary between struct definitions and functions on the structures.
  })
(cons kcfa
  @elem{
    Performs 1-CFA on a lambda calculus term that computes @exact|{~$\RktMeta{2*(1+3) = 2*1 + 2*3}$}| via Church numerals.
    The (mutable) binding environment flows throughout functions in the benchmark.
    When this environment crosses a type boundary, it acquires a new proxy.
  })
(cons snake
  @elem{
    Implements the Snake game; the benchmark replays a fixed sequence of moves.
    Modules in this benchmark frequently exchange first-order values, such as lists and integers.
  })
(cons take5
  @elem{
    Manages a card game between AI players.
    These players communicate infrequently, so gradual typing adds relatively little overhead.
  })
(cons acquire
  @elem{
    Simulates a board game via message-passing objects.
    These objects encapsulate the core data structures; few higher-order values cross type boundaries.
  })
(cons tetris
  @elem{
    Replays a pre-recorded game of Tetris.
    Frequent interactions, rather than proxies or expensive runtime checks, are the source of performance overhead.
  })
(cons synth
  @elem{
    Converts a description of notes and drum beats to @tt{WAV} format.
    Modules in the benchmark come from two sources, a music library and an array library.
    The worst overhead occurs when arrays frequently cross type boundaries.
  })
(cons gregor
  @elem{
    Provides tools for manipulating calendar dates.
    The benchmark builds tens of date values and runs unit tests on these values.
  })
(cons (list quadBG quadMB)
  @elem{
    Converts S-expression source code to @tt{PDF} format.
    The two versions of this benchmark differ in their type annotations, but have nearly identical source code.

    The original version, @bm[quadMB], uses type annotations by the original author.
    This version has a high typed/untyped ratio
     because it explicitly compiles types to runtime predicates
     and uses these predicates to eagerly check data invariants.
    In other words, the typed configuration is slower than the untyped configuration because it does more work.

    The second version, @bm[quadBG], uses identical code but weakens types to match the untyped configuration.
    This version is therefore suitable for judging the implementation
     of Typed Racket rather than the user experience of Typed Racket.
    The conference version of this paper included data only for @bm[quadMB].

    @; To give a concrete example of different types, here are the definitions
    @;  for the core @tt{Quad} datatype from both @bm[quadMB] and @bm[quadBG].
    @; @racket[(define-type QuadMB (Pairof Symbol (Listof QuadMB)))]
    @; @racket[(define-type QuadBG (Pairof Symbol (Listof Any)))]
    @; The former is a homogenous, recursive type.
    @; As such, the predicate asserting that a value has type @racket[QuadMB] is a linear-time tree traversal.
    @; The predicate for @racket[QuadBG] runs significantly faster.
  })
]


@; -----------------------------------------------------------------------------
@profile-point{sec:tr:characteristics}
@; @subsection{Static Benchmark Characteristics}

@Figure-ref{fig:bm} tabulates the size and complexity of the benchmark programs.@note{The appendix presents the information in @figure-ref{fig:bm} graphically.}
The lines of code (Untyped LOC) and number of experimental modules (# Mod.) approximate program size.
The type annotations (Annotation LOC) count additional lines in the typed configuration.
These lines are primarily type annotations, but also include type casts and assertions.@note{The benchmarks use more annotations than Typed Racket requires because they give full type signatures for each import. Only imports from untyped modules require annotation.}
Adaptor modules (# Adp.) roughly correspond to the number of user-defined datatypes in each benchmark;
 the next section provides a precise explanation.
Lastly, the boundaries (# Bnd.) and exports (# Exp.) distill each benchmark's graph structure.
Boundaries are import statements from one module to another, excluding imports for runtime or third-party libraries.
An identifier named in such an import statement counts as an export.
For example, the one import statement in @bm[sieve] names nine identifiers.

@figure*["fig:bm" @elem{Static characteristics of the @|GTP| benchmarks}
  @render-benchmarks-table{}
]


@; -----------------------------------------------------------------------------
@section[#:tag "sec:conversion"]{From Programs to Benchmarks}

@(define MIN-ITERS-STR "3")
@(define MAX-ITERS-STR "30")
@(define FREQ-STR "1.40 GHz")
@(define TYPED-BM* (list fsm synth quad))

@string-titlecase[@integer->word[(- (*NUM-BENCHMARKS*) (length TYPED-BM*))]] of the benchmark programs are adaptations of untyped programs.
The other three benchmarks (@bm[fsm], @bm[synth], and @bm[quad]) use most of the type annotations and code from originally-typed programs.
Any differences between the original programs and the benchmarks are due to the following five complications.

First, the addition of types to untyped code occasionally requires type casts or small refactorings.
For example, the expression @racket[(string->number "42")] has the Typed Racket type @racket[(U Complex #f)].
This expression cannot appear in a context expecting an @racket[Integer] without an explicit type cast.
As another example, the @bm[quad] programs call a library function to partition a @racket[(Listof (U A B))] into a @racket[(Listof A)] and a @racket[(Listof B)] using a predicate for values of type @racket[A].
Typed Racket cannot currently prove that values which fail the predicate have type @racket[B], so the @bm[quad] benchmarks replace the call with two filtering passes.

Second, Typed Racket cannot enforce certain types across a type boundary.
For example, the core datatypes in the @bm[synth] benchmark are monomorphic because Typed Racket cannot dynamically enforce parametric polymorphism on instances of an untyped structure.

Third, any contracts present in the untyped programs are represented as type annotations and in-line assertions in the derived benchmarks.
The @bm[acquire] program in particular uses contracts to ensure that certain lists are sorted and have unique elements.
The benchmark enforces these conditions with explicit pre and post-conditions on the relevant functions.

Fourth, each @emph{static import} of an untyped struct type into typed code generates a unique datatype.
Typed modules that share instances of an untyped struct must therefore reference a common static import site.
The benchmarks include additional fixed modules that we call @emph{adaptor modules} to provide this canonical import site; for each module @math{M} in the original program that exports a struct, the benchmark includes an adaptor module that provides type annotations for every identifier exported by @math{M}.
Adaptor modules add a layer of indirection, but this indirection does not add measurable performance overhead.

Fifth, some benchmarks use a different modularization than the original program.
The @bm[kcfa] benchmark is modularized according to comments in the original, single-module program.
The @bm[suffixtree], @bm[synth], and @bm[gregor] benchmarks each have a single file containing all their data structure definitions; the original programs defined these structures in the same module as the functions on the structures.
Lastly, the @bm[quadBG] benchmark has two fewer modules than @bm[quadMB] because it inlines the definitions of two (large) data structures that @bm[quadMB] keeps in separate files.
This inlining does not affect overhead due to gradual typing, but greatly reduces the number of configurations.

