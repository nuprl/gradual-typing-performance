#lang scribble/base

@require[
  "benchmark.rkt"
  "common.rkt"
  "typed-racket.rkt"
  "util.rkt"
  (only-in racket/format ~r)
  (only-in gtp-summarize/path-util add-commas)
  (except-in gtp-summarize/lnm-parameters defparam)
]


@profile-point{sec:devils}
@title[#:tag "sec:devils"]{Dissecting Performance Overhead}
@; -- AKA "Four contracts of the apocalypse"
@;        "Devils Contracts"
@;        "Performance Overhead Spectrum"

Our evaluation demonstrates that adding types to a random set of Racket modules can introduce large performance overhead.
This section explains more precisely how such overheads arose in our benchmarks,
 both as inspiration for maintainers of gradual type systems and as anti-patterns for developers.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:frequency"]{High-Frequency Typechecking}
@; -- AKA high-frequency

No matter the cost of a single runtime type check, if the check occurs frequently the program will suffer.
@Figure-ref{fig:devils:stack}, for example, calls the typed function @racket[stack-empty?] one million times from untyped code.
Each call is type-correct, nevertheless Typed Racket validates the argument @racket[stk] against the specification @ctc{Listof A} one million times.
These checks dominate the performance of this example program.

    @figure["fig:devils:stack" "A high-frequency type boundary"
      @(begin
      #reader scribble/comment-reader
      @list[
        @codeblock|{
        #lang typed/racket
        (define-type (Stack A) (Listof A))
        ;; represent stacks as homogenous lists
        (: stack-empty? (All (A) ((Stack A) -> Boolean)))
        (define (stack-empty? stk)
          (null? stk))
        (provide stack-empty?)
        }|
      @tt{================================================================}
        @codeblock|{
        #lang racket

        (require 'stack)
        ;; Create a stack of 20 elements
        (define stk (range 20))
        (for ([i (in-range (expt 10 6))])
          (stack-empty? stk))
        }|])
    ]

Many of our benchmark programs have similar pathological boundaries.
@(let ([highway* '(
        @; -- Source of Truth: `src/traces/`
        (morsecode  821060 372100)
        (quadBG     5 28160 2878 17 8696 339674 323648)
        (sieve      200000000)
        (snake      13801200 4 0 6494400 5245801 17335000 18856600)
        (suffixtree 143989951 17428292 24336 145826484 42941058)
        (synth      110 18288 668 15 15 17 42 445637)
        (tetris     7 82338320 41605597 90270 2063 22534 23191))])
  @elem{
    In @bm[snake], five boundaries are crossed over 5 million times each.
    @; In @bm[tetris], two boundaries are crossed over 40 million times.
    In @bm[suffixtree], two boundaries are crossed over 100 million times.
  })
@; Increasing the size of these benchmarks' input increases the number of crossings.
Each of these boundary-crossings can trigger multiple assertions.
For example, validating the type @type{(Listof A)} from @figure-ref{fig:devils:stack} requires a check for each of the twenty elements in the list.@note{On a related note,
  Racket v6.3 and earlier guarded struct type predicates with a trivial @ctc{Any -> Boolean} chaperone@~cite[tfgnvf-popl-2016].
  Checking @ctc{Any} and @ctc{Boolean} is inexpensive, but removing this contract significantly improved performance in Racket v6.4.}

In lieu of eliminating frequently-crossed type boundaries, we have two suggestions for reducing their cost.
First is to memoize the outcome of successful runtime type checks.
The Hummingbird project implements one such cache as a global table@~cite[rf-pldi-2016]; chaperones for mutable data structures could also store proofs of type-correctness.
@; mutation => invalidate

Second is to delay runtime type checks.
Instead of validating untyped arguments eagerly, typed functions could incrementally check values as typed code traverses them.
Provided typed code accesses the entire value and any type errors report the original boundary, the delayed semantics would be indistinguishable from Typed Racket today.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:types"]{High-Cost Types}
@; -- AKA unexpectedly large

Certain types are computationally expensive to check dynamically.
Immutable lists require a linear number of checks.
Functions require proxies, whose total cost then depends on the number of subsequent calls.
Mutable data structures (hash tables, objects) require a linear number of such proxies.

In general Typed Racket programmers are aware of these costs, but predicting the cost of enforcing a specific type in a specific program is difficult.
One example comes from @bm[quadMB]; the core datatype is a tagged @math{n}-ary tree type.
@;
@;    @racketblock[
@;      (define-type Quad (Pairof Symbol (Listof Quad)))
@;    ]
@;
@(let* ([v "6.4"]
        [tu (typed/untyped-ratio (benchmark-rktd quadMB v))]
        [tu-str (format "~ax" (~r tu #:precision '(= 1)))])
   @elem{
     Heavy use of the predicate for this type led to the @|tu-str| typed/untyped ratio in Racket v6.4.
   })
Incidentally, the programmer who designed this datatype was hoping Typed Racket would improve the program's performance.
The high overhead was a complete surprise.

Similarly, a programmer recently shared the code in @figure-ref{fig:devils:pfds} on the Racket mailing list.
Changing the first line to @code{#lang typed/racket} improves this script's performance
 from @|PFDS-BEFORE| to less than @|PFDS-AFTER| because the @racket[trie] datatype
 happens to be implemented as a hashtable.
Typing the script removes the need to proxy @racket[trie] values.

    @figure["fig:devils:pfds" "Performance pitfall, discovered by John Clements."
      @(begin
      #reader scribble/comment-reader
      @codeblock|{
      #lang racket
      (require pfds/trie) ;; a Typed Racket library

      (define t (trie (list (range 128))))
      (time (bind (range 128) 0 t))
      }|)
    ]

    @; Our @bm[kcfa] benchmark also uses immutable hashtables and pays the
    @;  runtime cost of contracts for mutable data.
    @; We estimate that removing just those hashtable contracts would improve the
    @;  worst-case performance of @bm[kcfa] from 8x to 5x on Racket v6.4.

@; -- future work: cost model for contracts?
@; - profiler
@; - static pmodels

Even if Typed Racket implementors reduce the cost of type checks and proxies,
 programmers would still benefit from a formal measure of runtime type checking.
Such a measure must include at least a model for predicting costs
 and a profiler for determining the actual cost of proxies spread thoughout a program.
Racket's feature-specific profiler may serve as a starting point@~cite[saf-cc-2015].


@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:boundary"]{Dynamic Type Boundaries}
@; -- AKA surprise boundaries

@; Suppose a programmer is eager to avoid frequently-crossed type boundaries and type boundaries for ``expensive'' types.
@; Identifying all the type boundaries in a program is still a significant challenge.

Understanding the type boundary structure of a higher-order program is a challenge in itself.
Every higher-order value that crosses a static type boundary at runtime encapsulates a new, dynamic type boundary.
Finding all type boundaries in a program amounts to building a call graph.

Other language features can futher complicate the task of identifying type boundaries.
For example, a Racket macro can reference any function in scope where the macro was defined.
If these functions are typed and the macro expands in an untyped context, the expansion introduces type boundaries.

This scenario arose in the @bm[synth] benchmark.
One macro in @bm[synth] provided a ``fast'' array iterator that used unsafe operations in an inner loop.
In half of all configurations, these unsafe operations encapsulated a costly type boundary.

These observations call for a profiling tool tailored to @emph{type boundaries} in a gradually typed program.
Such a tool should direct programmers to high-cost boundaries and report how dynamic type boundaries tunnel through static API boundaries.@note{Racket's contract
    profiler addresses the first goal, but not the second@~cite[saf-cc-2015]. Running the profiler on @bm[synth] attributed a high cost to the unsafe operations, a correct but misleading assessment.}


@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:wrapping"]{Layered Proxies}
@; -- AKA repeated wrapping

@; TODO note issue with space-efficiency,
@;      translating suffixtree to use list/vector gave 'outta memory'

  @figure["fig:devils:fsm+forth" @elem{Accumulating proxies in @bm[fsm] and @bm[forth].}
    @(begin
    #reader scribble/comment-reader
    @list[
      @codeblock|{
      #lang typed/racket ;; From the 'fsm' benchmark
      (require (prefix-in P. "population.rkt"))

      (: evolve (P.Population Natural -> Real))
      (define (evolve p iters)
        (cond
          [(zero? iters) (get-payoff p)]
          [else (define p2 (P.match-up* p r))
                (define p3 (P.death-birth p2 s))
                (evolve p3 (- iters 1))]))
      }|
      @tt{================================================================}
      @codeblock|{
      #lang typed/racket ;; From the 'forth' benchmark

      (: eval (Input-Port -> Env))
      (define (eval input)
        (for/fold ([env  : Env    (base-env)])
                  ([line : String (in-lines input)])
          ;; Cycle through commands in `env` until we get
          ;;  a non-#f results from `eval-line`
          (for/first ([c : (Instance Cmd%) (in-list env)])
            (send c eval-line env line))))
      }|])
  ]

Higher-order values that repeatedly flow across type boundaries may accumulate layers of type-checking proxies.
These proxies add levels of indirection and accumulate space.
Collapsing layers of proxies and pruning redundant proxies is an area of active research@~cite[htf-hosc-2010 sw-popl-2010 g-popl-2015].

Racket's chaperones implement a predicate that tells whether the current chaperone subsumes another, arbitrary chaperone.
In many cases, these predicates remove unnecessary proxies, but a few of our benchmarks still experienced pathologies.

For example, the @bm[fsm], @bm[fsmoo], and @bm[forth] benchmarks updated mutable data structures in a loop.
@Figure-ref{fig:devils:fsm+forth} demonstrates the problematic functions in each benchmark.
In @bm[fsm] (on top), the value @racket[p] accumulates one proxy every time it crosses a type boundary; that is, four proxies for each iteration of @racket[evolve].
In @bm[forth] (on bottom), the loop functionally updates an environment @racket[env] of calculator command objects.
Modifying both functions to an imperative style with global state removes the performance overhead, but we consider such refactorings a last resort.

The @bm[zombie] benchmark exhibits similar overhead due to higher-order functions.
For example, the @racket[Posn] datatype in @figure-ref{fig:devils:zombie} is a higher-order function that responds to symbols @code{'x}, @code{'y}, and @code{'move} with a tagged method.
Helper functions like @racket[posn-move] implement a tag-free interface, but calling such functions across a type boundary led exponential performance overhead.
Our benchmark replayed @bm[zombie] on a sequence of 100 commands and recorded a worst-case overhead of 300x on Racket v6.4.
@; zombie input: 100 commands
@; worst case: 21 seconds = 300x overhead

  @figure["fig:devils:zombie" @elem{Adapted from the @bm[zombie] benchmark.}
    @(begin
    #reader scribble/comment-reader
    @codeblock|{
    #lang typed/racket

    (define-type Posn ((U 'x 'y 'move) ->
                       (U (List 'x (-> Natural))
                          (List 'y (-> Natural))
                          (List 'move (-> Natural Natural Posn)))))

    (: posn-move (Posn Natural Natural -> Posn))
    (define (posn-move p x y)
      (define key 'move)
      (define r (p key))
      (if (eq? (first r) key)
        ((second r) x y)
        (error 'key-error)))
    }|)
  ]


@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:library"]{Library Boundaries}
@; -- AKA library. The problem = gradualizing an entire language
@;                             = core language
@;                             = packages, maintaining untyped compat.
@;                             = docs, examples, faqs

Racket libraries are either typed or untyped; there is no middle ground, therefore one class of library clients are forced to communicate over a type boundary.
For instance, our @bm[mbta] and @bm[zordoz] benchmarks rely on an untyped library and consequently had relatively high typed/untyped ratios on Racket v6.2
 (@rnd[@typed/untyped-ratio[@benchmark-rktd[mbta "6.2"]]]x and
  @rnd[@typed/untyped-ratio[@benchmark-rktd[zordoz "6.2"]]]x, respectively).
On the other hand, @bm[lnm] relies on two typed libraries and runs significantly faster fully typed.

One possible solution is for library authors to maintain two versions of each library, one typed and one untyped.
Another is to put @emph{trusted} type signatures on an untyped API or to @emph{unsoundly erase} types from a typed API.
None of these are ideal, and the latter solution of erasing types may lead to security vulnerabilities as demonstrated in @figure-ref{fig:devils:vote}.
If the type signature for @racket[add-votes] is not enforced, clients can supply negative numbers to the function.
A correct untyped version of @racket[add-votes] must assert that its argument is non-negative.

The Racket community is aware of these issues and is actively exploring the design space.
Trusted type annotations are currently the preferred solution, though Typed Racket recently added support for unsafe imports and exports.
In the end, the best solution may be to give more control to library clients.

    @figure["fig:devils:vote" @elem{Erasing types would compromise the invariant of @racket[total-votes].}
      @(begin
      #reader scribble/comment-reader

      @;   (define total-votes 0) ;; Invariant: `0 <= total-votes`
      @;
      @;   ;; Add `n` votes to the global variable `total-votes`
      @;   (define (add-votes n)
      @;     (unless (exact-nonnegative-integer? n)
      @;       (error "Number of votes must be a Natural number"))
      @;     (set! total-votes (+ total-votes n)))

      @codeblock|{
        #lang typed/racket

        (define total-votes : Natural 0)

        (: add-votes (Natural -> Void))
        (define (add-votes n)
          (set! total-votes (+ total-votes n)))

        (provide add-votes)
      }|)
    ]

