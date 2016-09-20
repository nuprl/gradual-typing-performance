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

Our evaluation demonstrates that adding types to an arbitrarily chosen subset of Racket modules in a program can introduce large performance overhead.
Here we explain with a few examples how such overheads may arise, both as inspiration for maintainers of gradual type systems and as anti-patterns for developers.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:frequency"]{High-Frequency Typechecking}
@; -- AKA high-frequency

No matter the cost of a single runtime type check, if the check occurs frequently then the program will suffer.
@Figure-ref{fig:devils:stack}, for example, calls the typed function @racket[stack-empty?] one million times from untyped code.
Each call is type-correct; nevertheless, Typed Racket validates the argument @racket[stk] against the specification @ctc{Listof A} one million times.
These checks dominate the performance of this example program, simply because many values flow across the module boundary.

    @figure["fig:devils:stack" "A high-frequency type boundary"
      @(begin
      #reader scribble/comment-reader
      @list[
        @codeblock|{
        #lang typed/racket
        (provide stack-empty?)

        (define-type Stack (Listof Integer))
        ;; represent stacks as homogenous lists

        (: stack-empty? (Stack -> Boolean))
        (define (stack-empty? stk)
          (null? stk))
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

High-frequency module boundaries are common in our benchmarks.
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
    To give an extreme example, over six million values flow across four separate boundaries in @bm[snake].
    In @bm[suffixtree], over one hundred million values flow across two boundaries.
    @; In @bm[tetris], two boundaries are crossed over 40 million times.
  })
@; Increasing the size of these benchmarks' input increases the number of crossings.
When these module boundaries are type boundaries, the benchmarks suffer considerable overhead; their respective worst cases are 32x and 28x on Racket v6.4.
  @; V    snake suffixtree
  @; 6.3     70        174
  @; 6.4     32         28
  @; @note{Racket v6.3 guards struct type predicates with a trivial @ctc{Any -> Boolean} chaperone@~cite[tfgnvf-popl-2016].
  @;   Checking @ctc{Any} and @ctc{Boolean} by themselves is inexpensive, but removing this contract significantly improves the performance of our benchmarks in Racket v6.4.}



@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:types"]{High-Cost Types}
@; -- AKA unexpectedly large

Certain types require computationally expensive runtime checks in Typed Racket.
Immutable lists require a linear number of checks.
Functions require proxies, whose total cost then depends on the number of subsequent calls.
Mutable data structures (hash tables, objects) are the worst of both worlds, as they require a linear number of such proxies.

In general Typed Racket programmers are aware of these costs, but predicting the cost of enforcing a specific type in a specific program is difficult.
One example comes from @bm[quadMB], in which the core datatype is a tagged @math{n}-ary tree type.
@;
@;    @racketblock[
@;      (define-type Quad (Pairof Symbol (Listof Quad)))
@;    ]
@;
@(let* ([v "6.4"]
        [tu (typed/untyped-ratio (benchmark-rktd quadMB v))]
        [tu-str (format "~ax" (~r tu #:precision '(= 1)))])
   @elem{
     Heavy use of the predicate for this type causes the @|tu-str| typed/untyped ratio in Racket v6.4.
    @; @note{Incidentally,
    @;   the programmer who designed this datatype was hoping Typed Racket would improve the application's performance.
    @;   The high overhead was a complete surprise.}
   })
Another example is the @bm[kcfa] benchmark, in which hashtable types account for a 3x slowdown in the worst configurations.

Similarly, the Racket script in @figure-ref{fig:devils:pfds} executes in approximately @|PFDS-BEFORE-str|.
Changing its language to @code{#lang typed/racket} improves its performance to under @|PFDS-AFTER| by removing a type boundary to the @library{trie} library.@note{A programmer recently shared this code on the Racket mailing list, see the appendix.}
The drastic improvement is just because the @emph{undocumented} type representing @tt{trie} structures is expensive to enforce dynamically.

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


@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:boundary"]{Complex Type Boundaries}
@; -- AKA surprise boundaries

Higher-order values and metaprogramming features introduce fine-grained and dynamic type boundaries.
For example, every proxy that enforces a type specification is a dynamically-generated type boundary.
Such boundaries lead to greater overhead than one would predict based on static module dependencies.

The @bm[synth] benchmark illustrates one problematic use of metaprogramming.
One module in @bm[synth] exports a macro that expands to a low-level iteration construct.
The expanded code introduces a reference to a server module, whether or not the macro client statically imports the server.
Thus, when the server and client are separated by a type boundary, the macro @emph{implicitly} puts a type boundary in the expanded looping code.
In order to predict such costs, a programmer must track the namespaces available to every macro.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:wrapping"]{Layered Proxies}
@; -- AKA repeated wrapping

@; TODO note issue with space-efficiency,
@;      translating suffixtree to use list/vector gave 'outta memory'

  @figure["fig:devils:fsm" @elem{Accumulating proxies in @bm[fsm].}
    @(begin
    #reader scribble/comment-reader
      @codeblock|{
      #lang typed/racket
      (require (prefix-in P. "population.rkt"))

      (: evolve (P.Population Natural -> Real))
      (define (evolve p iters)
        (cond
          [(zero? iters) (get-payoff p)]
          [else (define p2 (P.match-up* p r))
                (define p3 (P.death-birth p2 s))
                (evolve p3 (- iters 1))]))
      }|)
  ]

  @figure["fig:devils:forth" @elem{Accumulating proxies in @bm[forth].}
    @(begin
    #reader scribble/comment-reader
      @codeblock|{
      #lang typed/racket

      (require (prefix-in C. "command.rkt"))

      (: eval (Input-Port -> Env))
      (define (eval input)
        (for/fold ([env  : C.Env  (base-env)])
                  ([line : String (in-lines input)])
          ;; Cycle through commands in `env` until we get
          ;;  a non-#f results from `eval-line`
          (for/first ([c : (Instance C.Cmd%) (in-list env)])
            (send c eval-line env line))))
      }|)
  ]

Higher-order values that repeatedly flow across type boundaries may accumulate layers of type-checking proxies.
These proxies add indirection and space overhead.
Collapsing layers of proxies and pruning redundant proxies is an area of active research@~cite[htf-hosc-2010 sw-popl-2010 g-popl-2015].

Racket's chaperones implement a predicate that tells whether the current chaperone subsumes another, arbitrary chaperone.
In many cases, these predicates remove unnecessary proxies, but a few of our benchmarks still suffer from redundant proxies.

For example, the @bm[fsm], @bm[fsmoo], and @bm[forth] benchmarks update mutable data structures in a loop.
@Figure-ref{fig:devils:fsm fig:devils:forth} demonstrate the problematic functions in each benchmark.
In @bm[fsm], the value @racket[p] accumulates one proxy every time it crosses a type boundary; that is, four proxies for each iteration of @racket[evolve].
The worst case overhead for this benchmark is 235x on Racket v6.4.
In @bm[forth], the loop functionally updates an environment @racket[env] of calculator command objects;
 its worst-case overhead is 27x on Racket v6.4.@note{Modifying both functions to use an imperative message-passing style removes the performance overhead, though we consider such refactorings a last resort.}

The @bm[zombie] benchmark exhibits similar overhead due to higher-order functions.
For example, the @racket[Posn] datatype in @figure-ref{fig:devils:zombie} is a higher-order function that responds to symbols @code{'x}, @code{'y}, and @code{'move} with a tagged method.
Helper functions like @racket[posn-move] implement a tag-free interface, but calling such functions across a type boundary leads to layered proxies.
Our benchmark replays @bm[zombie] on a sequence of 100 commands and reports a worst-case overhead of 300x on Racket v6.4.
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

Racket libraries are either typed or untyped; there is no middle ground, therefore one class of library clients are forced to communicate across a type boundary.
For instance, our @bm[mbta] and @bm[zordoz] benchmarks rely on untyped libraries and consequently have relatively high typed/untyped ratios on Racket v6.2
 (@rnd[@typed/untyped-ratio[@benchmark-rktd[mbta "6.2"]]]x and
  @rnd[@typed/untyped-ratio[@benchmark-rktd[zordoz "6.2"]]]x, respectively).
In contrast, the @bm[lnm] benchmark relies on two typed libraries and runs significantly faster when fully typed.

@; @; DIE ... it's all analysis
@; Ideally, clients should be able to choose between typed and untyped versions of any library, but it is unclear how to provide this choice.
@; Devoted library authors could maintain two separate versions of their code.
@; Two other solutions are to put @emph{trusted} type signatures on an untyped API or to @emph{unsoundly erase} types from a typed API.
@; Neither of these are ideal, and the latter solution of erasing types may lead to security vulnerabilities as demonstrated in @figure-ref{fig:devils:vote}.
@; If the type signature for @racket[add-votes] is not enforced, clients can supply negative numbers to the function.
@; A correct untyped version of @racket[add-votes] must assert that its argument is non-negative.
@;
@; The Racket community is aware of these issues and is actively exploring the design space.
@; Trusted type annotations are currently the preferred solution, though Typed Racket recently added support for unsafe imports and exports.
@; In the end, the best solution may be to give more control to library clients.

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

