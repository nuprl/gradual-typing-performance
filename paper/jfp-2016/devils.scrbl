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

    @figure["fig:devils:stack" "High-Frequency Type Boundary"
      @(begin
      #reader scribble/comment-reader
      @codeblock|{
      #lang typed/racket
      (define-type (Stack A) (Listof A))
      ;; represent stacks as homogenous lists
      (: stack-empty? (All (A) ((Stack A) -> Boolean)))
      (define (stack-empty? stk)
        (null? stk))
      (provide stack-empty?)
      }|
      @tt{--------------------------------------------------------------------------------}
      @codeblock|{
      #lang racket

      (require 'stack)
      ;; Create a stack of 20 elements
      (define stk (range 20))
      (for ([i (in-range (expt 10 6))])
        (stack-empty? stk))
      }|)
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

    @racketblock[
      (define-type Quad (Pairof Symbol (Listof Quad)))
    ]


@(let* ([v "6.4"]
        [tu (typed/untyped-ratio (benchmark-rktd quadMB v))]
        [tu-str (format "~ax" (~r tu #:precision '(= 2)))])
   @elem{
     Heavy use of the predicate for this type led to the @|tu-str| typed/untyped ratio in Racket v6.4.
   })
Incidentally, the programmer who designed the @racket[Quad] type was hoping Typed Racket would improve the program's performance.
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
Racket's feature-specific profiler can serve as a starting point@~cite[saf-cc-2015].


@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:boundary"]{Dynamic Type Boundaries}
@; -- AKA surprise boundaries

@; wait, how does this support the thesis "how high overheads arise" ?

Suppose a programmer is eager to avoid frequently-crossed type boundaries and type boundaries for ``expensive'' types.
Identifying all the type boundaries in a program is still a significant challenge.
In a higher-order language, static module boundaries are only a subset of type boundaries.
Every chaperoned value is a dynamically-allocated type boundary.
Listing all the type boundaries in a program amounts to building a call graph.

Other language features can futher complicate the task of finding type boundaries.
For example, a Racket macro can encapsulate calls to any function in scope where the macro was defined.
If these functions are typed and the macro expands into untyped code, the expansion introduces type boundaries.

This scenario arose in the @bm[synth] benchmark.
One macro in @bm[synth] provided a ``fast'' array iterator that used unsafe operations in an inner loop.
Therefore half of all configurations suffered from a type boundary in this loop.

@; Source of truth: `src/synth-profile*.txt`
@(let* ([total-runtime      5144]
        [contract-runtime   1975]
        [c:Array-unsafe-proc  824]
        [c:Array3             450.5]
        [c:next-indexes!      418]
        [c:unsafe-build-array 239.5]
        [c:build-array        33]
        [c:Array-shape        10]
        [surprise-runtime (+ c:Array-unsafe-proc
                             c:next-indexes!
                             c:unsafe-build-array)]
        [surprise-percent (rnd (* 100 (abs (/ (- surprise-runtime total-runtime) total-runtime))))]
        [surprise-overhead (string-append (rnd (/ surprise-runtime total-runtime)) "x")])
  @elem{
    Applying Racket's contract profiler@~cite[saf-cc-2015] to this
     typed-math / untyped-music configuration revealed an unexpected pathology:
     @id[surprise-percent]% of the total runtime was spent in contracts
     generated by @emph{private functions} from the math library.
    Clients are unable to call these functions, nonetheless the functions
     crossed a type boundary and ultimately caused most of the configuration's
     performance overhead.
  })

What happened was that a macro definition in the math library captured references
 to the private functions.
Expanding the macro in untyped code introduced the new and unexpected type
 boundaries.
Ironically, the macro was designed as a fast, low-level array iterator
The optimization backfired in untyped code by inserting a type boundary
 into the hottest part of the loop.

This phenomena of functions tunnelling through a macro and introducing
 a type boundary after expansion is unique to languages with syntax
 extensions, but it highlights a general issue that the graph structure
 of programs is often complex.
Even small programs like @bm[synth] have surprising boundaries.
Large programs, or programs with dynamically introduced boundaries will face similar issues.
We use the term @emph{tunnelling contracts} to denote the contracts
 on type boundaries that the programmer was unaware of.
Going forward, we propose a @emph{boundary profiler} that identifies
 high-overhead type boundaries and suggest ways to either bypass or remove them.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:wrapping"]{Accumulated Proxies}
@; -- AKA repeated wrapping

@; TODO note issue with space-efficiency,
@;      translating suffixtree to use list/vector gave 'outta memory'

A @exact{na\"ive} implementation of higher-order contracts will wrap
 function and mutable values with a contract each time the values cross a
 type boundary, even if the new contract is redundant given existing contracts
 attached to the value.
This issue is well known and there are many published techniques for identifying
 redundant contracts@~cite[htf-hosc-2010 sw-popl-2010 g-popl-2015].
Racket implements a predicate-based technique: for each class of contracts
 there is a binary predicate that decides whether a contract in the class
 subsumes another, arbitrary contract.
   @;---similar to the @tt{equals} predicate
   @;packaged with every Java and Scala object.
Nonetheless, we found a few pathologies due to repeated wrapping
 and therefore include them here as test cases for future work.

Our first example is an identity function on instances of a
 class type @racket[C%].
The details of @racket[C%] are unimportant except that it needs a method
 that accepts objects of class @racket[C%].
@(begin
#reader scribble/comment-reader
@codeblock|{
#lang typed/racket

(define-type C%
  (Class
    (equals (-> (Instance C%) (Instance C%) Boolean))))

(: id (-> (Instance C%) (Instance C%)))
(define (id x)
  x)
}|)

Calling @racket[(id obj)] in untyped code wraps @racket[obj] with two
 contracts: the first ensures that the untyped value @racket[obj] has
 the type @racket[(Instance C%)] and the second guarantees that after @racket[obj]
 is returned to untyped code it can never be mutated to have a type other
 than @racket[(Instance C%)].
Installing a contract is necessary the first time an untyped object crosses
 into typed code, but on Racket v6.3 and earlier every call to
 @racket[id] wraps @racket[obj] in two new contracts.
On Racket v6.4 the issue is fixed.

Similar issues arose in the @bm[fsm] and @bm[forth] benchmarks.
Both versions of @bm[fsm] functionally update a value @racket[p]
 of type @racket[Population] in their top-level loop.
Whether @racket[Population] was implemented as a vector (in @bm[fsm])
 or an object (in @bm[fsmoo]), it was wrapped in two new contracts each time it
 crossed type boundaries via @racket[match-up*] and @racket[death-birth].

@(begin
#reader scribble/comment-reader
@codeblock|{
#lang typed/racket ;; From the 'fsm' benchmark
(require (only-in "population.rkt" match-up* death-birth))

(: evolve (Population Natural -> Real))
(define (evolve p iters)
  (cond
    [(zero? iters) (get-payoff p)]
    [else (define p2 (match-up* p r))
          (define p3 (death-birth p2 s))
          (evolve p3 (- iters 1))]))
}|)

The @bm[forth] benchmark builds a environment @racket[env] of interpreter
 commands as it reads definitions and statements.
Each command @racket[c] in @racket[env] is an object implementing a method
 @racket[eval-line] for updating the current @racket[env].

@(begin
#reader scribble/comment-reader
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
}|)

Threading @racket[env] through each evaluation step lets commands extend or
 modify the environment, but it also adds a layer of contracts to every
 command for every line in the input stream.
On the other hand, implementing @bm[forth] with a global environment that
 is updated statefully removes nearly all gradual typing overhead.

Lastly, the @bm[zombie] benchmark implements a functional encoding of objects
 that is prone to accumulate contracts.
As a minimal example, the following is a @bm[zombie]-encoded stream object
 with methods @tt{obs} and @tt{nxt}, for observing the current value of
 the stream and advancing its state.

@(begin
#reader scribble/comment-reader
@codeblock|{
#lang typed/racket
(define-type Stream
 ((U 'obs 'nxt)
  ->
  (U (Pairof 'obs (-> Natural))
     (Pairof 'nxt (-> Stream)))))
}|)

In other words, streams are essentially functions from method names to methods.
We need pairs in the codomain to tag the different zero-arity alternatives in
 the union.
Another helper function can deal with the untagging,

@(begin
#reader scribble/comment-reader
@codeblock|{
#lang typed/racket
(: stream-nxt (Stream -> Stream))
(define (stream-nxt s)
  (define key 'nxt)
  (define r (s key))
  (if (eq? (car r) key)
    (cdr r)
    (error 'key-error)))
}|)

but untyped clients using this accessor send
 higher-order functions across a type boundary, resulting in higher-order contract wrappers.
This is the source of overhead in @bm[zombie].


@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:library"]{Software Ecosystem Constraints}
@; -- AKA library. The problem = gradualizing an entire language
@;                             = core language
@;                             = packages, maintaining untyped compat.
@;                             = docs, examples, faqs

Many of our benchmarks are self-contained, but others
 depend on libraries within the Racket ecosystem.
With the exception of core Racket libraries trusted by the typechecker,
 these libraries are @emph{either} typed or untyped.
Hence some clients are forced to communicate through a type boundary.

Our @bm[mbta] and @bm[zordoz] benchmarks rely on untyped libraries,
 so they have relatively high typed/untyped ratios
 (@rnd[@typed/untyped-ratio[@benchmark-rktd[mbta "6.2"]]]x and
  @rnd[@typed/untyped-ratio[@benchmark-rktd[zordoz "6.2"]]]x
  on v6.2, respectively).
For the same reason, the @math{k=1} plots for these benchmarks are similar
 to the @math{k=0} plots, as typing additional modules is likely to introduce
 a type boundary with the library.
Conversely, @bm[lnm] uses two typed libraries.
Removing the type boundaries to these libraries improves performance
 to at best @min-overhead[@benchmark-rktd[lnm "6.2"]] relative to the untyped runtime on Racket v6.2.

The obvious fix is to provide typed and untyped versions of each library,
 but this replaces the performance overhead with software maintenance overhead.
For instance, suppose there is an untyped library for managing elections that
 includes a function for adding votes to a global tally:

@(begin
#reader scribble/comment-reader
@codeblock|{
  (define total-votes 0) ;; Invariant: `0 <= total-votes`

  ;; Add `n` votes to the global variable `total-votes`
  (define (add-votes n)
    (unless (exact-nonnegative-integer? n)
      (error "Number of votes must be a Natural number"))
    (set! total-votes (+ total-votes n)))
}|)

The function depends on an assertion to guarantee its argument is a natural number.
If this invariant is not checked, the value of @racket[total-votes] may become
 negative and trigger an error elsewhere in the code.
Of course, a typed version of the same function can replace the dynamic
 assertion with a type declaration:

@(begin
#reader scribble/comment-reader
@codeblock|{
  (define total-votes : Natural 0)

  (: add-votes (Natural -> Void))
  (define (add-votes n)
    (set! total-votes (+ total-votes n)))
}|)

Type annotations aside, we now have two different versions of the same function.
Just erasing types in the second version will @emph{not} produce a safe untyped program.
The difference is small, but these small incompatibilities are common and
 impose an undue maintenance cost of developers using current software management
 tools.

@; TODO doesn't fit
@;On a final, related note, adding types to an untyped program is currently only the first
@; step in converting the program to Typed Racket.
@;The program's documentation also needs to be converted to use and reflect the
@; type annotations.
@; -- also migrate FAQs, examples
