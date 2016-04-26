#lang scribble/base

@require[
  "common.rkt"
  "typed-racket.rkt"
 (except-in gtp-summarize/lnm-parameters defparam)
]


@profile-point{sec:devils}
@title[#:tag "sec:devils"]{The Devil's Contracts}
@; -- AKA "Four contracts of the apocalypse"

@; TODO better intro, urgently
Many of our high-overhead benchmarks exhibit similar pathologies.
Here are some of the most interesting.

@; Of course contracts and boundary structure make it slow.
@; But some are worse than others. Or at least more interesting.

We hope that language designers manage to reduce the overhead caused by
 these pathological cases.
But in the worst case, these may serve as anti-patterns for performance-minded
 applications.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:frequency"]{Highway Contracts}
@; -- AKA high-frequency

Some module boundaries are crossed extremely often in a benchmark execution.
We deem these boundaries @emph{highways}.
@(let ([highway* '((morsecode  821060 372100)
                   (quadBG     5 28160 2878 17 8696 339674 323648)
                   (sieve      200000000)
                   (snake      13801200 4 0 6494400 5245801 17335000 18856600)
                   (suffixtree 143989951 17428292 24336 145826484 42941058)
                   (synth      110 18288 668 15 15 17 42 445637)
                   (tetris     7 82338320 41605597 90270 2063 22534 23191))])
  @; See `src/traces` folder for details on boundaries
  @elem{
    For example, @bm{snake} has five boundaries that are crossed over
     5 million times each in our benchmark; @bm{suffixtree} has three boundaries
     that are crossed over 100 million times.
  })
When a highway is also a type boundary, any contracts guarding it
 will cause significant performance overhead.

One common scenario is demonstrated by the following stack data structure
 and its untyped client.

@(begin
#reader scribble/comment-reader
@codeblock|{
#lang racket ;; -- enclosing module is untyped

;; -- typed submodule
(module stack typed/racket
  (define-type (Stack A) (Listof A))
  ;; -- represent stacks as homogenous lists
  (: stack-empty? (All (A) ((Stack A) -> Boolean)))
  (define (stack-empty? stk)
    (null? stk))
  (provide stack-empty?))
;; -- End of typed submodule
(require 'stack)
;; -- Create a stack of 20 elements
(define stk (range 20))
(for ([_i (in-range (expt 10 6))])
  ;; -- Repeatedly cross a type boundary
  (stack-empty? stk))
}|)

Each of the million boundary-crossings in this program triggers a contract
 check on the stack @tt{stk} to ensure it is a homogenous list.
This check requires walking the list and validating each of its 20
 elements.@note{In general, contracts on list take linear time in the size of their input.}
Although one such traversal takes a fraction of a second and thousands are
 hardly noticable, the contract checks in the above program dominate its
 running time.

We suggest two potential solutions to this pathology.
First is to memoize the outcome of contract assertions.
After the untyped value @racket[stk] is validated by the
 @exact{$\ctc{\RktMeta{(Stack A)}}$} contract for the first time,
 the value should be given a certificate that obviates the need for future checks.
Mutating @racket[stk] should be allowed and should invalidate the certificate.

Our second suggestion is to check contracts @emph{by-need}; in other words,
 only when a typed function accesses part of the untyped value @racket[stk].
If applied to this program, by-need contracts would eliminate all traversals
 because @racket[stack-empty?] never reads or writes to its argument.
What remains to be seen is whether implementing this behavior would
 typically improve performance in realistic programs.

@; any->bool
A similar pathology regarding the contracts generated for user-defined
 structure types was fixed between Racket v6.3 and v6.4.
When a programmer creates a struct
 like the following, Racket generates a predicate and accessor functions.

@(begin
#reader scribble/comment-reader
@codeblock|{
#lang typed/racket
(struct Point ([x : Real] [y : Real]))
}|)

Naturally, the accessors @racket[Point-x] and @racket[Point-y] require
 their argument to be a @racket[Point] value and should be protected by a contract.
But the predicate @racket[Point?] has type @racket[(Any -> Boolean)].
Enforcing this type with a higher-order contract is unnecessary:
 any value will pass the @racket[Any] contract and @racket[Point?] is guaranteed
 by construction to return a @racket[Boolean] value.
The generated contract also executes fairly quickly; nevertheless, we
 found that checking these predicate contracts accounted for up to
 30% of some configurations @emph{total} running time@~cite[tfgnvf-popl-2016].

This overhead was due to extremely frequent checks.
As @racket[Point?] is the only way to identify values of type @racket[Point?],
 calls to functions like @racket[Point-x] trigger calls to @racket[Point?] as
 part of their own contracts.
Removing these predicate contracts therefore caused a significant part of the
 performance improvement between versions 6.3 and 6.4.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:types"]{Iceberg Contracts}
@; -- AKA unexpectedly large

When an apparently simple type @exact|{$\RktMeta{T}$}| generates a large or
 unexpectedly slow contract @exact|{$\ctc{\RktMeta{T}}$}|,
 we call @exact|{$\ctc{\RktMeta{T}}$}| an @emph{iceberg contract}.
One example of iceberg contracts are the types used in @bm{quadMB}, for instance:

@racketblock[
  (define-type Quad (Pairof Symbol (Listof Quad)))
]

 generates a recursive contract over @math{n}-ary trees.
@(let* ([tu* (for/list ([v (*RKT-VERSIONS*)]) (typed/untyped-ratio 'quadMB v))]
        [tu-lo (add-commas (rnd (apply min tu*)))]
        [tu-hi (add-commas (rnd (apply max tu*)))])
   @elem{
     These tree types caused typed/untyped ratios ranging from
      @id[tu-lo]x to @id[tu-hi]x across versions of @bm{quadMB}.
   })
Incidentally, the developer who created these types was hoping Typed Racket would
 improve the performance of a typesetting system.
The overhead of testing whether large input files were well-formed @racket[Quad]
 data came as a surprise.

Another Typed Racket user recently published the following untyped script to
 the Racket mailing list.
Changing the @exact{$\RktMeta{\#lang}$} line to @racket[typed/racket]
 improves performance from 10 seconds to 1 millisecond.

@(begin
#reader scribble/comment-reader
@codeblock|{
#lang racket
(require pfds/trie) ;; -- a Typed Racket library

(define t (trie (list (range 128))))
(define u (time (bind (range 128) 0 t)))
}|)

The underlying issue is quite subtle: it happens that the @racket[trie] library
 uses an immutable hashtable as its core datatype but Typed Racket can
 only generate contracts for @emph{mutable} hashtables.
Therefore trie values are wrapped in a contract that is both expensive to install
 and adds an indirection layer to every subsequent operation---all this to
 duplicate the guarantee that an immutable value is never mutated.

Our @bm{kcfa} benchmark also uses immutable hashtables and pays the
 run-time cost of contracts for mutable data.
Removing just those hashtable contracts would improve the worst-case
 performance of @bm{kcfa} from 8x to 5x on Racket v6.4.

@; future work: cost model for contracts?
These bottlenecks due to type-generated contracts spell out a need
 for a user-facing cost model of enforcing type soundness.
Even if language designers can remove most of the overhead, users of
 gradual type systems would benefit from tools to statically approximate the runtime
 cost of enforcing specific types and profilers to dynamically attribute
 runtime overhead to specific types or values.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:boundary"]{Tunnelling Contracts}
@; -- AKA surprise boundaries

Typed Racket's math library is known to cause performance overhead when used
 in untyped programs.
Part of this library is included in our @bm{synth} benchmark, whose main
 functionality is to build a short musical piece.
As expected, the @bm{synth} configuration where all math library modules
 are typed and all music-generating modules are untyped exhibits large performance
 overhead (@configuration->overhead['synth "6.4" "1111000000"] on v6.4).

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
Ironically, the macro was designed as a fast, low-level array iterator;
 the optimization backfired in untyped code by putting a type boundary
 into the hot part of the loop.

This phenomena of functions tunnelling through a macro and introducing
 a type boundary after expansion is unique to languages with syntax
 extensions, but it highlights a general issue that the graph structure
 of programs is often complex.
Even small programs like @bm{synth} have surprising static boundaries.
Large programs, or programs with dynamically introduced boundaries will
 face similar issues---the programmer seeking to address a performance issue
 first needs to learn about all high-overhead type boundaries.
Therefore we use the term @emph{tunnelling contracts} to denote the contracts
 on type boundaries that the programmer was unaware of.
An interesting tooling challenge is to identify high-overhead
 type boundaries and suggest ways to either bypass or remove them.
@; rly need that last sentence?


@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:wrapping"]{Duplicate Contracts}
@; -- AKA repeated wrapping

@; TODO Be positive, don't hurt Robby's feelings

A @exact{na\"ive} implementation of higher-order contracts will wrap
 function and mutable values with a contract each time the values cross a
 type boundary, even if the new contract is redundant given existing contracts
 attached to the value.
This issue is well known and there are many published techniques for identifying
 redundant contracts@~cite[htf-hosc-2010 sw-popl-2010 g-popl-2015].
Racket implements a predicate-based technique: for each class of contracts
 there is a binary predicate that decides whether a contract in the class
 subsumes another, arbitrary contract.
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

(define id (λ ([x : (Instance C%)]) x))
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

Similar issues arose in the @bm{fsm} and @bm{forth} benchmarks.
Both versions of @bm{fsm} functionally update a value @racket[p]
 of type @racket[Population] in their top-level loop.
Whether @racket[Population] was implemented as a vector (in @bm{fsm})
 or an object (in @bm{fsmoo}), it was wrapped in two new contracts each time it
 crossed type boundaries via @racket[match-up*] and @racket[death-birth].

@(begin
#reader scribble/comment-reader
@codeblock|{
#lang typed/racket
(: evolve (-> Population Natural Real))
(define (evolve p iters)
  (cond
    [(zero? iters) (get-payoff p)]
    [else (define p2 (match-up* p r))
          (define p3 (death-birth p2 s))
          (evolve p3 (- iters 1))]))
}|)

The @bm{forth} benchmark builds a environment @racket[env] of interpreter
 commands as it reads a file of definitions and statements.
Each command @racket[c] in @racket[env] is an object that produces
 a new environment given the current environment and a line of input.

@(begin
#reader scribble/comment-reader
@codeblock|{
#lang typed/racket
(define (eval (input : Input-Port)) : Env
  (for/fold ([env  : Env    (base-env)])
            ([line : String (in-lines input)])
    ;; Cycle through commands in `env` until we get
    ;;  a non-#f results from `eval-line`
    (for/first ([c : (Instance Cmd%) (in-list env)])
      (send c eval-line env line))))
}|)

Threading @racket[env] through each evaluation step lets commands extend or
 modify the environment, but it also adds a layer of contracts to @emph{each}
 command for every line in the input stream.
On the other hand, implementing @bm{forth} with a global environment that
 is updated statefully removes nearly all performance overhead.

Lastly, the @bm{zombie} benchmark implements a functional encoding of objects
 that is prone to accumulate contracts.
As a minimal example, the following is a @bm{zombie}-encoded stream object
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

In other words, streams are functions from method names to methods.
The one complication is that we use pairs in the codomain to distinguish
 the function types representing methods.
It is therefore useful to provide ``getter'' functions for each method:

@(begin
#reader scribble/comment-reader
@codeblock|{
#lang typed/racket
(define (stream-nxt (s : Stream)) : (-> Stream)
  (define key 'nxt)
  (define r (s key))
  (if (eq? (car r) key)
    (cdr r)
    (error 'key-error)))
}|)

however, untyped clients using these accessor methods now repeatedly send
 higher-order functions across a type boundary.
This is the source of overhead in @bm{zombie}.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:library"]{Ecological Contracts}
@; -- AKA library

MBTA Zordoz just got worse.
Conversely lnm got better.
Because benchmarks depended on libraries that were untyped and typed, respectively.
Part of the Racket ecosystem.

@; mbta, zordoz
@; dungeon (removed)

How to migrate code / generate typed.
More interesting: generate docs \& stackoverflow.

@; Need to convert the ecosystem

