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
The overhead of testing whether large input files were well-formed @bm{Quad}
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

@; - choice of, synth/suffixtree
@; - macro-bending synth/suffixtree



@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:wrapping"]{Duplicate Contracts}
@; -- AKA repeated wrapping

@; functions in zombie
@; objects in forth,fsm(,acquire)
@; classes in dungeon?

@; ENCAPSULATION
@; acquire,take5 vs fsm,forth
@; better to do stateful than functional OO


@; -----------------------------------------------------------------------------
@section[#:tag "sec:devils:library"]{Ecological Contracts}
@; -- AKA library

@; mbta, zordoz
@; dungeon (removed)

@; Need to convert the ecosystem

