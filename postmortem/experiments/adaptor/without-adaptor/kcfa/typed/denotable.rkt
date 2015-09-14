#lang typed/racket/base

;; Denotable values and stores to hold them.
;; A denotable is a set of values
;; (A value is a closure)

(require
  benchmark-util
  racket/set
)
(define-type Exp (U exp Ref Lam Call))
(define-type Label Symbol)
(define-type Var Symbol)
(require/typed/check "structs.rkt"
  [#:struct Stx ([label : Label])]
  [#:struct (exp Stx) ()]
  [#:struct (Ref exp) ([var : Var])]
  [#:struct (Lam exp) ([formals : (Listof Var)] [call : Exp])]
  [#:struct (Call Stx) ([fun : Exp] [args : (Listof Exp)])]
)
(define-type BEnv (HashTable Var Addr))
(define-type Addr Binding)
(define-type Time (Listof Label))
(require/typed/check "benv.rkt"
  [#:struct Closure
    ([lam : Lam]
     [benv : BEnv])]
  [#:struct Binding
    ([var : Var]
     [time : Time])]
  (empty-benv BEnv)
  (benv-lookup (-> BEnv Var Addr))
  (benv-extend (-> BEnv Var Addr BEnv))
  (benv-extend* (-> BEnv (Listof Var) (Listof Addr) BEnv))
)
(require/typed/check "time.rkt"
  [time-zero Time]
  [k (Parameterof Natural)]
  [tick (-> Stx Time Time)]
  [alloc (-> Time (-> Var Addr))]
)
(define-type Value Closure)

;; -----------------------------------------------------------------------------

(provide
  (struct-out State)
  d-bot
  d-join
  empty-store
  store-lookup
  store-update
  store-update*
  store-join
)

;; =============================================================================

;; -- private
(define-type Denotable (Setof Value))
(define-type Store (HashTable Addr Denotable))

;; -- structs

(struct State
 ([call : Exp]
  [benv : BEnv]
  [store : Store]
  [time : Time]))

;; -- public

(: d-bot Denotable)
(define d-bot (set))

(: d-join (-> Denotable Denotable Denotable))
(define d-join set-union)

(: empty-store Store)
(define empty-store (make-immutable-hasheq '()))

(: store-lookup (-> Store Addr Denotable))
(define (store-lookup s a)
  (hash-ref s a (lambda () d-bot)))

(: store-update (-> Store Addr Denotable Store))
(define (store-update store addr value)
  (: update-lam (-> Denotable Denotable))
  (define (update-lam d) (d-join d value))
  (hash-update store addr update-lam (lambda () d-bot)))

(: store-update* (-> Store (Listof Addr) (Listof Denotable) Store))
(define (store-update* s as vs)
  (for/fold ([store s])
    ([a (in-list as)]
     [v (in-list vs)])
    (store-update store a v)))

(: store-join (-> Store Store Store))
(define (store-join s1 s2)
  (for/fold ([new-store s1])
    ([(k v) (in-hash s2)])
    (store-update new-store k v)))

