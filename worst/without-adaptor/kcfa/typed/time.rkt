#lang typed/racket/base

(require
 benchmark-util
)
(require/typed/check "structs.rkt"
  [#:struct Stx ([label : Label])]
  [#:struct (exp Stx) ()]
  [#:struct (Ref exp) ([var : Var])]
  [#:struct (Lam exp) ([formals : (Listof Var)] [call : Exp])]
  [#:struct (Call Stx) ([fun : Exp] [args : (Listof Exp)])]
)

(define-type Exp (U exp Ref Lam Call))
(define-type Label Symbol)
(define-type Var Symbol)

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
(define-type BEnv (HashTable Var Addr))
(define-type Addr Binding)
(define-type Time (Listof Label))

;; ---

(provide
  time-zero
  k
  tick
  alloc
)

;; =============================================================================

;; ---
(define-type Value Closure)

(: take* (All (A) (-> (Listof A) Natural (Listof A))))
(define (take* l n)
  (for/list ([e (in-list l)]
             [i (in-range n)])
    e))

;; ---

(: time-zero Time)
(define time-zero '())

(: k (Parameterof Natural))
(define k (make-parameter 1))

(: tick (-> Stx Time Time))
(define (tick call time)
  (define label (Stx-label call))
  (take* (cons label time) (k)))

(: alloc (-> Time (-> Var Addr)))
(define ((alloc time) var)
  (Binding var time))

