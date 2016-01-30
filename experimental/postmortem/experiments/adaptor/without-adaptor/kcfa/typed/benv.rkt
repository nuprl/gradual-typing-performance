#lang typed/racket/base

;; Binding environment,
;; helper functions

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

(provide
  (struct-out Closure)
  (struct-out Binding)
  empty-benv
  benv-lookup
  benv-extend
  benv-extend*
)

;; =============================================================================

;; -- private

(define-type BEnv (HashTable Var Addr))
(define-type Addr Binding)
(define-type Time (Listof Label))

;; -- structs

(struct Closure
 ([lam : Lam]
  [benv : BEnv]))

(struct Binding
 ([var : Var]
  [time : Time]))

;; -- public

(: empty-benv BEnv)
(define empty-benv (make-immutable-hasheq '()))

(: benv-lookup (-> BEnv Var Addr))
(define benv-lookup hash-ref)

(: benv-extend (-> BEnv Var Addr BEnv))
(define benv-extend hash-set)

(: benv-extend* (-> BEnv (Listof Var) (Listof Addr) BEnv))
(define (benv-extend* benv vars addrs)
  (for/fold ([benv benv])
    ([v (in-list vars)]
     [a (in-list addrs)])
    (benv-extend benv v a)))

