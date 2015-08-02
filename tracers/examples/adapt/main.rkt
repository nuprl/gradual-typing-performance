#lang typed/racket/base

;; Hmmm, the output looks good.
;; I think the boundary is just mis-attributed, because instead of
;;  data-adapted => main
;; we have boundaries
;;  data => data-adapted
;; that `main` is tripping.

;; Does not work! Cannot require/typed from the adaptor
;(require/typed "data-adapted.rkt"
;  [#:struct foo ([x : (-> Natural Natural)] [y : String] [z : (Listof Natural)])]
;  [val Index]
;  [foo1 foo])

;; To match common use case
(require "data-adapted.rkt")

(define (main)
  ((foo-x foo1) 1)
  (eq? '() (foo-z foo1))
  (+ val val)
  (foo (lambda (x) (+ x 1)) "yes" '())
  (lam 1)
  (lam (lam val))
  (void))

(main)
