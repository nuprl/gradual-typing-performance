#lang racket

;; The problem from Forth, in a small example
;;
;; Trouble is iteratively calling a mixin on a "sufficiently complicated" object

;; Solution: typeful contracts?
;; - Objects from typed would be "certified chaperones"
;; - When a "certified" chap flows in, don't add a wrapper
;; (Trouble is knowing when a seal is broken)

(module t1 typed/racket

  ;; --- 1. Create a "complicated-enough" object type
  (define-type ComplexEnough%
    (Class
      (lam
        ;(-> (Class) (Class))  ;; No problem
        ;(-> C C) ;; Problem
        ;(-> Any ComplexEnough) ;; Less-serious problem
        ;(-> ComplexEnough Any) ;; Problem
        (-> ComplexEnough ComplexEnough) ;; Problem
      )))
  (define-type ComplexEnough (Instance ComplexEnough%))

  (: c% ComplexEnough%)
  (define c%
    (class object%
      (super-new)
      (define/public (lam x) x)))

  (: obj ComplexEnough)
  (define obj (new c%))

  ;; --- 2. Create a simple mixin
  (: mixin (-> ComplexEnough ComplexEnough))
  (define (mixin x) x)

  (provide obj mixin))

;; -----------------------------------------------------------------------------

(require 't1)

(define (main M)
  (for/fold
      ([obj obj])
      ([i (in-range M)])
    (mixin obj))
  (void))

;; -----------------------------------------------------------------------------

(module+ main
  (define iters 30)

  (for ([i (in-list (for/list ([j (in-range 1 iters)]) (* 5 j)))])
    (displayln i)
    (time (main i))))
