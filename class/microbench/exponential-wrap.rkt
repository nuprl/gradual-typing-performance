#lang racket

;; Exponential slowdown from class contracts.

;; -----------------------------------------------------------------------------

(module+ main
  (define lo 6)
  (define hi 9)

  (for ([i (in-list (for/list ([j (in-range lo hi)]) (* 5 j)))])
    (displayln i)
    (time (main i))))

;; -----------------------------------------------------------------------------

(module t1 typed/racket

  ;; --- 1. Create a "complicated-enough" object type
  (define-type ComplexEnough%
    (Class
      (lam
        ;(-> (Class) (Class))            ;; No problem
        ;(-> C C)                        ;; Problem
        ;(-> Any ComplexEnough)          ;; Less-serious problem
        ;(-> ComplexEnough Any)          ;; Problem
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

  ;; --- 2. Create a simple function on objects
  (: myid (-> ComplexEnough ComplexEnough))
  (define (myid x) x)

  (provide obj myid))

;; --- 3. Repeatedly apply the function to an object
(require 't1)
(define (main M)
  (for/fold
      ([obj obj])
      ([i (in-range M)])
    (myid obj))
  (void))

