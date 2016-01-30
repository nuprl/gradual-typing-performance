#lang racket

;; Exponential slowdown from object contracts.
;;
;; (Class contracts seem fine, so far)

;; -----------------------------------------------------------------------------

(module+ main
  (require
    benchmark-util
    racket/sandbox)

  (with-limits 1 1
    (time (main 99)))

)

;; -----------------------------------------------------------------------------

(module t1 typed/racket

  ;; --- 1. Create a "complicated-enough" object type
  (define-type ComplexEnough%
    (Class
      (lam (-> ComplexEnough ComplexEnough))))
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

  (provide c% obj myid))

;; --- 3. Repeatedly apply the function to an object
(require 't1)

(define (main M)
  (for/fold
      ([obj1 obj])
      ([i (in-range M)])
    (myid obj1))
  (void))

