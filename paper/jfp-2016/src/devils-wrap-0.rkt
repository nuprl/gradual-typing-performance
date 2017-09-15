#lang racket

;; Exponential slowdown from object contracts.
;;  (Class contracts are fine here)

;; Runs in milliseconds on v6.4, seconds on v <= 6.3
;;  (and exponentially slower as iterations increase)
;; Fix was removing currying in the contract library so `eq?` checks now succeed.

;; -----------------------------------------------------------------------------

(module T typed/racket
  (define-type C%
    (Class (f (-> (Instance C%) Void))))

  (define c% : C%
    (class object%
      (super-new)
      (define/public (f x) (void))))

  (define id (lambda ([x : (Instance C%)]) x))

  (define my-c : (Instance C%)
    (new c%))

  (provide id my-c))
(require 'T)

;; -----------------------------------------------------------------------------

(define (loop N obj)
  (if (zero? N)
    (void)
    (loop (- N 1) (id obj))))
(loop 99 my-c)
