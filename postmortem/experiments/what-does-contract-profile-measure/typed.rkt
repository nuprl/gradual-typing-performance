#lang racket/base



(module original typed/racket/base
  (: make-truth (-> Any Boolean))
  (define (make-truth x) #t)
  (provide make-truth))

(module slow-contract typed/racket/base
  (define-type Boolean (U #t #f 0 1 2 3 4 5 6 7 8 9 10))
  (: make-truth (-> Any Boolean))
  (define (make-truth x) #t)
  (provide make-truth))

(module slow-function typed/racket/base
  (: make-truth (-> Any Boolean))
  (define (make-truth x)
    (for ([i (in-range 200)])
      (boolean? #t))
    #t)
  (provide make-truth))

(require
  ;'original
  ;'slow-contract
  'slow-function
)

(define (main)
  (for ([i (in-range 999999)])
    (make-truth i)))

(require contract-profile)
(contract-profile-thunk main)
