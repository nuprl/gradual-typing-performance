#lang racket/base



(module original racket/base
  (require racket/contract)
  (define (make-truth x) #t)
  (provide (contract-out [make-truth (-> any/c boolean?)])))

(module slow-contract racket/base
  (require racket/contract)
  (define (boolean+ x)
    (for ([i (in-range 200)])
      (boolean? x))
    (boolean? x))
  (define (make-truth x) #t)
  (provide (contract-out [make-truth (-> any/c boolean+)])))

(module slow-function racket/base
  (require racket/contract)
  (define (make-truth x)
    (for ([i (in-range 200)])
      (boolean? #t))
    #t)
  (provide (contract-out [make-truth (-> any/c boolean?)])))

(require
  'original
  ;'slow-contract
  ;'slow-function
)

(define (main)
  (for ([i (in-range 999999)])
    (make-truth i)))

(require contract-profile)
(contract-profile-thunk main)
