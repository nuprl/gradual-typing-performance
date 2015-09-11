#lang racket/base

(module data racket/base
  (require racket/contract)
  (struct atom ())
  (provide (struct-out atom)))

(module vec typed/racket/base
  (require/typed (submod ".." data)
    [#:struct atom []])
  (: make-v (-> (Vectorof atom) Boolean))
  (define (make-v x)
    #t)
  (provide make-v))

(require 'data)
(require 'vec)

(define (main)
  (for ([i (in-range 10000)])
    (make-v (make-vector 10000 (atom)))
    (void))
  (void))

(require contract-profile)
(contract-profile-thunk main)

