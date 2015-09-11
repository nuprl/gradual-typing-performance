#lang racket/base

(module data racket/base
  (require racket/contract)
  (struct atom ())
  (provide (struct-out atom)))

(module ls typed/racket/base
  (require/typed (submod ".." data)
    [#:struct atom []])
  (: make-l (-> (Listof atom) Boolean))
  (define (make-l x)
    #t)
  (provide make-l))

(require 'data)
(require 'ls)
(require racket/list)

(define (main)
  (for ([i (in-range 10000)])
    (make-l (make-list 10000 (atom)))
    (void))
  (void))

(require contract-profile)
(contract-profile-thunk main)

