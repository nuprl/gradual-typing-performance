#lang typed/racket/base

(define-type Stack (Listof Integer))

(require require-typed-check)
(require/typed/check "stack.rkt"
  [init (-> Stack)]
  [push (Stack Integer . -> . Stack)])

(: main (Natural . -> . Void))
(define (main N)
  (for/fold ([st (init)])
            ([i (in-range N)])
    (push st i))
  (void))

(time (main 60000))
