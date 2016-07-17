#lang typed/racket/base

(require require-typed-check)
(require/typed/check "aux.rkt"
  [SLEEP-TIME Natural])

(define (main)
  (add1 SLEEP-TIME)
  (void))

(time (main))
