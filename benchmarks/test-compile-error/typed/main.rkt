#lang typed/racket/base

(require require-typed-check)
(require/typed/check "aux.rkt"
  [SLEEP-TIME (<- COMPILE ERROR ->)])

(define (main)
  (add1 SLEEP-TIME)
  (void))

(time (main))
