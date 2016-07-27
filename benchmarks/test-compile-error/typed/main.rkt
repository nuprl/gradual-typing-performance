#lang typed/racket/base

(require/typed "aux.rkt"
  [SLEEP-TIME (<- COMPILE ERROR ->)])

(define (main)
  (add1 SLEEP-TIME)
  (void))

(time (main))
