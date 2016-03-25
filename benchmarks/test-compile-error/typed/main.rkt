#lang typed/racket/base

(require benchmark-util)
(require/typed/check "aux.rkt"
  [SLEEP-TIME (<- COMPILE ERROR ->)])

(define (main)
  (add1 SLEEP-TIME)
  (void))

(time (main))
