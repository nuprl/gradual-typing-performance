#lang racket/base

(require require-typed-check)
(require "aux.rkt")

(define (main)
  (add1 SLEEP-TIME)
  (void))

(time (main))
