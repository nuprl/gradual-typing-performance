#lang racket/base

(require benchmark-util)
(require "aux.rkt")

(define (main)
  (add1 SLEEP-TIME)
  (void))

(time (main))
