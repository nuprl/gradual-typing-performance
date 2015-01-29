#lang racket

(require "c.rkt")

(provide f)

(define (f x)
  (sleep 0.01)
  (mac x))
