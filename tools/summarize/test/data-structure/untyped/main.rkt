#lang racket/base
(require "x.rkt")

(define y (list x x x))
(for ([arg (in-list y)])
  (+ arg 1))
