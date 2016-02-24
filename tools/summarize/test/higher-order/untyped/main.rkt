#lang racket/base

(require "vec.rkt")

(define (f x) 0)
(begin
  (f v)
  (f v)
  (f v)
  (void))
