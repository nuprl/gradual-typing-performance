#lang racket

(module stack typed/racket
  (define-type (Stack A) (Listof A))

  (: stack-empty? (All (A) ((Stack A) -> Boolean)))
  (define (stack-empty? stk)
    (null? stk))

  (provide stack-empty?))

(require 'stack)

(define my-stack (range 20))
(time
(for ([_i (in-range (expt 10 6))])
  (stack-empty? my-stack)))
