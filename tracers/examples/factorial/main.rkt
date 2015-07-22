#lang typed/racket/base

(require/typed "fact.rkt" [fact-naive (-> Natural Natural)] [fact-acc (-> Natural Natural)])
;(require "fact.rkt")

(define (main)
  (fact-naive 1)
  (fact-naive 10)
  (fact-acc 1)
  (fact-acc 10)
  (void))

(main)
