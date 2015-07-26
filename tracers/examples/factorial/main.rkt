#lang typed/racket/base

(require/typed "fact.rkt"
  [my-num Natural]
  [fact-naive (-> Natural Natural)]
  [fact-acc (-> Natural Natural)])

(define (main)
  (fact-naive 1)
  (fact-naive 10)
  (fact-acc 1)
  (fact-acc 10)
  (+ 1 my-num)
  (void))

(main)
