#lang racket/base

(provide fact-naive fact-acc my-num)

(define my-num 4)

(define (fact-naive n)
  (if (< n 2)
    1
    (* n (fact-naive (sub1 n)))))

(define (fact-acc n)
  (for/fold ([acc 1])
            ([i (in-range 1 (add1 n))])
    (* acc i)))
