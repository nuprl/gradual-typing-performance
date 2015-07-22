#lang racket/base

(provide fact-naive fact-acc)
;(require racket/contract)
;(provide
;  (contract-out
;    [fact-naive (-> natural-number/c natural-number/c)]
;    [fact-acc   (-> natural-number/c natural-number/c)])
;)

(define (fact-naive n)
  (if (< n 2) 1 (* n (fact-naive (sub1 n)))))

(define (fact-acc n)
  (for/fold ([acc 1])
            ([i (in-range 1 (add1 n))])
    (* acc i)))
