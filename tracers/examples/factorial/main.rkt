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

;; -----------------------------------------------------------------------------
;; --- output
;; [BG:CREATE]	0	main.rkt	my-num	fact.rkt
;; [BG:APPLY]	0	main.rkt	my-num	fact.rkt
;; [BG:CREATE]	1	main.rkt	fact-naive	fact.rkt
;; [BG:CREATE]	2	main.rkt	fact-acc	fact.rkt
;; [BG:APPLY]	1	main.rkt	fact-naive	fact.rkt
;; [BG:APPLY]	1	main.rkt	fact-naive	fact.rkt
;; [BG:APPLY]	2	main.rkt	fact-acc	fact.rkt
;; [BG:APPLY]	2	main.rkt	fact-acc	fact.rkt
