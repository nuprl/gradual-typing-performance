#lang typed/racket/base

(require/typed "data.rkt"
  [#:struct anystruct (
    [var : String]
    [fun : (-> Natural Natural Natural (Pairof Natural Natural))]
    [vec : (Vectorof (U Symbol Integer))]
   )]
  [s1 anystruct])

(define s2 (anystruct "yes" (lambda (a b c) (cons 1 1)) (vector 8)))
(define v1 (anystruct-var s1))
(define v2 ((anystruct-fun s1) 1 1 1))
(define v3 (vector-ref (anystruct-vec s1) 2))

;; -----------------------------------------------------------------------------
;; --- output
;; [BG:CREATE]	0	main.rkt	anystruct?	data.rkt
;; [BG:CREATE]	1	main.rkt	(#<syntax:/home/ben/code/racket/benchmark/gradual-typing-performance/tracers/examples/struct/main.rkt:4:12 anystruct> #<syntax anystruct3>)	data.rkt
;; [BG:CREATE]	2	main.rkt	anystruct-var	data.rkt
;; [BG:CREATE]	3	main.rkt	anystruct-fun	data.rkt
;; [BG:CREATE]	4	main.rkt	anystruct-vec	data.rkt
;; [BG:CREATE]	5	main.rkt	s1	data.rkt
;; [BG:APPLY]	0	main.rkt	anystruct?	data.rkt
;; [BG:APPLY]	5	main.rkt	s1	data.rkt
;; [BG:APPLY]	0	main.rkt	anystruct?	data.rkt
;; [BG:APPLY]	1	main.rkt	(#<syntax:/home/ben/code/racket/benchmark/gradual-typing-performance/tracers/examples/struct/main.rkt:4:12 anystruct> #<syntax anystruct3>)	data.rkt
;; [BG:APPLY]	2	main.rkt	anystruct-var	data.rkt
;; [BG:APPLY]	3	main.rkt	anystruct-fun	data.rkt
;; [BG:APPLY]	3	main.rkt	anystruct-fun	data.rkt
