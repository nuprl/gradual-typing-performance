#lang racket

(require "data.rkt")
(require racket/vector)
;; NeSegs is one of:
;; - (cons Posn empty)
;; - (cons Posn NeSegs)

;; cut-tail : NeSegs -> Segs
;; Cut off the tail.
(define (cut-tail segs)
  (let ([r (vector-drop segs 1)])
    (cond [(equal? '#() r) '#()]
          [else (vector-append (vector (vector-ref segs 0)) (cut-tail r))])))

(provide
 cut-tail)
