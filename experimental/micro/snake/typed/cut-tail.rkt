#lang typed/racket/base

(provide cut-tail)

;; -----------------------------------------------------------------------------

(require "data-posn-adapted.rkt")

;; =============================================================================

;; cut-tail : NeSegs -> Segs
;; Cut off the tail.
(: cut-tail : (-> NEList (Listof Posn)))
(define (cut-tail segs)
  (let ([r (cdr segs)])
    (cond [(eq? '() r) '()]
          [else (cons (car segs) (cut-tail (cons (car r) (cdr r))))])))
