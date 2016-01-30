#lang typed/racket/base

(provide posn=?)

(require "data-posn-adapted.rkt")

;; =============================================================================

(: posn=? (-> posn posn Boolean))
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))
