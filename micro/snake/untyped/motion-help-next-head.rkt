#lang racket/base

(provide next-head)

;; -----------------------------------------------------------------------------

(require
         "data-snake.rkt"
         "data-posn.rkt")

;; =============================================================================

;; next-head : Posn Direction -> Posn
;; Compute next position for head.
(define (next-head seg dir)
  (cond [(equal? "right" dir) (posn (add1 (posn-x seg)) (posn-y seg))]
        [(equal? "left" dir)  (posn (sub1 (posn-x seg)) (posn-y seg))]
        [(equal? "down" dir)  (posn (posn-x seg) (sub1 (posn-y seg)))]
        [else                 (posn (posn-x seg) (add1 (posn-y seg)))]))
