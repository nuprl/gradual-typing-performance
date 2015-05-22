#lang typed/racket/base

(provide next-head)

;; -----------------------------------------------------------------------------

(require benchmark-util
         "data-snake-adapted.rkt"
         "data-posn-adapted.rkt")

;; =============================================================================

;; next-head : Posn Direction -> Posn
;; Compute next position for head.
(: next-head : (Posn Dir . -> . Posn))
(define (next-head seg dir)
  (cond [(equal? "right" dir) (posn (add1 (posn-x seg)) (posn-y seg))]
        [(equal? "left" dir)  (posn (sub1 (posn-x seg)) (posn-y seg))]
        [(equal? "down" dir)  (posn (posn-x seg) (sub1 (posn-y seg)))]
        [else                 (posn (posn-x seg) (add1 (posn-y seg)))]))
