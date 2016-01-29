#lang racket/base

(provide snake-slither)

;; -----------------------------------------------------------------------------

(require
         "data-posn.rkt"
         "data-snake.rkt"
(only-in "motion-help-next-head.rkt" next-head)
(only-in "cut-tail.rkt" cut-tail))

;; =============================================================================

;; snake-slither : Snake -> Snake
;; move the snake one step
(define (snake-slither snk)
  (let ([d (snake-dir snk)])
    (snake d
           (cons (next-head (car (snake-segs snk))
                            d)
                 (cut-tail (snake-segs snk))))))
