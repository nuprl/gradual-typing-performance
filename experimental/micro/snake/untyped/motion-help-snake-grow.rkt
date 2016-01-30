#lang racket/base

(provide snake-grow)

;; -----------------------------------------------------------------------------

(require
         "data-snake.rkt"
(only-in "motion-help-next-head.rkt" next-head))

;; =============================================================================

;; snake-grow : Snake -> Snake
;; Grow the snake one segment.
(define (snake-grow snk)
  (let ([d (snake-dir snk)])
    (snake d
           (cons (next-head (car (snake-segs snk))
                            d)
                 (snake-segs snk)))))
