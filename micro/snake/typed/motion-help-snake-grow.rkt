#lang typed/racket/base

(provide snake-grow)

;; -----------------------------------------------------------------------------

(require benchmark-util
         "data-posn-adapted.rkt"
         "data-snake-adapted.rkt")
(require/typed/check "motion-help-next-head.rkt"
  [next-head (-> Posn Dir Posn)])

;; =============================================================================

;; snake-grow : Snake -> Snake
;; Grow the snake one segment.
(: snake-grow : (Snake . -> . Snake))
(define (snake-grow snk)
  (let ([d (snake-dir snk)])
    (snake d
           (cons (next-head (car (snake-segs snk))
                            d)
                 (snake-segs snk)))))
