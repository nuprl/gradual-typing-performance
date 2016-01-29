#lang racket/base

(provide segs-self-collide?)

;; -----------------------------------------------------------------------------

(require
         "data-posn.rkt"
(only-in "data-posn-eq.rkt" posn=?))

;; =============================================================================

(define (segs-self-collide? h segs)
  (cond [(eq? '() segs) #f]
        [else (or (posn=? (car segs) h)
                  (segs-self-collide? h (cdr segs)))]))
