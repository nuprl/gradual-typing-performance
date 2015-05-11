#lang typed/racket/base

(provide snake-slither)

;; -----------------------------------------------------------------------------

(require benchmark-util
         "data-posn-adapted.rkt"
         "data-snake-adapted.rkt")
(require/typed/check "motion-help-next-head.rkt"
  [next-head (-> Posn Dir Posn)])
(require/typed/check "cut-tail.rkt"
  [cut-tail (-> NEList (Listof Posn))])

;; =============================================================================

;; snake-slither : Snake -> Snake
;; move the snake one step
(: snake-slither : (Snake . -> . Snake))
(define (snake-slither snk)
  (let ([d (snake-dir snk)])
    (snake d
           (cons (next-head (car (snake-segs snk))
                            d)
                 (cut-tail (snake-segs snk))))))
