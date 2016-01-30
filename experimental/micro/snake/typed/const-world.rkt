#lang typed/racket/base

(provide WORLD)

;; -----------------------------------------------------------------------------

(require benchmark-util
         "data-snake-adapted.rkt"
         "data-posn-adapted.rkt"
         "data-world-adapted.rkt")

;; =============================================================================

(: WORLD (-> World))
(define (WORLD) (world (snake "right" (cons (posn 5 3) '() ))
                       (posn 8 12)))
