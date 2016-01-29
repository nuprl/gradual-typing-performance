#lang racket/base

(provide WORLD)

;; -----------------------------------------------------------------------------

(require
         "data-snake.rkt"
         "data-posn.rkt"
         "data-world.rkt")

;; =============================================================================

(define (WORLD) (world (snake "right" (cons (posn 5 3) '() ))
                       (posn 8 12)))
