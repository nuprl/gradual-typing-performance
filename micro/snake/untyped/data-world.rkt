#lang racket/base

(provide (struct-out world))

;; -----------------------------------------------------------------------------

(require "data-snake.rkt"
         "data-posn.rkt")

;; =============================================================================

(struct world (snake
               food))
