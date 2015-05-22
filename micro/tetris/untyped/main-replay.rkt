#lang racket/base

(provide replay)

(require benchmark-util
         racket/match
         "data-world.rkt")
(require "world-world-key-move.rkt")
(require "world-next-world.rkt")
(require "bset-blocks-overflow.rkt")

;; =============================================================================

;(: replay (-> World (Listof Any) Void))
(define (replay w0 hist)
  (for/fold ([w w0])
            ([e hist])
    (match e
      [`(on-key ,(? string? ke)) (world-key-move w ke)]
      [`(on-tick) (next-world w)]
      [`(stop-when)
       (λ (w) (blocks-overflow? (world-blocks w)))
       w]))
  (void))
