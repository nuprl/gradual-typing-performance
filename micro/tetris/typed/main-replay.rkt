#lang typed/racket/base

(provide replay)

(require benchmark-util
         racket/match
         "data-block-adapted.rkt"
         "data-world-adapted.rkt")
(require/typed/check "world-world-key-move.rkt"
  [world-key-move (-> World String World)])
(require/typed/check "world-next-world.rkt"
  [next-world (-> World World)])
(require/typed/check "bset-blocks-overflow.rkt"
  [blocks-overflow? (-> BSet Boolean)])

;; =============================================================================

(: replay (-> World (Listof Any) Void))
(define (replay w0 hist)
  (for/fold ([w : World w0])
            ([e hist])
    (match e
      [`(on-key ,(? string? ke)) (world-key-move w ke)]
      [`(on-tick) (next-world w)]
      [`(stop-when)
       (λ ([w : World]) (blocks-overflow? (world-blocks w)))
       w]))
  (void))
