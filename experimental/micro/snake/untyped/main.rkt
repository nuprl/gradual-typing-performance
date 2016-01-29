#lang racket/base

;; -----------------------------------------------------------------------------

(require
(only-in "const-world.rkt" WORLD)
(only-in "main-replay.rkt" replay))

;; =============================================================================

(define (main)
  (define raw-hist (with-input-from-file "../base/snake-hist-4.txt" read))
  (cond [(list? raw-hist)
         (define hist (reverse raw-hist))
         (for ([i (in-range 100)])
           (replay (WORLD) hist))]
        [else
         (error "THIS BENCHMARK IS BROKEN")]))

(time (main))
