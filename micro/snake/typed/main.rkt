#lang typed/racket/base

;; -----------------------------------------------------------------------------

(require benchmark-util
         "data-world-adapted.rkt")
(require/typed/check "const-world.rkt"
  [WORLD (-> World)])
(require/typed/check "main-replay.rkt"
  [replay (-> World (Listof Any) Void)])

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
