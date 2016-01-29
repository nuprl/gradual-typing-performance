#lang racket/base

(provide replay)

;; -----------------------------------------------------------------------------

(require
         racket/match
         "data-world.rkt"
(only-in "motion-reset.rkt" reset!)
(only-in "motion-world-world.rkt" world->world)
(only-in "handlers-handle-key.rkt" handle-key)
(only-in "handlers-game-over.rkt" game-over?))

;; =============================================================================

(define (replay w0 hist)
  (reset!)
  (let loop ((w w0)
             (h hist))
    (if (eq? '() h)
        w
        (let ()
          (loop
           (match (car h)
             [`(on-key ,(? string? ke))
              (handle-key w ke)]
             [`(on-tick)
              (world->world w)]
             [`(stop-when)
              (game-over? w)
              w])
           (cdr h)))))
  (void))
