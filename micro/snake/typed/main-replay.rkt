#lang typed/racket/base

(provide replay)

;; -----------------------------------------------------------------------------

(require benchmark-util
         racket/match
         "data-world-adapted.rkt")
(require/typed/check "motion-reset.rkt"
  [reset! (-> Void)])
(require/typed/check "motion-world-world.rkt"
  [world->world (-> World World)])
(require/typed/check "handlers-handle-key.rkt"
  [handle-key (-> World String World)])
(require/typed/check "handlers-game-over.rkt"
  [game-over? (-> World Boolean)])

;; =============================================================================

(: replay : World (Listof Any) -> Void)
(define (replay w0 hist)
  (reset!)
  (let loop ((w : World w0)
             (h : (Listof Any) hist))
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
