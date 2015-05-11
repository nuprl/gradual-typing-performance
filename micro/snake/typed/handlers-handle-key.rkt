#lang typed/racket/base

(provide handle-key)

;; -----------------------------------------------------------------------------

(require benchmark-util
         "data-snake-adapted.rkt"
         "data-world-adapted.rkt")
(require/typed/check "motion-world-change-dir.rkt"
  [world-change-dir (-> World Dir World)])

;; =============================================================================

(: handle-key : (World String . -> . World))
(define (handle-key w ke)
  (cond [(equal? ke "w") (world-change-dir w "up")]
        [(equal? ke "s") (world-change-dir w "down")]
        [(equal? ke "a") (world-change-dir w "left")]
        [(equal? ke "d") (world-change-dir w "right")]
        [else w]))
