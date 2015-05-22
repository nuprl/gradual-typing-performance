#lang racket/base

(provide handle-key)

;; -----------------------------------------------------------------------------

(require
         "data-world.rkt"
(only-in "motion-world-change-dir.rkt" world-change-dir))

;; =============================================================================

(define (handle-key w ke)
  (cond [(equal? ke "w") (world-change-dir w "up")]
        [(equal? ke "s") (world-change-dir w "down")]
        [(equal? ke "a") (world-change-dir w "left")]
        [(equal? ke "d") (world-change-dir w "right")]
        [else w]))
