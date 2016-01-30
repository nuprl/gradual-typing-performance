#lang typed/racket/base

(provide reset!)

;; -----------------------------------------------------------------------------

(require benchmark-util)
(require/typed/check "motion-r.rkt"
  [r Pseudo-Random-Generator])

;; =============================================================================

(: reset! (-> Void))
(define (reset!)
  (parameterize ((current-pseudo-random-generator r))
    (random-seed 1324)))
