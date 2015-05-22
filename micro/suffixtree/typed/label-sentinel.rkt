#lang typed/racket/base

(provide sentinel?)

;; -----------------------------------------------------------------------------
;; =============================================================================

(: sentinel? (-> Any Boolean))
(define (sentinel? datum)
 (symbol? datum))
