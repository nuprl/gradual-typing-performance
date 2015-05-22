#lang typed/racket/base

(provide label-element?)

;; -----------------------------------------------------------------------------
;; =============================================================================

(: label-element? (-> Any Boolean))
(define (label-element? obj) #t)
