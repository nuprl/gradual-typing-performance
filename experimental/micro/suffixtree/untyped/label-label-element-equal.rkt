#lang racket/base

(provide label-element-equal?)

;; -----------------------------------------------------------------------------
;; =============================================================================

;; When comparing label elements, we use equal?.
(define label-element-equal? equal?)
