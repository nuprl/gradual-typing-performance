#lang typed/racket/base

(provide label-element-equal?)

;; -----------------------------------------------------------------------------
;; =============================================================================

;; When comparing label elements, we use equal?.
(: label-element-equal? (-> Any Any Boolean))
(define label-element-equal? equal?)
