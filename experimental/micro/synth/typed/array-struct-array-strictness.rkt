#lang typed/racket/base

(provide array-strictness)

;; =============================================================================

(: array-strictness (Parameterof Boolean))
(define array-strictness (make-parameter #t))
