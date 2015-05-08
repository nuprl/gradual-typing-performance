#lang typed/racket/base

(provide array-broadcasting)

;; =============================================================================

(: array-broadcasting (Parameterof (U #f #t 'permissive)))
(define array-broadcasting (make-parameter #t))
