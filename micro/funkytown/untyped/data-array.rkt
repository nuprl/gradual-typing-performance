#lang racket/base

(provide (struct-out Array))

;; =============================================================================

(struct Array (shape
               size
               strict?
               strict!
               unsafe-proc))
