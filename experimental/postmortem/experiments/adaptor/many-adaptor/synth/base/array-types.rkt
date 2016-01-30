#lang typed/racket/base

;; Type aliases for the array library

(provide
  Indexes
  In-Indexes
)


(define-type Indexes (Vectorof Integer))
(define-type In-Indexes Indexes)


