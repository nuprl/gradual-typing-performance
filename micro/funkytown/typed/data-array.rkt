#lang typed/racket/base

(provide (struct-out Array))

;; =============================================================================

(struct Array ([shape : (Vectorof Integer)]
               [size : Integer]
               [strict? : (Boxof Boolean)]
               [strict! : (-> Void)]
               [unsafe-proc : (-> (Vectorof Integer) Float)]))
