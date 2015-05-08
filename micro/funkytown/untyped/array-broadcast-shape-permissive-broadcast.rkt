#lang racket/base

(provide shape-permissive-broadcast)

;; -----------------------------------------------------------------------------

(require
         (only-in racket/fixnum fxmax))

;; =============================================================================

(define (shape-permissive-broadcast ds1 ds2 dims fail)
  (define new-ds (make-vector dims 0))
  (let loop ([k 0])
    (cond [(k . < . dims)
           (define dk1 (vector-ref ds1 k))
           (define dk2 (vector-ref ds2 k))
           (vector-set!
            new-ds k
            (cond [(or (= dk1 0) (= dk2 0))  (fail)]
                  [else  (fxmax dk1 dk2)]))
           (loop (+ k 1))]
          [else  new-ds])))
