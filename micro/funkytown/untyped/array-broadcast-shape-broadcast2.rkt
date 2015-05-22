#lang racket/base

(provide shape-broadcast2)

;; -----------------------------------------------------------------------------

(require (only-in "array-broadcast-shape-insert-axes.rkt"
  shape-insert-axes)
(only-in "array-broadcast-shape-permissive-broadcast.rkt"
  shape-permissive-broadcast)
(only-in "array-broadcast-shape-normal-broadcast.rkt"
  shape-normal-broadcast))

;; =============================================================================

(define (shape-broadcast2 ds1 ds2 fail broadcasting)
  (cond [(equal? ds1 ds2)  ds1]
        [(not broadcasting)  (fail)]
        [else
         (define dims1 (vector-length ds1))
         (define dims2 (vector-length ds2))
         (define n (- dims2 dims1))
         (let-values ([(ds1 ds2 dims)
                       (cond [(n . > . 0)  (values (shape-insert-axes ds1 n) ds2 dims2)]
                             [(n . < . 0)  (values ds1 (shape-insert-axes ds2 (- n)) dims1)]
                             [else         (values ds1 ds2 dims1)])])
           (if (eq? broadcasting 'permissive)
               (shape-permissive-broadcast ds1 ds2 dims fail)
               (shape-normal-broadcast ds1 ds2 dims fail)))]))
