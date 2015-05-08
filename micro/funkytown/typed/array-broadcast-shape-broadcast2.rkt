#lang typed/racket/base

(provide shape-broadcast2)

;; -----------------------------------------------------------------------------

(require "type-aliases.rkt"
         benchmark-util)
(require/typed/check "array-broadcast-shape-insert-axes.rkt"
  [shape-insert-axes (Indexes Integer -> Indexes)])
(require/typed/check "array-broadcast-shape-permissive-broadcast.rkt"
  [shape-permissive-broadcast (Indexes Indexes Integer (-> Nothing) -> Indexes)])
(require/typed/check "array-broadcast-shape-normal-broadcast.rkt"
  [shape-normal-broadcast (Indexes Indexes Integer (-> Nothing) -> Indexes)])

;; =============================================================================

(: shape-broadcast2 (Indexes Indexes (-> Nothing) (U #f #t 'permissive) -> Indexes))
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
