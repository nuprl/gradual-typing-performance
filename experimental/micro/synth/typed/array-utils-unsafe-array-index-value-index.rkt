#lang typed/racket/base

(provide unsafe-array-index->value-index)

;; -----------------------------------------------------------------------------

(require (only-in racket/performance-hint begin-encourage-inline)
         "type-aliases.rkt"
         (only-in racket/fixnum fx* fx+)
         benchmark-util)

;; (require/typed/check "array-utils-array-shape-size.rkt"
;;   [array-shape-size (Indexes -> Integer)])

;; =============================================================================

(begin-encourage-inline
  (: unsafe-array-index->value-index (Indexes Indexes -> Integer))
  (define (unsafe-array-index->value-index ds js)
    (define dims (vector-length ds))
    (let loop ([#{i : Integer} 0] [#{j : Integer} 0])
      (cond [(i . < . dims)
             (define di (vector-ref ds i))
             (define ji (vector-ref js i))
             (loop (+ i 1) (fx+ ji (fx* di j)))]
            [else  j])))
)
