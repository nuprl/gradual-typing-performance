#lang racket/base

(provide array-index->value-index)

;; -----------------------------------------------------------------------------

(require
         (only-in racket/fixnum fx* fx+)
(only-in "array-utils-raise-array-index-error.rkt"
  raise-array-index-error))

;; =============================================================================

(define (array-index->value-index name ds js)
  (define (raise-index-error) (raise-array-index-error name ds js))
  (define dims (vector-length ds))
  (unless (= dims (vector-length js)) (raise-index-error))
  (let loop ([i 0] [j  0])
    (cond [(i . < . dims)
           (define di (vector-ref ds i))
           (define ji (vector-ref js i))
           (cond [(and (exact-integer? ji) (0 . <= . ji) (ji . < . di))
                  (loop (+ i 1) (fx+ ji (fx* di j)))]
                 [else  (raise-index-error)])]
          [else  j])))
