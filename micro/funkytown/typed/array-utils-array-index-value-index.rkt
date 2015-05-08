#lang typed/racket/base

(provide array-index->value-index)

;; -----------------------------------------------------------------------------

(require "type-aliases.rkt"
         (only-in racket/fixnum fx* fx+)
         benchmark-util)
(require/typed/check "array-utils-raise-array-index-error.rkt"
  [raise-array-index-error (Symbol Indexes In-Indexes -> Nothing)])

;; =============================================================================

(: array-index->value-index (Symbol Indexes In-Indexes -> Integer))
(define (array-index->value-index name ds js)
  (define (raise-index-error) (raise-array-index-error name ds js))
  (define dims (vector-length ds))
  (unless (= dims (vector-length js)) (raise-index-error))
  (let loop ([#{i : Integer} 0] [#{j : Integer}  0])
    (cond [(i . < . dims)
           (define di (vector-ref ds i))
           (define ji (vector-ref js i))
           (cond [(and (exact-integer? ji) (0 . <= . ji) (ji . < . di))
                  (loop (+ i 1) (fx+ ji (fx* di j)))]
                 [else  (raise-index-error)])]
          [else  j])))
