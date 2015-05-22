#lang racket/base

(provide check-array-indexes)

;; -----------------------------------------------------------------------------

(require (only-in "array-utils-raise-array-index-error.rkt"
  raise-array-index-error))

;; =============================================================================

(define (check-array-indexes name ds js)
  (define (raise-index-error) (raise-array-index-error name ds js))
  (define dims (vector-length ds))
  (unless (= dims (vector-length js)) (raise-index-error))
  (define new-js (make-vector dims 0))
  (let loop ([i 0])
    (cond [(i . < . dims)
           (define di (vector-ref ds i))
           (define ji (vector-ref js i))
           (cond [(and (exact-integer? ji) (0 . <= . ji) (ji . < . di))
                  (vector-set! new-js i ji)
                  (loop (+ i 1))]
                 [else  (raise-index-error)])]
          [else  new-js])))
