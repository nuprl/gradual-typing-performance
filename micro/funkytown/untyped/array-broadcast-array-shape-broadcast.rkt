#lang racket/base

(provide array-shape-broadcast)

;; -----------------------------------------------------------------------------

(require
         (only-in racket/string string-join)
 (only-in "array-broadcast-array-broadcasting.rkt"
  array-broadcasting)
 (only-in "array-broadcast-shape-broadcast2.rkt"
  shape-broadcast2))

;; =============================================================================

(define (array-shape-broadcast dss [broadcasting (array-broadcasting)])
  (define (fail) (error 'array-shape-broadcast
                        "incompatible array shapes (array-broadcasting ~v): ~a"
                        broadcasting
                        (string-join (map (λ (ds) (format "~e" ds)) dss) ", ")))
  (cond [(eq? '() dss)  #()]
        [else  (for/fold ([new-ds  (car dss)]) ([ds  (in-list (cdr dss))])
                 (shape-broadcast2 new-ds ds fail broadcasting))]))
