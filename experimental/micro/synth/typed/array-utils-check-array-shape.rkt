#lang typed/racket/base

(provide check-array-shape)

;; -----------------------------------------------------------------------------

(require (only-in racket/performance-hint begin-encourage-inline)
         "type-aliases.rkt"
         benchmark-util)

;; (require/typed/check "array-utils-array-shape-size.rkt"
;;   [array-shape-size (Indexes -> Integer)])

;; =============================================================================

(begin-encourage-inline
  (: check-array-shape ((Vectorof Integer) (-> Nothing) -> Indexes))
  (define (check-array-shape ds fail)
    (define dims (vector-length ds))
    (define: new-ds : Indexes (make-vector dims 0))
    (let loop ([#{i : Integer} 0])
      (cond [(i . < . dims)
             (define di (vector-ref ds i))
             (cond [(index? di)  (vector-set! new-ds i di)
                                 (loop (+ i 1))]
                   [else  (fail)])]
            [else  new-ds])))
)
