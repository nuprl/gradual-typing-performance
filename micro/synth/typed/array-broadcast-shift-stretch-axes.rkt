#lang typed/racket/base

(provide shift-stretch-axes)

;; -----------------------------------------------------------------------------

(require "type-aliases.rkt"
         "data-array-adapted.rkt"
         (only-in racket/fixnum fxmodulo)
         benchmark-util)

(require/typed/check "array-utils-make-thread-local-indexes.rkt"
  [make-thread-local-indexes (Integer -> (-> Indexes))])
(require/typed/check "array-struct-unsafe-build-array.rkt"
  [unsafe-build-array ((Vectorof Integer) ((Vectorof Integer) -> Float) -> Array)])

;; =============================================================================

(: shift-stretch-axes (-> Array Indexes Array))
(define (shift-stretch-axes arr new-ds)
  (define old-ds (Array-shape arr))
  (define old-dims (vector-length old-ds))
  (define new-dims (vector-length new-ds))
  (define shift
    (let ([shift  (- new-dims old-dims)])
      (cond [(index? shift)  shift]
            [else  (error 'array-broadcast
                          "cannot broadcast to a lower-dimensional shape; given ~e and ~e"
                          arr new-ds)])))
  (define old-js (make-thread-local-indexes old-dims))
  (define old-f (Array-unsafe-proc arr))
  (unsafe-build-array
   new-ds
   (λ: ([new-js : Indexes])
     (let ([old-js  (old-js)])
       (let: loop : Float ([k : Integer  0])
         (cond [(k . < . old-dims)
                (define new-jk (vector-ref new-js (+ k shift)))
                (define old-dk (vector-ref old-ds k))
                (define old-jk (fxmodulo new-jk old-dk))
                (vector-set! old-js k old-jk)
                (loop (+ k 1))]
               [else  (old-f old-js)]))))) )
