#lang racket/base

(provide shift-stretch-axes)

;; -----------------------------------------------------------------------------

(require
         "data-array.rkt"
         (only-in racket/fixnum fxmodulo)
(only-in "array-utils-make-thread-local-indexes.rkt"
  make-thread-local-indexes)
(only-in "array-struct-unsafe-build-array.rkt"
  unsafe-build-array))

;; =============================================================================

(define (shift-stretch-axes arr new-ds)
  (define old-ds (Array-shape arr))
  (define old-dims (vector-length old-ds))
  (define new-dims (vector-length new-ds))
  (define shift
    (let ([shift  (- new-dims old-dims)])
      (cond [(<= 0 shift)  shift]
            [else  (error 'array-broadcast
                          "cannot broadcast to a lower-dimensional shape; given ~e and ~e"
                          arr new-ds)])))
  (define old-js (make-thread-local-indexes old-dims))
  (define old-f (Array-unsafe-proc arr))
  (unsafe-build-array
   new-ds
   (lambda (new-js)
     (let ([old-js  (old-js)])
       (let loop ([k  0])
         (cond [(k . < . old-dims)
                (define new-jk (vector-ref new-js (+ k shift)))
                (define old-dk (vector-ref old-ds k))
                (define old-jk (fxmodulo new-jk old-dk))
                (vector-set! old-js k old-jk)
                (loop (+ k 1))]
               [else  (old-f old-js)]))))) )
