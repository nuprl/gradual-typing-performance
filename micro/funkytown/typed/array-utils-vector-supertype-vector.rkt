#lang typed/racket/base

(provide vector->supertype-vector)

;; -----------------------------------------------------------------------------

(require (only-in racket/performance-hint begin-encourage-inline)
         "type-aliases.rkt")

;; =============================================================================

(begin-encourage-inline

 (: vector->supertype-vector (-> Indexes Indexes))
 (define (vector->supertype-vector js)
   (define dims (vector-length js))
   (cond [(= dims 0)  (vector)]
         [else  (define: new-js : Indexes (make-vector dims (vector-ref js 0)))
                (let loop ([#{i : Integer} 1])
                  (cond [(i . < . dims)  (vector-set! new-js i (vector-ref js i))
                         (loop (+ i 1))]
                        [else  new-js]))]))
)
