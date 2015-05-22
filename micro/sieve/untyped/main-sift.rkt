#lang racket/base

(provide sift)
(require benchmark-util
         "streams-struct.rkt"
"streams-make-stream.rkt"
"streams-stream-unfold.rkt")

;; ---

;; `sift n st` Filter all elements in `st` that are equal to `n`.
;; Return a new stream.
;(: sift (-> Natural stream stream))
(define (sift n st)
  (define-values (hd tl) (stream-unfold st))
  (cond [(= 0 (modulo hd n)) (sift n tl)]
        [else (make-stream hd (lambda () (sift n tl)))]))

