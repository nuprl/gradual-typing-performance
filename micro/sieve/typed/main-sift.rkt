#lang typed/racket/base

(provide sift)
(require benchmark-util
         "streams-struct-adapted.rkt")
(require/typed/check "streams-make-stream.rkt"
  [make-stream (-> Natural (-> stream) stream)])
(require/typed/check "streams-stream-unfold.rkt"
  [stream-unfold (-> stream (values Natural stream))])

;; ---

;; `sift n st` Filter all elements in `st` that are equal to `n`.
;; Return a new stream.
(: sift (-> Natural stream stream))
(define (sift n st)
  (define-values (hd tl) (stream-unfold st))
  (cond [(= 0 (modulo hd n)) (sift n tl)]
        [else (make-stream hd (lambda () (sift n tl)))]))

