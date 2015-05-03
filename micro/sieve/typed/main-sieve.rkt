#lang typed/racket/base

(provide sieve)
(require benchmark-util
         "streams-struct-adapted.rkt")
(require/typed/check "main-sift.rkt"
  [sift (-> Natural stream stream)])
(require/typed/check "streams-make-stream.rkt"
  [make-stream (-> Natural (-> stream) stream)])
(require/typed/check "streams-stream-unfold.rkt"
  [stream-unfold (-> stream (values Natural stream))])

;; ---

;; `sieve st` Sieve of Eratosthenes
(: sieve (-> stream stream))
(define (sieve st)
  (define-values (hd tl) (stream-unfold st))
  (make-stream hd (lambda () (sieve (sift hd tl)))))

