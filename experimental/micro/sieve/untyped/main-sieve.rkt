#lang racket/base

(provide sieve)
(require benchmark-util
         "streams-struct.rkt"
"main-sift.rkt"
"streams-make-stream.rkt"
"streams-stream-unfold.rkt")

;; ---

;; `sieve st` Sieve of Eratosthenes
;(: sieve (-> stream stream))
(define (sieve st)
  (define-values (hd tl) (stream-unfold st))
  (make-stream hd (lambda () (sieve (sift hd tl)))))

