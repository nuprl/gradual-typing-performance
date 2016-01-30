#lang typed/racket/base

(provide count-from)
(require benchmark-util
         "streams-struct-adapted.rkt")
(require/typed/check "streams-make-stream.rkt"
  [make-stream (-> Natural (-> stream) stream)])

;; ---

;; `count-from n` Build a stream of integers starting from `n` and iteratively adding 1
(: count-from (-> Natural stream))
(define (count-from n)
  (make-stream n (lambda () (count-from (add1 n)))))
