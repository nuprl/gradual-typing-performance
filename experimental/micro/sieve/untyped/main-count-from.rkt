#lang racket/base

(provide count-from)
(require benchmark-util
         "streams-struct.rkt"
         "streams-make-stream.rkt")

;; ---

;; `count-from n` Build a stream of integers starting from `n` and iteratively adding 1
;(: count-from (-> Natural stream))
(define (count-from n)
  (make-stream n (lambda () (count-from (add1 n)))))
