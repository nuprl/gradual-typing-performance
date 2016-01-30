#lang racket/base

(provide make-stream)
(require "streams-struct.rkt")

;(: make-stream (-> Natural (-> stream) stream))
(define (make-stream hd thunk)
  (stream hd thunk))
