#lang typed/racket/base

(provide stream-unfold)
(require "streams-struct-adapted.rkt")

;; Destruct a stream into its first value and the new stream produced by de-thunking the tail
(: stream-unfold (-> stream (values Natural stream)))
(define (stream-unfold st)
  (values (stream-first st) ((stream-rest st))))
