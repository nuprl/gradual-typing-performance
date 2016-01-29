#lang racket/base

(provide stream-get)
(require benchmark-util
         "streams-struct.rkt"
"streams-stream-unfold.rkt")

;; [stream-get st i] Get the [i]-th element from the stream [st]
;(: stream-get (-> stream Natural Natural))
(define (stream-get st i)
  (define-values (hd tl) (stream-unfold st))
  (cond [(= i 0) hd]
        [else    (stream-get tl (sub1 i))]))

