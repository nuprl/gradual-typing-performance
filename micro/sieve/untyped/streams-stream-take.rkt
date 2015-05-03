#lang racket/base

(provide stream-take)
(require benchmark-util
         "streams-struct.rkt"
"streams-stream-unfold.rkt")

;; [stream-take st n] Collect the first [n] elements of the stream [st].
;(: stream-take (-> stream Natural (Listof Natural)))
(define (stream-take st n)
  (cond [(= n 0) '()]
        [else (define-values (hd tl) (stream-unfold st))
              (cons hd (stream-take tl (sub1 n)))]))
