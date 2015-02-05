#lang typed/racket/base

;; Simple streams library.
;; For building and using infinite lists.

(provide stream
         make-stream
         stream-unfold
         stream-get
         stream-take)

;; ;; A stream is a cons of a value and a thunk that computes the next value when applied
(struct: (A) stream ([first : A] [rest : (-> (stream A))]))

;;--------------------------------------------------------------------------------------------------

(: make-stream (All (A) (-> A (-> (stream A)) (stream A))))
(define (make-stream hd thunk)
  (stream hd thunk))

;; Destruct a stream into its first value and the new stream produced by de-thunking the tail
(: stream-unfold (All (A) (-> (stream A) (values A (stream A)))))
(define (stream-unfold st)
  (values (stream-first st) ((stream-rest st))))

;; [stream-get st i] Get the [i]-th element from the stream [st]
(: stream-get (All (A) (-> (stream A) Natural A)))
(define (stream-get st i)
  (define-values (hd tl) (stream-unfold st))
  (cond [(= i 0) hd]
        [else    (stream-get tl (sub1 i))]))

;; [stream-take st n] Collect the first [n] elements of the stream [st].
(: stream-take (All (A) (-> (stream A) Natural (Listof A))))
(define (stream-take st n)
  (cond [(= n 0) '()]
        [else (define-values (hd tl) (stream-unfold st))
              (cons hd (stream-take tl (sub1 n)))]))
