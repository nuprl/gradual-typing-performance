#lang racket/base

(module t typed/racket
  (define-type Stream
   (-> (U 'obs 'nxt)
       (U (Pairof 'obs (-> Natural))
          (Pairof 'nxt (-> Stream)))))

  (define (stream-nxt (s : Stream)) : (-> Stream)
    (let ([r (s 'nxt)])
      (if (eq? 'nxt (car r))
        (cdr r)
        (error 'key))))

  (define (stream-obs (s : Stream)) : (-> Natural)
    (let ([r (s 'obs)])
      (if (eq? 'obs (car r))
        (cdr r)
        (error 'key))))

  (: from (-> Natural Stream))
  (define (from n)
   (lambda ([msg : Symbol])
    (if (eq? msg 'obs)
      (cons 'obs (lambda () n))
      (cons 'nxt (lambda () (from (+ n 1)))))))

  (define nats (from 0))

  (provide nats stream-nxt stream-obs)
)
(require 't)

(define (main N)
  (for/fold ([w nats])
            ([ii (in-range N)])
    ((stream-nxt w)))
  (void))

;; 2 wraps per iteration
(for ([i (in-range 60 100 50)])
  (time (main i)))

