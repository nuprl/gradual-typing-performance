#lang racket

#|
(require block bset data elim-row elim tetras visual image world)
|#

(require "visual.rkt" "world.rkt" "bset.rkt" "data.rkt")
(require 2htdp/universe)

(define history empty)
(define (! e)
  (set! history (cons e history)))

(define (play)
  (big-bang (world0)
            (on-tick (λ (w) (! `(on-tick)) (next-world w)) 1/5)
            (on-key (λ (w ke) (! `(on-key ,ke)) (world-key-move w ke)))
            (to-draw (λ (w) #;(! `(to-draw)) (world->image w)))
            (stop-when (λ (w) (! `(stop-when)) (blocks-overflow? (world-blocks w))))))

(define (start w)
  (big-bang w
            (on-tick next-world 1/5)
            (on-key world-key-move)
            (to-draw world->image)
            (stop-when (compose blocks-overflow? world-blocks))))

#;
(with-output-to-file "tetris-hist-3.txt"
  (λ ()
    (set! history empty)
    (with-output-to-file "tetris-sanity.txt" (lambda () (write (play))))
    (write history)))
    

(define (replay w0 hist)
  (for/fold ([w w0]) ([e hist])
    #;(printf "~a~n" e)
    (match e
      [`(on-key ,ke) (world-key-move w ke)]
      [`(on-tick) (next-world w)]
      [`(to-draw) (world->image w) w]
      [`(stop-when)
       (λ (w) (blocks-overflow? (world-blocks w)))
       w])))

(define w0 (world0))
(provide replay w0 start)


(module+ main
  (play))
