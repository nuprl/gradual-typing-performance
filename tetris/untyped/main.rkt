#lang racket

(require "visual.rkt" "world.rkt" "bset.rkt" "data.rkt")

(define (replay w0 hist)
  (for/fold ([w w0]) ([e hist])
    (match e
      [`(on-key ,ke) (world-key-move w ke)]
      [`(on-tick) (next-world w)]
      [`(to-draw) (world->image w) w]
      [`(stop-when)
       (λ (w) (blocks-overflow? (world-blocks w)))
       w])))

(module+ main
  (define w0 (world0))
  (define hist (reverse (with-input-from-file "tetris-hist-3.txt" read)))

  (replay w0 hist))
   
