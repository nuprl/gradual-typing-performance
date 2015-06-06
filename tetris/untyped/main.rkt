#lang racket

(require "aux.rkt" "world.rkt" "bset.rkt" "data.rkt")

(define (world0)
  (world (list-pick-random tetras) empty))

(define (replay w0 hist)
  (for/fold ([w w0]) ([e hist])
    (match e
      [`(on-key ,ke) (world-key-move w ke)]
      [`(on-tick) (next-world w)]
      [`(stop-when)
       (λ (w) (blocks-overflow? (world-blocks w)))
       w]))
  (void))

(time
 (define w0 (world0))
 (define raw (with-input-from-file "../base/tetris-hist-3.txt" read))
 (when (list? raw)
   (replay w0 (reverse raw))))
   
