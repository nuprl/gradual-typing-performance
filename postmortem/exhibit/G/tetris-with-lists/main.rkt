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

(define LARGE_TEST "tetris-hist-large.rktd")

(define (main filename)
  (define w0 (world0))
  (define raw (with-input-from-file filename read))
  (unless (list? raw) (error "bad input"))
  (replay w0 (reverse raw)))

(require contract-profile)
(contract-profile-thunk (lambda () (main LARGE_TEST)))
