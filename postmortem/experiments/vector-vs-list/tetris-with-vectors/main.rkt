#lang racket

(require "aux.rkt" "world.rkt" "bset.rkt" "data.rkt")

(define (world0)
  (world (vector-pick-random tetras) '#()))

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

(define (vector-reverse vec)
  (define L (vector-length vec))
  (build-vector L (lambda (i) (vector-ref vec (- L 1 i)))))

(define (main filename)
  (define w0 (world0))
  (define raw (with-input-from-file filename read))
  (unless (vector? raw) (error "bad input"))
  (replay w0 (vector-reverse raw)))

(require contract-profile)
(contract-profile-thunk (lambda () (main LARGE_TEST)))
