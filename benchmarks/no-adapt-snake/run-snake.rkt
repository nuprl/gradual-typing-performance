#lang racket

(require (prefix-in s: "snake.rkt"))

(define s:h4 (reverse (with-input-from-file "snake-hist-4.txt" read)))

(define (contract-main)
  (s:replay s:w0 s:h4))

(time (begin (contract-main) (void)))
