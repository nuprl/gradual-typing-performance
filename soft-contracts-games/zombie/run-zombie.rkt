#lang racket  

(require (prefix-in z: "zombie.rkt"))

(define z:h (reverse (with-input-from-file "zombie-hist-4.txt" read)))

(define (contract-main)
  (z:replay z:w1 z:h))

(time (begin (contract-main) (void)))
