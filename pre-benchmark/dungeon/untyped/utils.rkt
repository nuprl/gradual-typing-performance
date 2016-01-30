#lang racket

(provide (all-defined-out))

(define (article capitalize? specific?
                 #:an? [an? #f])
  (if specific?
      (if capitalize? "The" "the")
      (if an?
          (if capitalize? "An" "an")
          (if capitalize? "A"  "a"))))

(define (random-between min max) ;; TODO replace with 6.4's `random`
  (+ min (random (- max min))))
(define (d6)
  (random-between 1 7))
(define (d20)
  (random-between 1 21))

(define (random-from l)
  (first (shuffle l)))
