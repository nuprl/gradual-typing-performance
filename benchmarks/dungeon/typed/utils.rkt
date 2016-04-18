#lang typed/racket/base

(provide
  article
  random-between
  d6
  d20
  random-from
)

(require
  (only-in racket/list shuffle first)
)

;; =============================================================================

(: article (->* (Boolean Boolean) (#:an? Boolean) String))
(define (article capitalize? specific?
                 #:an? [an? #f])
  (if specific?
      (if capitalize? "The" "the")
      (if an?
          (if capitalize? "An" "an")
          (if capitalize? "A"  "a"))))

(: random-between (-> Integer Integer Integer))
(define (random-between min max) ;; TODO replace with 6.4's `random`
  (+ min (random (- max min))))

(: d6 (-> Integer))
(define (d6)
  (random-between 1 7))

(: d20 (-> Integer))
(define (d20)
  (random-between 1 21))

(: random-from (All (A) (-> (Listof A) A)))
(define (random-from l)
  (first (shuffle l)))
