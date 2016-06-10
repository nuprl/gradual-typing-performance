#lang typed/racket/base

(provide
  article
  random-between
  d6
  d20
  random-from
  random
)

(require
  (only-in racket/list first)
  (only-in racket/file file->value)
)

;; =============================================================================

(: random (-> Integer Natural))
(define random
  (let ([r* (box '(2 10 24 3 0 2 10 45 2 2 2 2 49 3 1 5 1 0 0 2 1 0 2 1 0 0 2 2 5 0 0 0 3 0 1 2 0 3 0 0 2 2 0 2 2 0 0 3 0 0 2 0 3 1 0 2 0 0 1 1 0 2 0 0 3 0 0 1 2 0 3 1 0 2 0 0 0 1 3 1 1 0 1 2 0 3 2 0 1 2 0 1 1 0 2 2 0 1 1 0 2 2 0 0 0 2 1 0 0 0 0 3 4 0 0 2 1 0 2 1 0 3 1 0 1 0 0 1 0 0 1 2 0 1 0 0 2 2 0 2 2 0 3 1 0 1 0 0 1 1 0 2 1 0 3 2 0 3 0 0 2 2 0 0 0 3 4 2 0 3 0 0 3 1 0 0 3 0 4 0 0 2 0 0 2 2 0 2 1 0 0 0 3 6 1 0 3 0 0 0 2 1 3 0 0 3 1 0 1 1 0 2 0 0 3 2 0 2 1 0 1 2 0 0 3 0 2 2 0 2 2 0 2 2 0 1 1 0 3 1 0 2 1 0 1 2 0 0 2 0 3 1 0 1 1 0 2 2 0 2 2 0 1 5 3 3 2 1))])
    (lambda ([n : Integer])
      (begin0 (car (unbox r*)) (set-box! r* (cdr (unbox r*)))))))

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

(: shuffle (All (A) (-> (Listof A) (Listof A))))
(define (shuffle l)
  (reverse l))
