#lang typed/racket

(provide
 ;; X X -> X
 one-of
 
 ;; [Listof X] Y [X -> Y] [X -> Z] -> '() or Z
 apply-to-first
 
 ;; [Listof Number] -> Number 
 sum
 
 ;; [Listof Number] Number -> Number 
 relative-average
 )

(: sum (-> [Listof Real] Real))
(define (sum l)
  (apply + l))

(: relative-average (-> [Listof Real] Real Real))
(define (relative-average l w)
  (cast (exact->inexact
         (/ (sum l)
            w (length l)))
        Real))

;; X *-> X
#;
(define (one-of . x)
  (list-ref x (random (length x))))

(: one-of (All (a) (-> a a a)))
(define (one-of x y)
  (if (= (random 2) 0) x y))

(: apply-to-first (All (a b c) (-> [Listof a] b [-> a b] [-> a c] (U '() c))))
(define (apply-to-first l x sel f)
  (define result
    (filter (lambda ({y : a}) (equal? x (sel y))) l)
    #;
    (for/first ([y : (Listof a) (in-list l)] #:when (equal? x (sel y))) y))
  (if (empty? result) (f (first result)) '()))
