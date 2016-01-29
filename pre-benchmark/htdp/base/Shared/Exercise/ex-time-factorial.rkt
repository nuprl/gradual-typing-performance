#lang racket

; (check-expect (! 0) 1)
; (check-expect (! 3) 6)

(define (! n)
  (cond
    [(zero? n) 1]
    [else (* n (! (sub1 n)))]))

; (check-expect (!.v2 0) 1)
; (check-expect (!.v2 3) 6)

(define (!.v2 n0)
  (local ((define (!/a n a)
            (cond
              [(zero? n) a]
              [else (!/a (sub1 n) (* a n))])))
    (!/a n0 1)))

(define (run !)
  (collect-garbage) (collect-garbage) (collect-garbage)
  (time (for ((i (in-range 1000000))) (! 30)))
  (void))

(run !)
(run !.v2)