#lang racket 

(provide 
 ;; Plot 
 ;; poly plotted on [-1,5] to visualize the roots and the flatness at 3
 poly-root
 ;; Plot 
 ;; a function with a root between labeled interval [a,b] with midpoint
 f+interval)

(require plot)

(define (f x) (- (expt x 2) (* -10 (expt x 1)) 50))

(define f-plot (function f))

(define ax 2)
(define y-low -50)
(define y-hi +120)
(define a
  (lines
   (list (vector ax y-low)
         (vector ax y-hi))))
(define bx 8)
(define b
  (lines
   (list (vector bx y-low)
         (vector bx y-hi))))

(define Δ 20)
(define mx (/ (+ ax bx) 2))
(define mid
  (lines 
   (list (vector mx (+ y-low Δ))
         (vector mx (- y-hi Δ)))))

(define range-1 
  (lines 
   (list (vector ax y-hi)
         (vector bx y-hi))
   #:width 5))

(define range-2 
  (lines 
   (list (vector ax (- y-hi Δ))
         (vector mx (- y-hi Δ)))
   #:width 5))

(define f+interval
  (plot (list (axes)
              f-plot
              (point-label (vector ax (f ax)) "f(a)")
              (point-label (vector mx (f mx)) "f(mid)")
              (point-label (vector bx (f bx)) "f(b)")
              range-2 (point-label (vector ax (+ (- y-hi Δ) 5)) "range 2")
              range-1 (point-label (vector mx (+ y-hi 5)) "range 1")
              mid (point-label (vector mx -5 #;(+ y-low Δ)) "m")
              a (point-label (vector ax -5 #;y-low) "a")
              b (point-label (vector bx -5 #;y-low) "b"))
        #:x-min -10 #:x-max +10))

; f+interval

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

(define poly-root 
  (plot (list (axes #:x-labels? #t) (function poly))
        #:x-min -1 #:x-max +5 #:y-min -5 #:y-max 10))

; poly-root