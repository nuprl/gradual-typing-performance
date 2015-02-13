#lang racket 

(provide 
 ;; Image 
 ;; labeled trapezoids
 trapezoid trapezoid-1
 ;; Plot
 ;; a rocket acceleration 
 constant
 ;; Plot
 ;; a rocket acceleration 
 accelerate
 ;; Plot 
 ;; an integration sketch
 integrate
 ;; Plot 
 ;; an integration sketch
 adaptive)

(require plot (except-in 2htdp/image line))

;; -----------------------------------------------------------------------------

(define-values (trapezoid trapezoid-1)
  (local ((define width 200)
          (define tri-height 100)
          (define rect-height 100)
          (define font-size 20)
          ;; ---
          (define trap
            (above (right-triangle width tri-height 'outline 'red)
                   (rectangle width rect-height 'outline 'red)))
          
          (define trap-1 (flip-horizontal trap))
          (define atxt "L")
          (define btxt "R")
          (define a (text atxt font-size 'black))
          (define b (text btxt font-size 'black))
          (define f@a (text (format "f(~a)" atxt) font-size 'black))
          (define f@b (text (format "f(~a)" btxt) font-size 'black))
          (define h-blank (rectangle (- width (+ (image-width a) (image-width b))) 10 'solid 'white))
          (define v-blank (square 10 'solid 'white))
          
          (define shape 
            (overlay/xy f@b (- (image-width f@b) 4 width) (- 10 tri-height) 
                        (overlay/xy f@a 0 (image-height f@a)
                        (above trap 
                               v-blank 
                               (beside a h-blank b)))))
          (define shape-1
            (overlay/xy f@b (- (image-width f@b) 4 width) (image-height f@b) 
                        (overlay/xy f@a 0 (- 30 tri-height)
                        (above trap-1
                               v-blank 
                               (beside a h-blank b))))))
    (values (scale .8 shape) (scale .8 shape-1))))

; (list trapezoid trapezoid-1)

;; -----------------------------------------------------------------------------

(define (f x) (+ (* .09  (- x -8) (- x 2) (- x 4)) 20))

(define ax -2)
(define y-low 0)
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
   (list (vector mx (+ y-low #; Δ))
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

(define (K x) (lambda (y) x))
(define zero (K 0))

(define f-plot (function f))
(define f-area (function-interval zero f ax bx))

(define integrate
  (plot (list (axes)
              f-plot
              f-area
              ; (function-interval zero (K (f ax)) ax (+ ax 5) #:line1-style 'dot)
              (point-label (vector ax (f ax)) "f(a)")
              ; (point-label (vector mx (f mx)) "f(mid)")
              (point-label (vector bx (f bx)) "f(b)")
              ; range-2 (point-label (vector ax (+ (- y-hi Δ) 5)) "range 2")
              ; range-1 (point-label (vector mx (+ y-hi 5)) "range 1")
              ; mid (point-label (vector mx -5 #;(+ y-low Δ)) "m")
              a (point-label (vector ax -5 #;y-low) "a")
              b (point-label (vector bx -5 #;y-low) "b"))
        #:x-min -10 #:x-max +10))

; integrate

#;
(define accelerate
  (plot (list (axes)
              (function ))
        #:x-min 0 #:x-max 20))

(define constant
  (plot
   (list (axes)
         (function-interval zero (lambda (t) (* 2 t)) 0 20
                            #:line2-style 'dot #:line2-width 4 #:label "d")
         (function-interval zero (lambda (t) 2)  #:line2-width 4 #:label "v"))
   #:x-min 0 
   #:x-max 20
   #:width 250	 
   #:height 200
   #:title "constant speed"))

(define accelerate 
  (plot
   (list (axes)
         (function-interval zero (lambda (t) (* 1/2 (- 12 9) t t)) 0 20 
                            #:line2-style 'dot #:line2-width 4 #:label "d")
         (function-interval zero (lambda (t) (* 2 t))  #:line2-width 4 #:label "v"))
   #:x-min 0 
   #:x-max 20
   #:width 250	 
   #:height 200
   #:title "increasing speed"))

; accelerate
; constant

(define (sin-curve x)
  (cond
    [(< x 5) 5]
    [else (+ 5 (* .5 (- x 5)) (sin (* 10 (- x 5))))]))

(define adaptive (plot (function-interval zero sin-curve) #:x-min 0 #:x-max 10))