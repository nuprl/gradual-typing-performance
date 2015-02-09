#lang racket

(require (only-in lang/htdp-intermediate posn?))

(provide
 ;; N [List-of QPosn] Image -> Image 
 ;; assume if q is in QPosn, (<= 0 (posn-x q) (- n 1)) and (<= 0 (posn-y q) (- n 1))
 ;; (render n q* i) draws an n by n board and places the queens in q* using i as the queen's image
 (contract-out 
  [render (->i ((n natural-number/c)
                (q* (listof posn?))
                (i image?))
               #:pre (n q*) (for/and ((q q*)) (and (< (posn-x q) n) (< (posn-y q) n)))
               (r image?))])
 
 ;; Image: images of queens
 BLACK WHITE 
 
 ;; Image: a sketch why a 3 x 3 board does not have an n-queens solution
 3queens-1 3queens-2 3queens-3
 
 ;; Image: n-queens solutions for 4 x 4 and 5 x5 boards 
 4queens 4queens-other 5queens
 
 ;; Image: an N-queens board with 'threatened' diagonals
 queen-threatens)

;; ---------------------------------------------------------------------------------------------------
;; implenentation 

(require "xsupport.rkt" 2htdp/image (only-in lang/htdp-intermediate-lambda make-posn posn-x posn-y))

(define BLACK (scale .5 (bitmap "fig-queens-black.png")))
(define WHITE (scale .5 (bitmap "fig-queens.png")))

(define (render n queens queen-image)
  (local ((define S (square (width queen-image) "outline" "black"))
          (define (make-cells i)
            (local ((define (make-one-cell j)
                      (if (member? (make-posn i j) queens) (overlay queen-image S) S)))
              (foldr beside empty-image (build-list n make-one-cell)))))
    (foldr above empty-image (build-list n make-cells))))

(define member? member)

;; Image -> Number 
;; compute the side length of a square of the chess board, relative to an image of a queen
(define (width image)
  (+ (image-width image) 5))

;; ---------------------------------------------------------------------------------------------------
;; the exported images 

(define 3queens-1
  (large-frame (render 3 (list (make-posn 0 0) (make-posn 1 2)) BLACK)))
(define 3queens-2
  (large-frame (render 3 (list (make-posn 1 0) (make-posn 0 2)) BLACK)))
(define 3queens-3
  (large-frame (render 3 (list (make-posn 2 0) (make-posn 1 2)) BLACK)))

(define 4queens
  (large-frame
   (render 4 (list (make-posn 3 2) (make-posn 2 0) (make-posn 1 3) (make-posn 0 1)) WHITE)))

(define 4queens-other
  (large-frame
   (render 4 (list (make-posn 1 0) (make-posn 3 1) (make-posn 0 2) (make-posn 2 3)) BLACK)))

; (list (make-posn 2 0) (make-posn 0 1) (make-posn 3 2) (make-posn 1 3))

(define 5queens
  (large-frame
   (render 5 (list (make-posn 0 0) (make-posn 1 2) (make-posn 3 1) (make-posn 4 3) (make-posn 2 4)) 
           BLACK)))

(define queen-threatens
  (large-frame
   (local ((define w (width BLACK))
           (define p (make-pen 'red 3.0 'solid 'round 'bevel)))
     (build-scene 
      eight (render 8 (list (make-posn 5 1)) BLACK)
      #; "sw->ne diagonal"
      (add-line eight 0 (* w 7) (* w 7) 0 p)
      #; "nw->se diagonal"
      (add-line eight 0 (* w 4) (* w 4) (* w 8) p)
      #; "horizontal"
      (add-line eight 0 (* w 5.5) (* w 8) (* w 5.5) p)
      #; "vertical"
      (add-line eight (* w 1.5) 0 (* w 1.5) (* w 8) p)))))

;; ---------------------------------------------------------------------------------------------------

(module old-code racket 
  (require 
    (only-in slideshow rotate rectangle filled-rectangle cc-superimpose vc-append 
             hc-append filled-ellipse pin-over clip))
  
  (define n-queens
    (local ((define SIZE 30)
            (define N 8)
            (define F 5)
            (define BAR-WIDTH (quotient SIZE F))
            
            (define hbar (filled-rectangle (* N SIZE) BAR-WIDTH))
            (define horizontals
              (for/list ((i 8)) 
                (define h (rectangle (* N SIZE) SIZE))
                (if (= i 5)
                    (cc-superimpose h hbar)
                    h)))
            
            (define vbar (filled-rectangle BAR-WIDTH (* N SIZE)))
            (define verticals 
              (for/list ((i 8))
                (define v (rectangle SIZE (* N SIZE)))
                (if (= i 1)
                    (cc-superimpose v vbar)
                    v)))
            
            (define grid
              (cc-superimpose
               (apply vc-append horizontals)
               (apply hc-append verticals)))
            
            (define R 20)
            (define dot (filled-ellipse R R))
            (define grid+dot 
              (pin-over grid
                        (+ SIZE (quotient SIZE 2) (- (quotient R 2))) 
                        (+ SIZE (* 9 (quotient SIZE 2)) (- (quotient R 2)))
                        dot))
            
            (define (diagonal theta)
              (rotate (filled-rectangle (* (+ 4 N) SIZE) BAR-WIDTH) theta)))
      
      (clip
       (pin-over (pin-over grid+dot -10 -40 (diagonal (/ pi 4))) -20 100 (diagonal (/ pi -4)))))))
