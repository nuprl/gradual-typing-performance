#lang racket

(provide 
 ;; -> Image
 solitaire-stuck
 solitaire-enabled
 solitaire-jumped
 solitaire-5)

;; -----------------------------------------------------------------------------
(require "ysupport.rkt" slideshow)

(define hole-radius 21)
(define peg-radius  (* .66 hole-radius))

;; N -> Pict
(define (solitaire-board n . holes)
  (define rows
    (build-list n (lambda (i) 
                    (define pegs 
                      (build-list
                       (add1 i)
                       (lambda (j) 
                         (define hole (circle hole-radius))
                         (define peg  (filled-ellipse peg-radius peg-radius))
                         (if (member (list i j) holes)
                             hole
                             (cc-superimpose peg hole)))))
                    (apply hc-append pegs))))
  (define base (apply vc-append rows))
  (define frame
    (colorize (rectangle (* 2 (pict-width base)) (pict-height base)) "white"))
  (cc-superimpose frame base))

(define (solitaire-stuck)
  (solitaire-board 4 '(2 1)))

(define (solitaire-enabled)
  (define (point s)
    (define txt (text s "black" 16))
    (define r (+ (max (pict-width txt) (pict-height txt)) 3))
    (colorize (cc-superimpose txt (circle r)) "gray"))
  (define p1 (point "2"))
  (define p2 (point "1"))
  (define δ 40)
  (define *x 0)
  (define *y 0)
  (build-scene 
   s (solitaire-board 4 '(2 2))
   (pin-over s 0 0 p1)
   (pin-arrow-line 
    10 s
    p1 (lambda (_1 _2) (values hole-radius (/ hole-radius 2)))
    p1 (lambda (_1 _2) (values (* 3.5 hole-radius) (/ hole-radius 2))))
   (pin-over s 0 (* 2 hole-radius) p2)
   (pin-arrow-line 
    10 s
    p1 (lambda (_1 _2) (values hole-radius (* 2.5 hole-radius)))
    p1 (lambda (_1 _2) (values (* 2.5 hole-radius) (* 2.5 hole-radius))))))

(define (solitaire-jumped)
  (solitaire-board 4 '(1 1) '(0 0)))

(define (solitaire-5)
  (solitaire-board 5 '(3 1)))

;; --- view 
; (solitaire-stuck)
; (solitaire-enabled)
; (solitaire-jumped)
; (solitaire-5)
