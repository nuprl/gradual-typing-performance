#lang racket

(provide 
 ;; -> Image
 ;; binary trees of n levels 
 0-level-tree
 1-level-tree
 3-level-tree
 3-level-arrows)

;; -----------------------------------------------------------------------------
(require "ysupport.rkt" slideshow)

(define (interior) (filled-ellipse 10 10))
(define (leaf) (ellipse 10 10))

(define width 100)
(define arr-wgt 10)

(define mt interior)

(define (x-B x0)
  (- x0 40))

(define (y-B y0)
  (- y0 65))

(define (x-C x0)
  (+ x0 40))

(define (y-C y0)
  (y-B y0))

;; Image Number Number -> Image
;; (x0,y0) is where we want to add a branch 
(define (fork s0 x0 y0)
  (define xB (x-B x0))
  (define yB (y-B y0))
  (define xC (x-C x0))
  (define yC (y-C y0))
  [define A (interior)]
  [define B (leaf)]
  [define C (leaf)]
  (build-scene 
   scene s0
   (pin-over scene x0 y0 A)
   (pin-over scene xB yB B)
   (pin-over scene xC yC C)
   (pin-line scene A ct-find B cb-find)
   (pin-line scene A ct-find C cb-find)))

(define (0-level-tree)
  (pin-over (rectangle (* 2 width) 25) 90 7.5 (interior)))

(define (1-level-tree)
  (define height 100)
  (fork (rectangle (* 2 width) height) 90 (- height 25)))

(define (3-level-tree)
  (define height 225)
  (define y0 (- height 25))
  (define xB (x-B 90))
  (define yB (y-B y0))
  (define xC (x-C xB))
  (define yC (y-C yB))
  
  (build-scene 
   s (rectangle (* 2 width) height)
   (fork s 90 y0)
   (fork s xB yB)
   (fork s xC yC)))

(define (3-level-arrows)
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
   s (3-level-tree)
   (pin-over s (+ 100 δ) 65 p1)
   (pin-arrow-line 10 s 
                   p1 
                   (lambda (p q)
                     (define-values (x y) (lc-find p q))
                     (set! *x x)
                     (set! *y y)
                     (values x y))
                   p1
                   (lambda (_1 _2) (values (- *x δ) *y)))
   (pin-over s (+ 60 δ) 130 p2)
   (pin-arrow-line 10 s 
                   p2
                   (lambda (p q)
                     (define-values (x y) (lc-find p q))
                     (set! *x x)
                     (set! *y y)
                     (values x y))
                   p2
                   (lambda (_1 _2) (values (- *x δ) *y)))))

; (3-level-arrows)
               