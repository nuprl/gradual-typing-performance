#lang racket

(provide 
 ;; Image 
 ;; connected graph 
 the-graph
 
 the-cyclic
 
 simple-graph)

(require "ysupport.rkt" slideshow)

(def/dots A B C D E F G)

(define width  600)
(define height 200)
(define Yhigh   10)
(define Ylow   (- height (pict-height A) 10))
(define X0      10)
(define Xdelta 180)
(define arr-wgt 10)

(define scene
  (build-scene 
   scene (rectangle width height)
   (pin-over scene X0 50 A)
   (pin-over scene (+ X0 (* 1 Xdelta)) Yhigh B)
   (pin-over scene (+ X0 (* 1 Xdelta)) Ylow E)
   (pin-over scene (+ X0 (* 2 Xdelta)) Yhigh C)
   (pin-over scene (+ X0 (* 2 Xdelta)) Ylow F)
   (pin-over scene (+ X0 (* 3 Xdelta)) Yhigh D)
   (pin-over scene (+ X0 (* 3 Xdelta)) Ylow G)
   (pin-arrow-line arr-wgt scene A rc-find B lc-find)
   (pin-arrow-line arr-wgt scene A rc-find E lc-find)
   (pin-arrow-line arr-wgt scene B cb-find E ct-find)
   (pin-arrow-line arr-wgt scene B cb-find F lc-find)
   (pin-arrow-line arr-wgt scene C rc-find D lc-find)
   (pin-arrow-line arr-wgt scene E rc-find C lc-find)
   (pin-arrow-line arr-wgt scene E rc-find F lc-find)
   (pin-arrow-line arr-wgt scene F rc-find G lc-find)
   (pin-arrow-line arr-wgt scene F rc-find D lc-find)))

(define the-graph scene)

(define the-cyclic 
  (build-scene 
   scene scene
   (pin-arrow-line arr-wgt scene C lc-find B rc-find
                   #:start-angle (* 5/6 pi)
                   #:end-angle (* 7/6 pi))))

(define simple-graph
  (let* ([XDELTA 90]
         [xdelta (lambda (i) (+ 10 (* i XDELTA)))])
    (scale
      (build-scene 
	scene (rectangle 530 100)
	(pin-over scene (xdelta 0) 30 A)
	(pin-over scene (xdelta 1) 30 B)
	(pin-over scene (xdelta 2) 30 C)
	(pin-over scene (xdelta 3) 30 D)
	(pin-over scene (xdelta 4) 30 E)
	(pin-over scene (xdelta 5) 30 F)
	(pin-arrow-line arr-wgt scene A rc-find B lc-find)
	(pin-arrow-line arr-wgt scene B rc-find C lc-find)
	(pin-arrow-line arr-wgt scene C ct-find E ct-find
	  #:start-angle (* 1/6 pi) #:end-angle (* -1/6 pi))
	(pin-arrow-line arr-wgt scene D rc-find E lc-find)
	(pin-arrow-line arr-wgt scene E cb-find B cb-find  
	  #:start-angle (* -5/6 pi) #:end-angle (* 5/6 pi))
	(pin-arrow-line arr-wgt scene F rc-find F cb-find
	  #:start-angle (* 1/6 pi) 
	  #:start-pull 1.5
	  #:end-angle (* -7/6 pi)
	  #:end-pull 1.5))
      .66)))

