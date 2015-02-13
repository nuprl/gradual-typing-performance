#lang racket

(provide
 ;; Image -> Image 
 ;; wrap 2 white pixels around the given image
 large-frame 

 ;; syntax: (build-scene scene:id scene0:exp e:exp ...) 
 ;; monadically builds a scene sequentially from a series of image expressions
 build-scene)

;; ---------------------------------------------------------------------------------------------------

(require 2htdp/image)

(define (large-frame i)
  (define w (image-width i))
  (define h (image-height i))
  (overlay i (rectangle (+ w 1) (+ h 1) "solid" "white")))
  
(define-syntax-rule
  (build-scene scene scene0 e ...)
  (let* ([scene scene0]
         [scene e]
         ...)
    scene))

