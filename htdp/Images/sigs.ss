#lang scheme

(require "image-fakes.ss" "write.ss" #;"pict-aux.ss")
; (require 2htdp/universe)

;; physical constants 
(define WIDTH 100)
(define HEIGHT 100)

(define TANK-HEIGHT  6)
(define TANK-WIDTH  20)

;; graphical constants 
(define TANK (nw:rectangle TANK-WIDTH TANK-HEIGHT "solid" "blue"))
(define UFO (overlay (rectangle 20 2 "solid" "green")
                     (circle 3 "solid" "green")))
(define MISSILE (triangle 5 "solid" "red"))

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define (si-render xtank ytank xufo yufo (xm #f) (ym #f))
  (define +tank (place-image TANK xtank ytank BACKGROUND))
  (define +ufo (place-image UFO xufo yufo +tank))
  (define +missile (if xm (place-image MISSILE xm ym +ufo) +ufo))
  +missile)

(write-pict "sigs-aim" (si-render 28 (- HEIGHT TANK-HEIGHT) 20 10))
(write-pict "sigs-fired" (si-render 28 (- HEIGHT TANK-HEIGHT) 20 10 28  (- HEIGHT TANK-HEIGHT 10)))
(write-pict "sigs-fired2" (si-render 75 (- HEIGHT TANK-HEIGHT) 20 50 22 52))

#;
(make-fired 
  (make-tank 100 +3)
  (make-posn 20 100)
  (make-posn 22 103))
