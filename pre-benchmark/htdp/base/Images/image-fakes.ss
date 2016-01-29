#lang scheme

(require (prefix-in sp: slideshow/pict) #;"write.ss" #;"pict-aux.ss")

(provide (all-defined-out))

; (require 2htdp/universe)

(define (place-image i x y j) (sp:pin-over j x y i))

(define (nw:rectangle w h mode color)
  (define r ((if (string=? mode "solid") sp:filled-rectangle sp:rectangle) w h))
  (sp:colorize r color))

(define rectangle nw:rectangle)

(define (triangle s mode color)
  (define t (sp:arrowhead s (/ pi 2)))
  (sp:colorize t color))

(define (circle r mode color)
  (define s (* 2 r))
  (define c ((if (string=? mode "solid") sp:filled-ellipse sp:ellipse) s s))
  (sp:colorize c color))

(define (empty-scene w h)
  (sp:rectangle w h))

(define overlay sp:cc-superimpose)