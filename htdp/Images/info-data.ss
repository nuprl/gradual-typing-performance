#lang scheme

#|
I've finally found an approach to curve drawing for picts that I think
works well, so I've added it to `slideshow'.

The `pin-line' function now takes an angle for the exit and entry
points (defaulting to the angle of a straight line between the points),
and it also accepts a "pull" for each point (defaulting to 1/4 the
distance between the points).

So, if you have a `pin-line' call, but you want the line to start out
going straight to the right, then you just and add `#:start-angle 0' to
the call. The line will start straight to the right, then curve around
to hit the end point. Similarly, if you want the line to end going
straight down, add `#:end-angle (* pi -1/2)'.

Each angle and pull is really a polar-coordinate vector specifying the
displacement to a Bezier control point from each end point. In
retrospect, this is a completely obvious interface (at least now that
we have a drawing operation for Bezier curves).
|#

(require slideshow mred/mred "pict-aux.ss")

;; string[filename] pict -> pict
(define (write-ps-and-return filename the-pict)
  (let ([ps-setup (new ps-setup%)])
    (send ps-setup copy-from (current-ps-setup))
    (send ps-setup set-file filename)
    (let ([ps-dc (parameterize ((current-ps-setup ps-setup))
                   (make-object post-script-dc% #f #f #f #t))])
      (send ps-dc start-doc "")
      (send ps-dc start-page)
      (draw-pict the-pict ps-dc 0 0)
      (send ps-dc end-page)
      (send ps-dc end-doc)
      the-pict)))

;; String -> Pict
(define (bubble lbl)
  (define name:text  (t lbl))
  (cc-superimpose (ellipse (+ (pict-width name:text) 10) 30) name:text))

(define width 150)
(define height 150)

(define wall (cc-superimpose (vline 2 height) (blank width height)))
(define info (bubble "Information"))
(define data (bubble "Data"))

(define base
  (vc-append (hc-append (t "domain:") (blank width 1) (t "program:"))
             (hc-append info wall data)))

;; Pict Pict (Pict Pict ->* Nat Nat) Number Pict (Pict Pict ->* Nat Nat) Number String Number -> Pict

(define (my-add-labeled-arrow base from from-find sa to to-find ea txt off)
  (define FT 12)
  (define-values (x0 y0) (from-find base from))
  (define-values (x1 y1) (to-find base to))
  (define xc (quotient (+ x0 x1) 2))
  (define yc (quotient (+ y0 y1) 2))
  (define lbl (t-small txt))
  (define x (- xc (quotient (pict-width lbl) 2)))
  (define y (+ (- yc (quotient (pict-height lbl) 2)) off))
  (pin-over (pin-arrow-line 5.0 base from from-find to to-find  #:start-angle sa #:end-angle ea) x y lbl))
  
(define line1
  (my-add-labeled-arrow base info rc-find (* 1/6 pi) data lc-find (* -1/6 pi) #;(* 11/6 pi) "represent" -25))

(define line2
  (my-add-labeled-arrow line1 data lc-find  (* 7/6 pi) info rc-find (* -7/6 pi) "interpret" 25))
line2
