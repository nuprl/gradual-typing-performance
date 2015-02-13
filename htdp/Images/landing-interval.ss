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

(require slideshow mred/mred "write.ss" "pict-aux.ss")

;; String -> Pict
(define (bubble lbl)
  (define name:text  (t lbl))
  (cc-superimpose (ellipse (+ (pict-width name:text) 10) 30) name:text))

(define down (ht-append (filled-rectangle 1 5) (filled-rectangle 20 3) (filled-rectangle 1 5)))
(define up*   (hb-append (filled-rectangle 1 5) (filled-rectangle 20 3) (filled-rectangle 1 5)))
(define up   (hb-append (filled-rectangle 1 5) (filled-rounded-rectangle 20 3) (filled-rectangle 1 5)))


(define (vnumberline hi lo mi)
  (define FT 12)
  (define up_0 (hb-append (filled-rectangle 1 5) (filled-rectangle 20 3) (filled-rectangle 1 5)))
  (define hi-lbl (t-small hi))
  (define lo-lbl (t-small lo))
  (define mi-lbl (t-small mi))
  (define w (max (pict-width hi-lbl) (pict-width lo-lbl) (pict-width mi-lbl)))
  (define HI (lc-superimpose (blank w 1) hi-lbl))
  (define LO (lc-superimpose (blank w 1) lo-lbl))
  (define MI (lc-superimpose (blank w 1) mi-lbl))
  (define width  (* 3 (pict-width HI)))
  (define height 150)
  (define up-down (hc-append (blank 47 1) (vc-append up (blank 1 1) down) (blank 10 1) MI))
  [define t (vline width height)]
  [define s (ct-superimpose 
             t
             (apply vc-append 
                    down
                    ; (blank 10 5) 
                    (append 
                     (build-list (- (/ height 10) 0)
                                 (lambda (i)
                                   (cond
                                     [(= i 10) up-down]
                                     [(= i 11) (blank 1 1)]
                                     [else (hline 10 10)])))
                     (list up_0))))]
  [define a (pin-arrow-line 5.0 s s ct-find up_0 ct-find)]
  [define b (cc-superimpose (blank width height) a)]
  [define nl (rt-superimpose LO (rb-superimpose HI b))]
  (frame nl))

(vnumberline "HEIGHT" "0" "CLOSE")
