#lang scheme
(require slideshow)

;; Positive Positive Positive -> Pict
;; create a right interval end from the black radius, white radius and a blank rectangle of width δ and height 0
(define (crescent r1 r2 δ)
  (rc-superimpose 
   (colorize (filled-ellipse r1 r1) "black")
   ;; shift white circle to the right 
   (hc-append (colorize (filled-ellipse r2 r2) "white") (blank δ 0))))

(crescent 10 12 3)

'blank
(blank 10 0)
'white
(colorize (filled-ellipse 30 30) "white")
'black
(colorize (filled-ellipse 10 10) "black")
