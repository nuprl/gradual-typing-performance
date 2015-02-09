#lang slideshow

(require "box-diagram.ss")

(draw-box "peterlee" 
          "entry"
          '(("name" "Sarah Lee") 
            ("phone" "666-7771") 
            ("email" "lee@classy-university.edu")))

(draw-box "bobh" 
          "entry"
          '(("name" "Tara Harper") 
            ("phone" "666-7770") 
            ("email" "harper@small-college.edu")))

(define p (struct-pict "posn" `(("x" "30") ("y" "40"))))
(define v (struct-pict "vel" `(("deltax" "-10") ("deltay" "+5"))))
(scale (draw-box "2dball" "ball" `(("loc" ,p) ("velocity" ,v))) 1.5)
