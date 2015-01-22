(module const typed/racket
  (require "image.rkt"
           "data.rkt")

  (define GRID-SIZE 30)

  (define BOARD-HEIGHT 20)

  (define BOARD-WIDTH 30)
  
  (define (BOARD-HEIGHT-PIXELS) (* GRID-SIZE BOARD-HEIGHT))

  (define (BOARD-WIDTH-PIXELS) (* GRID-SIZE BOARD-WIDTH))

  (define (BACKGROUND) (empty-scene (BOARD-WIDTH-PIXELS) (BOARD-HEIGHT-PIXELS)))

  (define (SEGMENT-RADIUS) (/ GRID-SIZE 2))
  (define (SEGMENT-IMAGE)  (circle (SEGMENT-RADIUS) "solid" "red"))
  (define (FOOD-RADIUS) (SEGMENT-RADIUS))
  (define (FOOD-IMAGE)  (circle (FOOD-RADIUS) "solid" "green"))
  (define (WORLD) (world (snake "right" (cons (posn 5 3) empty))
                         (posn 8 12)))
  
  (provide
   WORLD
   BACKGROUND
   FOOD-IMAGE
   SEGMENT-IMAGE
   GRID-SIZE
   BOARD-HEIGHT-PIXELS
   BOARD-WIDTH
   BOARD-HEIGHT))
