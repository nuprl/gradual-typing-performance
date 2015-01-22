(module scenes typed/racket
  (require "data.rkt"
           "const.rkt"
           "image.rkt")
  
  ;; Build an image of the given world.
  (: world->scene : (World . -> . Image))
  (define (world->scene w)
    (snake+scene (world-snake w)
                 (food+scene (world-food w) (BACKGROUND))))
  
  ;; Add image of food to the given scene.
  (: food+scene : (Posn Image . -> . Image))
  (define (food+scene f scn)
    (place-image-on-grid (FOOD-IMAGE) (posn-x f) (posn-y f) scn))
  
  ;; place-image-on-grid : Image Number Number Image -> Image
  ;; Just like PLACE-IMAGE, but use grid coordinates.
  (: place-image-on-grid : (Image Real Real Image . -> . Image))
  (define (place-image-on-grid img x y scn)
    (place-image img
                 (* GRID-SIZE x)
                 (- (BOARD-HEIGHT-PIXELS) (* GRID-SIZE y))
                 scn))
  
  ;; Add an image of the snake to the scene.
  (: snake+scene : (Snake Image . -> . Image))
  (define (snake+scene snk scn)
    (segments+scene (snake-segs snk) scn))
  
  ;; segments+scene : Segs Image -> Image
  ;; Add an image of the snake segments to the scene.
  (: segments+scene : ((Listof Posn) Image . -> . Image))
  (define (segments+scene segs scn)
    (cond [(empty? segs) scn]
          [else (segments+scene (cdr segs) ;; tail recursion
                                (segment+scene (car segs) scn))]))
  
  ;; segment+scene : Posn Image -> Image
  ;; Add one snake segment to a scene.
  (: segment+scene : (Posn Image . -> . Image))
  (define (segment+scene seg scn)
    (place-image-on-grid (SEGMENT-IMAGE) (posn-x seg) (posn-y seg) scn))
  
  (provide
   world->scene
   food+scene
   place-image-on-grid
   snake+scene
   segments+scene
   segment+scene))
