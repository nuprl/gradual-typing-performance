#lang typed/racket

(require "image.rkt"
         "data.rkt"
         "consts.rkt"
         "world.rkt"
         "aux.rkt")

(provide
 world->image
 blocks->image
 block->image
 place-block
 world0)

;; Visualize whirled peas
(: world->image (-> World Image))
(define (world->image w)
  (place-image (blocks->image (append (tetra-blocks (world-tetra w))
                                      (append (ghost-blocks w)
                                              (world-blocks w))))
               (floor (/ (* board-width block-size) 2))
               (floor (/ (* board-height block-size) 2))
               (empty-scene (* board-width block-size)
                            (* board-height block-size))))

(: blocks->image (-> BSet Image))
(define (blocks->image bs)
  (foldr (λ: ([b   : Block]
              [img : Image])
             (cond [(<= 0 (block-y b)) (place-block b img)]
                   [else img]))
           (empty-scene (add1 (* board-width block-size)) 
                        (add1 (* board-height block-size)))
           bs))

;; Visualizes a block.
(: block->image (-> Block Image))
(define (block->image b)
  (overlay 
   (rectangle (add1 block-size) (add1 block-size) 'solid (block-color b))
   (rectangle (add1 block-size) (add1 block-size) 'outline 'black)))

(: place-block (-> Block Image Image))
(define (place-block b scene)
  (place-image (block->image b)
               (exact-floor (+ (* (block-x b) block-size) (/ block-size 2)))
               (exact-floor (+ (* (block-y b) block-size) (/ block-size 2)))
               scene))

(define (world0)
  (world (list-pick-random tetras) empty))

