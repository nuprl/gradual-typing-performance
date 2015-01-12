#lang racket

(require "image.rkt"
         "data.rkt"
         "consts.rkt"
         "world.rkt"
         "list-fun.rkt"
         "aux.rkt")

(provide/contract
 [world->image (WORLD/C . -> . image/c)]
 [blocks->image (BSET/C . -> . image/c)]
 [block->image (BLOCK/C . -> . image/c)]
 [place-block (BLOCK/C image/c . -> . image/c)]
 [world0 any/c])

;; Visualize whirled peas
;; World -> Scene
(define (world->image w)
  (place-image (blocks->image (append (tetra-blocks (world-tetra w))
                                      (append (ghost-blocks w)
                                              (world-blocks w))))
               (/ (* board-width block-size) 2)
               (/ (* board-height block-size) 2)
               (empty-scene (* board-width block-size)
                            (* board-height block-size))))

;; BSet -> Scene
(define (blocks->image bs)
  (foldr-i (λ (b img)
             (cond [(<= 0 (block-y b)) (place-block b img)]
                   [else img]))
           (empty-scene (add1 (* board-width block-size)) 
                        (add1 (* board-height block-size)))
           bs))

;; Visualizes a block.
;; Block -> Image
(define (block->image b)
  (overlay 
   (rectangle (add1 block-size) (add1 block-size) 'solid (block-color b))
   (rectangle (add1 block-size) (add1 block-size) 'outline 'black)))

;; Block Scene -> Scene
(define (place-block b scene)
  (place-image (block->image b)
               (+ (* (block-x b) block-size) (/ block-size 2))
               (+ (* (block-y b) block-size) (/ block-size 2))
               scene))

(define (world0)
  (world (list-pick-random tetras) empty))

