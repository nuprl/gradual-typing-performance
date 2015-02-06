#lang typed/racket

(require "base-types.rkt")
(require/typed/check "data.rkt"
  [posn=? (-> Posn Posn Boolean)])
(require/typed/check "consts.rkt"
  [block-size Integer]
  [board-height Integer]
  [board-width Integer])
(require/typed/check "aux.rkt"
  [list-pick-random (-> (Listof Tetra) Tetra)]
  [neg-1  Negative-Fixnum]
  [tetras (Listof Tetra)])
(require/typed/check "world.rkt"
  [world-key-move (-> World String World)]
  [next-world (-> World World)]
  [ghost-blocks (-> World BSet)])
(require/typed
    2htdp/image
  [#:opaque Image image?]
  [overlay   (-> Image Image Image)]
  [circle    (-> Integer String String Image)]
  [rectangle (-> Integer Integer Color Color Image)]
  [place-image (-> Image Integer Integer Image Image)]
  [empty-scene (-> Integer Integer Image)])

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

