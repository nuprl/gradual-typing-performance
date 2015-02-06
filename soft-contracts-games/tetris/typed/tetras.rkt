#lang typed/racket

(require "base-types.rkt")
(require benchmark-util)
(require/typed/check "data.rkt"
  [posn=? (-> Posn Posn Boolean)])
(require/typed/check "bset.rkt"
   [blocks-contains? (-> BSet Block Boolean)]
   [blocks=? (-> BSet BSet Boolean)]
   [blocks-subset? (-> BSet BSet Boolean)]
   [blocks-intersect (-> BSet BSet BSet)]
   [blocks-count (-> BSet Natural)]
   [blocks-overflow? (-> BSet Boolean)]
   [blocks-move (-> Real Real BSet BSet)]
   [blocks-rotate-cw (-> Posn BSet BSet)]
   [blocks-rotate-ccw (-> Posn BSet BSet)]
   [blocks-change-color (-> BSet Color BSet)]
   [blocks-row (-> BSet Real BSet)]
   [full-row? (-> BSet Natural Boolean)]
   [blocks-union (-> BSet BSet BSet)]
   [blocks-max-x (-> BSet Real)]
   [blocks-min-x (-> BSet Real)]
   [blocks-max-y (-> BSet Real)]
  )
(require/typed/check "consts.rkt"
  [block-size Integer]
  [board-height Integer]
  [board-width Integer])
(require/typed/check "block.rkt"
  [block-rotate-ccw (-> Posn Block Block)]
  [block-rotate-cw (-> Posn Block Block)]
  [block=? (-> Block Block Boolean)]
  [block-move (-> Real Real Block Block)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tetras

;; Move the Tetra by the given X & Y displacement.
(: tetra-move (-> Real Real Tetra Tetra))
(define (tetra-move dx dy t)
  (tetra (posn (+ dx (posn-x (tetra-center t)))
               (+ dy (posn-y (tetra-center t))))
         (blocks-move dx dy (tetra-blocks t))))

;; Rotate the tetra 90 degrees counterclockwise around its center.
(: tetra-rotate-ccw (-> Tetra Tetra))
(define (tetra-rotate-ccw t)
  (tetra (tetra-center t)
         (blocks-rotate-ccw (tetra-center t)
                            (tetra-blocks t))))

;; Rotate the tetra 90 degrees clockwise around its center.
(: tetra-rotate-cw (-> Tetra Tetra))
(define (tetra-rotate-cw t)
  (tetra (tetra-center t)
         (blocks-rotate-cw (tetra-center t)
                           (tetra-blocks t))))

;; Is the tetra on any of the blocks?
(: tetra-overlaps-blocks? (-> Tetra BSet Boolean))
(define (tetra-overlaps-blocks? t bs)
  (not (empty? (blocks-intersect (tetra-blocks t) bs))))

;; Change the color of the given tetra.
(: tetra-change-color (-> Tetra Color Tetra))
(define (tetra-change-color t c)
  (tetra (tetra-center t)
         (blocks-change-color (tetra-blocks t) c)))

(: build-tetra-blocks (-> Color Real Real Real Real Real Real Real Real Real Real Tetra))
(define (build-tetra-blocks color xc yc x1 y1 x2 y2 x3 y3 x4 y4)
  (tetra-move 3 0 
              (tetra (posn xc yc)
                     (list (block x1 y1 color)
                           (block x2 y2 color)
                           (block x3 y3 color)
                           (block x4 y4 color)))))

(provide
 tetra-move
 tetra-rotate-ccw
 tetra-rotate-cw
 tetra-overlaps-blocks?
 build-tetra-blocks
 tetra-change-color)

