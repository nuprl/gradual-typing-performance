#lang typed/racket

(define-type Color Symbol)
(define-type Posn (Pairof 'posn (List Real Real)))
(define-type Block (Pairof 'block (List Real Real Color)))
(define-type Tetra (Pairof 'tetra (List Posn (Listof Block))))
(define-type World (Pairof 'world (List Tetra (Listof Block))))

(require benchmark-util)
(require/typed/check "data.rkt"
 (posn (-> Real Real Posn))
 (posn-x (-> Posn Real))
 (posn-y (-> Posn Real))
 (block (-> Real Real Color Block))
 (block-x (-> Block Real))
 (block-y (-> Block Real))
 (block-color (-> Block Color))
 (tetra (-> Posn (Listof Block) Tetra))
 (tetra-center (-> Tetra Posn))
 (tetra-blocks (-> Tetra (Listof Block)))
 (world (-> Tetra (Listof Block) World))
 (world-tetra (-> World Tetra))
 (world-blocks (-> World (Listof Block)))
)
(require/typed 2htdp/image 
  [#:opaque Image image?])

(define-type BSet  (Listof Block))

(provide
 Posn  posn posn-x posn-y
 Block block block-x block-y block-color
 Tetra tetra tetra-center tetra-blocks
 World world world-tetra world-blocks
 Color
 BSet
 Image)
