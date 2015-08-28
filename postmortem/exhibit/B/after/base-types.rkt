#lang typed/racket

(define-type Color Symbol)
(require benchmark-util)
(require/typed/check "data.rkt"
  [#:struct posn ([x : Real]
                  [y : Real])]
  [#:struct block ([x : Real]
                   [y : Real]
                   [color : Color])]
  [#:struct tetra ([center : posn]
                   [blocks : BSet])]
  [#:struct world ([tetra : tetra]
                   [blocks : BSet+])])

(require/typed 2htdp/image 
  [#:opaque Image image?])

(define-type Posn posn)
(define-type Block block)
(define-type Tetra tetra)
(define-type World world)
(define-type BSet  (Listof Block))

;; NEW
(struct bset+ ([val : BSet]))
(define-type BSet+ (U BSet bset+))
(: unwrap-bset+ (-> BSet+ BSet))
(define (unwrap-bset+ b)
  (if (bset+? b) (bset+-val b) b))
(: wrap-bset+ (-> BSet+ BSet+))
(define (wrap-bset+ b)
  (if (bset+? b) b (bset+ b)))
(: make-tetra (-> Posn BSet+ Tetra))
(define (make-tetra a b)
  (tetra a (unwrap-bset+ b)))
(provide
 BSet+
 wrap-bset+
 unwrap-bset+
 (rename-out [make-tetra tetra])
 tetra? tetra-center tetra-blocks
)
;; END NEW

(provide
 (struct-out posn)
 (struct-out block)
 ;; (struct-out tetra)
 (struct-out world)
 Posn
 Block
 Tetra
 World
 Color
 BSet
 Color
 BSet
 Image)
