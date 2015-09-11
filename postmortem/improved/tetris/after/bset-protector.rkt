#lang typed/racket/base

(require
  "base-types.rkt"
  benchmark-util
)

(require/typed/check "bset.rkt"
   [blocks-intersect (-> BSet BSet BSet)]
   [blocks-move (-> Real Real BSet BSet)]
   [blocks-rotate-cw (-> Posn BSet BSet)]
   [blocks-rotate-ccw (-> Posn BSet BSet)]
   [blocks-change-color (-> BSet Color BSet)])

(provide
  (rename-out
  [blocks-intersect+ blocks-intersect]
  [blocks-move+ blocks-move]
  [blocks-rotate-ccw+ blocks-rotate-ccw]
  [blocks-rotate-cw+ blocks-rotate-cw]
  [blocks-change-color+ blocks-change-color]
))

(: blocks-intersect+ (-> BSet+ BSet+ BSet+))
(define (blocks-intersect+ b1 b2)
  (wrap-bset+ (blocks-intersect (unwrap-bset+ b1) (unwrap-bset+ b2))))

(: blocks-move+ (-> Real Real BSet+ BSet+))
(define (blocks-move+ r1 r2 b1)
  (wrap-bset+ (blocks-move r1 r2 (unwrap-bset+ b1))))

(: blocks-rotate-cw+ (-> Posn BSet+ BSet+))
(define (blocks-rotate-cw+ p b)
  (wrap-bset+ (blocks-rotate-cw p (unwrap-bset+ b))))

(: blocks-rotate-ccw+ (-> Posn BSet+ BSet+))
(define (blocks-rotate-ccw+ p b)
  (wrap-bset+ (blocks-rotate-ccw p (unwrap-bset+ b))))

(: blocks-change-color+ (-> BSet+ Color BSet+))
(define (blocks-change-color+ b c)
  (wrap-bset+ (blocks-change-color (unwrap-bset+ b) c)))
