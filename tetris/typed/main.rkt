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
   [blocks-max-y (-> BSet Real)])
(require/typed/check "world.rkt"
  [world-key-move (-> World String World)]
  [next-world (-> World World)]
  [ghost-blocks (-> World BSet)])
(require/typed/check "visual.rkt"
  [world->image (-> World Image)]
  [world0 (-> World)])


(: replay : World (Listof Any) -> World)
(define (replay w0 hist)
  (for/fold ([w : World w0])
            ([e hist])
    (match e
      [`(on-key ,(? string? ke)) (world-key-move w ke)]
      [`(on-tick) (next-world w)]
      [`(to-draw) (world->image w) w]
      [`(stop-when) 
       (λ ([w : World]) (blocks-overflow? (world-blocks w)))
       w])))

(define w0 (world0))
(define raw (with-input-from-file "tetris-hist-3.txt" read))
(when (list? raw)
  (define hist (reverse raw))
  (replay w0 hist))
