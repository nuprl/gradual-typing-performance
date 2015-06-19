#lang typed/racket

(require "base-types.rkt")
(require benchmark-util)
(require/typed/check "aux.rkt"
  [list-pick-random (-> (Listof Tetra) Tetra)]
  [tetras (Listof Tetra)])
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

(define (world0)
  (world (list-pick-random tetras) empty))

(: replay : World (Listof Any) -> Void)
(define (replay w0 hist)
  (for/fold ([w : World w0])
            ([e hist])
    (match e
      [`(on-key ,(? string? ke)) (world-key-move w ke)]
      [`(on-tick) (next-world w)]
      [`(stop-when)
       (λ ([w : World]) (blocks-overflow? (world-blocks w)))
       w]))
  (void))


(define SMALL_TEST "../base/tetris-hist-small.txt")
(define LARGE_TEST "../base/tetris-hist-large.txt")

(: main (-> String Void))
(define (main filename)
  (define w0 (world0))
  (define raw (with-input-from-file filename read))
  (when (list? raw)
    (replay w0 (reverse raw))))

;; (time (main SMALL_TEST)) ; 0ms
(time (main LARGE_TEST)) ; 147ms
