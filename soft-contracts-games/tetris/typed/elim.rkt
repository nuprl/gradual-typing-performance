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

;; Eliminate all full rows and shift down appropriately.
(: eliminate-full-rows (-> BSet BSet))
(define (eliminate-full-rows bs)
  (elim-row bs board-height 0))

(: elim-row (-> BSet Integer Integer BSet))
(define (elim-row bs i offset)
  (cond [(< i 0) empty]
        [(full-row? bs i)   (elim-row bs (sub1 i) (add1 offset))]
        [else (blocks-union (elim-row bs (sub1 i) offset)
                            (blocks-move 0 offset (blocks-row
                                                   bs i)))]))
(provide
 eliminate-full-rows)
