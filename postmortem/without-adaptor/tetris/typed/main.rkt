#lang typed/racket

(require benchmark-util)
(define-type Color Symbol)
(require/typed/check "data.rkt"
  [#:struct posn ([x : Real]
                  [y : Real])]
  [#:struct block ([x : Real]
                   [y : Real]
                   [color : Color])]
  [#:struct tetra ([center : posn]
                   [blocks : (Listof Block)])]
  [#:struct world ([tetra : tetra]
                   [blocks : (Listof Block)])])

(require/typed 2htdp/image 
  [#:opaque Image image?])

(define-type Posn posn)
(define-type Block block)
(define-type Tetra tetra)
(define-type World world)
(define-type BSet  (Listof Block))
(require/typed/check "aux.rkt"
  [list-pick-random (-> (Listof Tetra) Tetra)]
  [tetras (Listof Tetra)])
(require/typed/check "bset.rkt"
   [blocks-overflow? (-> BSet Boolean)])
(require/typed/check "world.rkt"
  [world-key-move (-> World String World)]
  [next-world (-> World World)])

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


(define SMALL_TEST "../base/tetris-hist-small.rktd")
(define LARGE_TEST "../base/tetris-hist-large.rktd")

(: main (-> String Void))
(define (main filename)
  (define w0 (world0))
  (define raw (with-input-from-file filename read))
  (if (list? raw)
    (replay w0 (reverse raw))
    (error "bad input")))

;; (time (main SMALL_TEST)) ; 0ms
(require/typed contract-profile [contract-profile-thunk (-> (-> Void) Void)])
(contract-profile-thunk (lambda () (main LARGE_TEST))) ; 417ms
