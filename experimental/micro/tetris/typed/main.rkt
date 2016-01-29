#lang typed/racket/base

(require benchmark-util
         "data-tetra-adapted.rkt"
         "data-world-adapted.rkt")
(require/typed/check "aux-tetras.rkt"
  [tetras (Listof Tetra)])
(require/typed/check "aux-list-pick-random.rkt"
  [list-pick-random (-> (Listof Tetra) Tetra)])
(require/typed/check "main-replay.rkt"
  [replay (-> World (Listof Any) Void)])

;; =============================================================================

(define (main)
 (define w0 (world (list-pick-random tetras) '()))
 (define raw (with-input-from-file "../base/tetris-hist-3.txt" read))
 (unless (list? raw) (error "input from file is not a list"))
 (replay w0 (reverse raw)))

(time (main))
