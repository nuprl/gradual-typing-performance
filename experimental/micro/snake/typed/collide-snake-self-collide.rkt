#lang typed/racket/base

(provide snake-self-collide?)

;; -----------------------------------------------------------------------------

(require benchmark-util
         "data-snake-adapted.rkt"
         "data-posn-adapted.rkt")
(require/typed/check "collide-segs-self-collide.rkt"
  [segs-self-collide? (-> Posn (Listof Posn) Boolean)])

;; =============================================================================

(: snake-self-collide? : (-> Snake Boolean))
(define (snake-self-collide? snk)
  (segs-self-collide? (car (snake-segs snk))
                      (cdr (snake-segs snk))))
