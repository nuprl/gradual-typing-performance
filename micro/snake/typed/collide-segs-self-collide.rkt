#lang typed/racket/base

(provide segs-self-collide?)

;; -----------------------------------------------------------------------------

(require benchmark-util
         "data-posn-adapted.rkt")
(require/typed/check "data-posn-eq.rkt"
  [posn=? (-> Posn Posn Boolean)])

;; =============================================================================

(: segs-self-collide? : (-> Posn (Listof Posn) Boolean))
(define (segs-self-collide? h segs)
  (cond [(eq? '() segs) #f]
        [else (or (posn=? (car segs) h)
                  (segs-self-collide? h (cdr segs)))]))
