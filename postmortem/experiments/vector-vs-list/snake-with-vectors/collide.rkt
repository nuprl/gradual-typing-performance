#lang typed/racket

(require benchmark-util
         "data-adaptor.rkt")
(require/typed/check "const.rkt"
                     [BOARD-WIDTH Integer]
                     [BOARD-HEIGHT Integer])
(require/typed/check "data.rkt"
                     [posn=? (Posn Posn . -> . Boolean)])

;; Is the snake colliding with any of the walls?
(: snake-wall-collide? : (Snake . -> . Boolean))
(define (snake-wall-collide? snk)
  (head-collide? (vector-ref (snake-segs snk) 0)))

(: head-collide? : (Posn . -> . Boolean))
(define (head-collide? p)
  (or (<= (posn-x p) 0)
      (>= (posn-x p) BOARD-WIDTH)
      (<= (posn-y p) 0)
      (>= (posn-y p) BOARD-HEIGHT)))

(: snake-self-collide? : (Snake . -> . Boolean))
(define (snake-self-collide? snk)
  (segs-self-collide? (vector-ref (snake-segs snk) 0)
                      (vector-drop (snake-segs snk) 1)))

(: segs-self-collide? : (Posn (Vectorof Posn) . -> . Boolean))
(define (segs-self-collide? h segs)
  (cond [(equal? '#() segs) #f]
        [else (or (posn=? (vector-ref segs 0) h)
                  (segs-self-collide? h (vector-drop segs 1)))]))
(provide
 snake-wall-collide?
 snake-self-collide?)
