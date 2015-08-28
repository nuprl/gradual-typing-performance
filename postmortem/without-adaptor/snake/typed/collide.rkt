#lang typed/racket

(require benchmark-util)
(require/typed/check "data.rkt"
  [#:struct posn ([x : Real]
                  [y : Real])]
  [#:struct snake ([dir : Dir]
                   [segs : (NEListof Posn)])]
  [#:struct world ([snake : Snake]
                   [food  : Posn])])

(define-type (NEListof A) (Pairof A (Listof A)))
(define-type Dir (U "up" "down" "left" "right"))
(define-type Snake snake)
(define-type World world)
(define-type Posn  posn)

(require/typed/check "const.rkt"
                     [BOARD-WIDTH Integer]
                     [BOARD-HEIGHT Integer])
(require/typed/check "data.rkt"
                     [posn=? (Posn Posn . -> . Boolean)])

;; Is the snake colliding with any of the walls?
(: snake-wall-collide? : (Snake . -> . Boolean))
(define (snake-wall-collide? snk)
  (head-collide? (car (snake-segs snk))))

(: head-collide? : (Posn . -> . Boolean))
(define (head-collide? p)
  (or (<= (posn-x p) 0)
      (>= (posn-x p) BOARD-WIDTH)
      (<= (posn-y p) 0)
      (>= (posn-y p) BOARD-HEIGHT)))

(: snake-self-collide? : (Snake . -> . Boolean))
(define (snake-self-collide? snk)
  (segs-self-collide? (car (snake-segs snk))
                      (cdr (snake-segs snk))))

(: segs-self-collide? : (Posn (Listof Posn) . -> . Boolean))
(define (segs-self-collide? h segs)
  (cond [(empty? segs) #f]
        [else (or (posn=? (car segs) h)
                  (segs-self-collide? h (cdr segs)))]))
(provide
 snake-wall-collide?
 snake-self-collide?)
