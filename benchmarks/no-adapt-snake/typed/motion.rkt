#lang typed/racket

(require benchmark-util
         "data-adaptor.rkt")

(require/typed/check "const.rkt"
                     [BOARD-WIDTH Integer]
                     [BOARD-HEIGHT Integer])
(require/typed/check "data.rkt"
                     [posn=? (Posn Posn . -> . Boolean)])
(require/typed/check "motion-help.rkt"
                     [snake-slither (Snake . -> . Snake)]
                     [snake-grow    (Snake . -> . Snake)])

(provide reset!)
(define r (make-pseudo-random-generator)) 
(define (reset!)
  (parameterize ((current-pseudo-random-generator r))
    (random-seed 1324)))

(: world->world : (World . -> . World))
(define (world->world w)
  (cond [(eating? w) (snake-eat w)]
        [else
         (world (snake-slither (world-snake w))
                (world-food w))]))

;; Is the snake eating the food in the world.
(: eating? : (World . -> . Boolean))
(define (eating? w)
  (posn=? (world-food w)
          (car (snake-segs (world-snake w)))))

;; Change the direction of the snake.
(: snake-change-direction : (Snake Dir . -> . Snake))
(define (snake-change-direction snk dir)
  (snake dir
         (snake-segs snk)))

;; Change direction of the world.
(: world-change-dir : (World Dir . -> . World))
(define (world-change-dir w dir)
  (world (snake-change-direction (world-snake w) dir)
         (world-food w)))

;; Eat the food and generate a new one.
(: snake-eat : (World . -> . World))
(define (snake-eat w)
  (define i (add1 (random (sub1 BOARD-WIDTH) r)))
  (define j (add1 (random (sub1 BOARD-HEIGHT) r)))
  (world (snake-grow (world-snake w))
         (posn i j)))
(provide
 world-change-dir
 world->world)
