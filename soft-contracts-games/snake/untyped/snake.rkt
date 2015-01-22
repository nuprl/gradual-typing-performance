#lang racket

(require 2htdp/universe)
(require "data.rkt"
         "const.rkt"
         "scenes.rkt"
         "handlers.rkt"
         "motion.rkt"
         "collide.rkt")

(define history empty)
(define (! e)
  (set! history (cons e history)))

(define (play)
  (big-bang (WORLD)
            (on-tick (λ (w) (! `(on-tick)) (world->world w)) 1/5)
            (on-key (λ (w ke) (! `(on-key ,ke)) (handle-key w ke)))
            (to-draw (λ (w) (world->scene w)))
            (stop-when (λ (w) (! `(stop-when)) (game-over? w)))))

#;
(with-output-to-file "snake-hist-2.txt" 
  (λ () 
    (set! history empty)
    (play)
    (write history)))


(define (replay w0 hist)
  (reset!)
  (let loop ((w w0) (h hist))
    (if (empty? h)
        w
        (let ()
          (loop
           (match (car h)
             [`(on-key ,ke)
              (handle-key w ke)]
             [`(on-tick)
              (world->world w)]
             [`(stop-when)
              (game-over? w)
              w])
           (cdr h))))))

(define (start w)
  (big-bang w
            (on-tick world->world 1/5)
            (on-key handle-key)
            (to-draw world->scene)
            (stop-when game-over?)))

(define w0 (WORLD))
;(replay (WORLD) h)

(module+ main
  (start w0))
(provide replay w0 start)
