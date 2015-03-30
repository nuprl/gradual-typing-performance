#lang racket

(require "data.rkt"
         "const.rkt"
         "handlers.rkt"
         "motion.rkt")

(define (replay w0 hist)
  (reset!)
  (let loop ((w w0) (h hist))
    (if (empty? h)
        w
        (let ()
          (loop
           (match (car h)
             [`(on-key ,(? string? ke))
              (handle-key w ke)]
             [`(on-tick)
              (world->world w)]
             [`(stop-when)
              (game-over? w)
              w])
           (cdr h))))))

(define w0 (WORLD))
(define raw-hist (time (with-input-from-file "../snake-hist-4.txt" read)))

(cond [(list? raw-hist)
       (time
        (define hist (reverse raw-hist))
        (for ([i (in-range 100)])
          (replay w0 hist)))]
      [else
       (error "THIS BENCHMARK IS BROKEN")])
