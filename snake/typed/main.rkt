#lang typed/racket

(require benchmark-util
         "data-adaptor.rkt")
(require/typed/check "const.rkt"
                     [WORLD (-> World)]
                     [GRID-SIZE Integer]
                     [BOARD-HEIGHT-PIXELS (-> Positive-Index)]
                     [BOARD-WIDTH Integer]
                     [BOARD-HEIGHT Integer])
(require/typed/check "motion.rkt"
                     [world-change-dir (World Dir . -> . World)]
                     [world->world     (World . -> . World)])
(require/typed/check "handlers.rkt"
                     [handle-key (World String . -> . World)]
                     [game-over? (World . -> . Boolean)])

(: replay : World (Listof Any) -> Void)
(define (replay w0 hist)
  (reset!)
  (let loop ((w : World w0)
             (h : (Listof Any) hist))
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
           (cdr h)))))
  (void))

(define w0 (WORLD))
(define raw-hist (time (with-input-from-file "../snake-hist-4.txt" read)))

(cond [(list? raw-hist)
       (time
        (define hist (reverse raw-hist))
        (for ([i (in-range 100)])
          (replay w0 hist)))]
      [else
       (error "THIS BENCHMARK IS BROKEN")])
