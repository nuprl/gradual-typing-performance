#lang racket

(require "data.rkt"
         "const.rkt"
         "handlers.rkt"
         "motion.rkt")

(define (replay w0 hist)
  (reset!)
  (let loop ((w w0) (h hist))
    (if (equal? '#() h)
        w
        (let ()
          (loop
           (match (vector-ref h 0)
             [`(on-key ,(? string? ke))
              (handle-key w ke)]
             [`(on-tick)
              (world->world w)]
             [`(stop-when)
              (game-over? w)
              w])
           (vector-drop h 1))))))

(define SMALL_TEST "../base/snake-hist-small.rktd")
(define LARGE_TEST "./snake-hist-large.rktd")

(define (vector-reverse vec)
  (define L (vector-length vec))
  (build-vector L (lambda (i) (vector-ref vec (- L 1 i)))))

(define (main filename)
  (define w0 (WORLD))
  (define raw-hist (with-input-from-file filename read))
  (cond [(vector? raw-hist)
         (define hist (vector-reverse raw-hist))
         (for ([i (in-range 100)])
           (replay w0 hist))]
        [else
         (error "bad input")]))

;(time (main LARGE_TEST)) ; 100ms
(require contract-profile)
(contract-profile-thunk (lambda () (main LARGE_TEST)))
