#lang racket/base

(require (only-in "zombie.rkt"
  w0
  world-on-mouse
  world-on-tick
))

;; =============================================================================

(define (replay w0 hist)
 (let loop ((w  w0)
            (h  hist))
  (cond
   [(null? h)
    (void)]
   [(not (list? (car h)))
    (error "input error")]
   [else
    (define m (caar h))
    (define as (cdar h))
    (case m
     ;; no rendering
     [(to-draw stop-when)
       (loop w (cdr h))]
     [(on-mouse)
      (define r (apply (world-on-mouse w) (if (and (list? as) (real? (car as)) (real? (cadr as)) (string? (caddr as)) (null? (cdddr as)))
                                              as (error "cast error"))))
      (loop r (cdr h))]
     [(on-tick)
      (define r ((world-on-tick w)))
      (loop r (cdr h))])])))

(define SMALL_TEST "../base/zombie-hist-small.rktd")
(define MICRO_TEST "../base/zombie-hist-micro.rktd")

(define (main filename)
  (define raw-hist (with-input-from-file filename read))
  (cond
   [(list? raw-hist)
    (define hist (reverse raw-hist))
    (for ([i  (in-range 100)])
      (replay w0 hist))]
   [else
    (error "bad input")]))

(time (main SMALL_TEST))
