#lang racket/base

(require
  (only-in "array-pointwise.rkt" array-map)
  (only-in "array-broadcast.rkt" array-broadcasting))

(provide mix)


;; A Weighted-Signal is a (List (Array Float) Real)

;; Weighted sum of signals, receives a list of lists (signal weight).
;; Shorter signals are repeated to match the length of the longest.
;; Normalizes output to be within [-1,1].

;; mix : Weighted-Signal * -> (Array Float)
(define (mix . ss)
  (define signals
    (for/list ([s ss])
      (car s)))
  (define weights
    (for/list ([x ss])
      (real->double-flonum (cadr x))))
  (define downscale-ratio (/ 1.0 (apply + weights)))
  ;; scale-signal : Float -> (Float -> Float)
  (define ((scale-signal w) x) (* x w downscale-ratio))
  (parameterize ([array-broadcasting 'permissive]) ; repeat short signals
    (for/fold ([res (array-map (scale-signal (car weights))
                               (car signals))])
        ([s (in-list (cdr signals))]
         [w (in-list (cdr weights))])
      (define scale (scale-signal w))
      (array-map (lambda (acc  ; : Float
                          new) ; : Float
                   (+ acc (scale new)))
                 res s))))
