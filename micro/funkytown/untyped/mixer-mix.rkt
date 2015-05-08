#lang racket/base

(provide mix)

;; -----------------------------------------------------------------------------

(require "data-array.rkt"
         (for-syntax racket/base)
(only-in "array-broadcast-array-broadcasting.rkt"
  array-broadcasting)
(only-in "mixer-array-map.rkt"
  array-map))

;; =============================================================================

(define (mix . ss)
  (define signals
    (for/list ([s ss])
      (car s)))
  (define weights
    (for/list ([x ss])
      (real->double-flonum (cadr x))))
  (define downscale-ratio (/ 1.0 (apply + weights)))
  (define ((scale-signal w) x) (* x w downscale-ratio))
  (parameterize ([array-broadcasting 'permissive]) ; repeat short signals
    (for/fold ([res (array-map (scale-signal (car weights))
                               (car signals))])
        ([s (in-list (cdr signals))]
         [w (in-list (cdr weights))])
      (define scale (scale-signal w))
      (array-map (lambda (acc
                          new)
                   (+ acc (scale new)))
                 res s))))
