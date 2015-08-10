#lang typed/racket/base

(provide mix)

;; -----------------------------------------------------------------------------

(require "data-array-adapted.rkt"

         "type-aliases.rkt"
         (for-syntax racket/base)
         benchmark-util)

(require/typed/check "array-broadcast-array-broadcasting.rkt"
  [array-broadcasting (Parameterof (U #f #t 'permissive))])
(require/typed/check "mixer-array-map.rkt"
  [array-map (case-> (-> (-> Float Float Float) Array Array Array)
                     (-> (-> Float Float) Array Array))])

;; =============================================================================

;; Just a shorthand
(define-type Weighted-Signal (List Array Real))

(: mix (-> Weighted-Signal * Array))
(define (mix . ss)
  (: signals (Listof Array))
  (define signals
    (for/list : (Listof Array) ([s : Weighted-Signal ss])
      (car s)))
  (: weights (Listof Float))
  (define weights
    (for/list : (Listof Float) ([x : Weighted-Signal ss])
      (real->double-flonum (cadr x))))
  (: downscale-ratio Float)
  (define downscale-ratio (/ 1.0 (apply + weights)))
  (: scale-signal (Float -> (Float -> Float)))
  (define ((scale-signal w) x) (* x w downscale-ratio))
  (parameterize ([array-broadcasting 'permissive]) ; repeat short signals
    (for/fold ([res : Array (array-map (scale-signal (car weights))
                               (car signals))])
        ([s (in-list (cdr signals))]
         [w (in-list (cdr weights))])
      (define scale (scale-signal w))
      (array-map (lambda ([acc : Float]
                          [new : Float])
                   (+ acc (scale new)))
                 res s))))
