#lang racket/base
(require math/flonum racket/draw racket/class sugar/debug sugar/list racket/list sugar/cache racket/serialize racket/file)
(provide measure-text measure-ascent round-float update-text-cache-file load-text-cache-file)

(define precision 4.0)
(define base (flexpt 10.0 precision))

(define-syntax-rule (round-float x)
  (fl/ (flround (fl* base (fl x))) base))

(define dc (new record-dc%))

(define max-size 1024) ; use fixnum to trigger faster bitshift division
;; changing max-size invalidates font cache (because it's based on max size, duh)

(define/caching (make-font/caching font weight style)
  (make-font #:size max-size #:style style #:weight weight #:face font))

(define (get-cache-file-path)
  (build-path "font.cache"))

(define (update-text-cache-file)
  (void))

(define (load-text-cache-file) 
  (void))


(define/caching (measure-max-size text font [weight 'normal] [style 'normal])
  '(0 0 0 0))

(define-syntax-rule (width x) (first x))
(define-syntax-rule (height x) (second x))
(define-syntax-rule (descent x) (third x))
(define-syntax-rule (extra x) (fourth x)) 

(define-syntax-rule (measure-text-max-size text font weight style)
  (width (measure-max-size text font weight style)))

(define (measure-text text size font [weight 'normal] [style 'normal])
  ;  ((string? flonum? string?) (symbol? symbol?) . ->* . flonum?)
  ;; Native function only accepts integers, so get max-size and scale down to size needed.
  (define raw-measure (measure-text-max-size text font weight style))
  (round-float (/ (* (exact->inexact raw-measure) (exact->inexact size)) max-size)))


(define-syntax-rule (measure-ascent-max-size text font weight style)
  (let ([result-list (measure-max-size text font weight style)])
    (- (height result-list) (descent result-list))))


(define (measure-ascent text size font [weight 'normal] [style 'normal])
  ;  ((string? flonum? string?) (symbol? symbol?) . ->* . flonum?)
  ;; Native function only accepts integers, so get max-size and scale down to size needed.
  (define raw-baseline-distance (measure-ascent-max-size text font weight style))
  (round-float (/ (* (exact->inexact raw-baseline-distance) (exact->inexact size)) max-size)))
