#lang racket

(provide
  ;; syntax
  
  ;; (def/dots id ...) introduces n named circles of radius R 
  def/dots

  ;; (build-scene id e0 e ...)
  ;; monadically builds a scene namde id from e0 and then e ...
  build-scene)

;; -----------------------------------------------------------------------------

(require slideshow)

(define R 30)
(define-syntax-rule
  (def/dots name ...)
  (begin
    (define texts (map t (list (format "~a" 'name) ...)))
    (define R (max (apply max (map pict-height texts)) (apply max (map pict-width texts))))
    (define name (cc-superimpose (t (format "~a" 'name)) (ellipse R R)))
    ...))

(define-syntax-rule
  (build-scene s s0 e ...)
  (let* ([s s0]
         [s e]
         ...)
    s))
