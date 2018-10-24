#lang racket/base

(provide
  rectangle/2t
  rounded-rectangle/2t)

(require
  racket/class
  racket/draw
  pict)

;; =============================================================================

(define WHITE "white")
(define BLACK "black")

(define (rectangle/2t width height
                      #:border-width [border-width 1]
                      #:border-color [border-color BLACK]
                      #:color-1 [color-1 WHITE]
                      #:color-2 [color-2 BLACK])
  (X-rectangle/2t 'draw-rectangle width height #:border-width border-width #:border-color border-color #:color-1 color-1 #:color-2 color-2))

(define (rounded-rectangle/2t width height
                              #:radius [radius -0.25]
                              #:border-width [border-width 1]
                              #:border-color [border-color BLACK]
                              #:color-1 [color-1 WHITE]
                              #:color-2 [color-2 BLACK])
  (X-rectangle/2t 'draw-rounded-rectangle width height #:border-width border-width #:border-color border-color #:color-1 color-1 #:color-2 color-2))

(define (X-rectangle/2t X-draw width height
                        #:radius [radius -0.25]
                        #:border-width [border-width 1]
                        #:border-color [border-color BLACK]
                        #:color-1 [color-1 WHITE]
                        #:color-2 [color-2 BLACK])
  ;; thank you asumu https://www.asumu.xyz/blog/2018/03/31/making-the-most-of-lang-slideshow/
  (dc (λ (dc dx dy)
        (define old-brush
          (send dc get-brush))
        (define old-pen
          (send dc get-pen))
        (define gradient
          (make-object
           linear-gradient%
           dx dy
           dx (+ dy height)
           `((0 ,(make-object color% color-1))
             (1 ,(make-object color% color-2)))))
        (send dc set-brush
          (new brush% [gradient gradient]))
        (send dc set-pen
          (new pen% [width border-width]
                    [color border-color]))
        (case X-draw
          ((draw-rectangle)
           (send dc draw-rectangle dx dy width height))
          ((draw-rounded-rectangle)
           (send dc draw-rounded-rectangle dx dy width height radius))
          (else
            (raise-argument-error 'X-rectangle "draw method" X-draw)))
        (send dc set-brush old-brush)
        (send dc set-pen old-pen))
      width height))
