#lang typed/racket

(require typed/pict)

(require/typed unstable/gui/pict
               [rectangle/border (->* (Real Real)
                                      (#:border-color String
                                       #:color String
                                       #:border-width Integer)
                                      pict)]
               [shadow (->* (pict Real Real)
                            (#:shadow-color String)
                            pict)])

(provide (all-defined-out))

(: lbox (->* (pict)
             (#:padding Integer #:color String
              #:border-color String #:shadow-color String)
             pict))
;; a light colored box surrounding the pict
(define (lbox pict
              #:padding [padding 10]
              #:color [color "honeydew"]
              #:border-color [border-color "dark gray"]
              #:shadow-color [shadow-color "black"])
  (define-values (w h)
    (values (pict-width pict) (pict-height pict)))
  (cc-superimpose
    (shadow (filled-rectangle (+ w (* padding 2))
                              (+ h (* padding 2)))
            25
            5
            #:shadow-color shadow-color)
    (rectangle/border #:color color
                      #:border-color border-color
                      (+ w (* padding 2))
                      (+ h (* padding 2)))
    pict))

