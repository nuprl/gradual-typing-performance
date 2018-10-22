#lang at-exp slideshow

(require
  pict-abbrevs
  ppict/2
  pict/shadow
  (only-in racket/list make-list first second third fourth fifth sixth)
  "util.rkt")

;; -----------------------------------------------------------------------------

(module+ main
  (set-page-numbers-visible! #true)
  (set-spotlight-style! #:size 60 #:color (color%-update-alpha HIGHLIGHT-COLOR 0.6))
  (parameterize ([current-main-font MONO-FONT]
                 [current-font-size NORMAL-FONT-SIZE]
                 [current-titlet string->title])
    (void)
    (sec:title)
    #;(sec:gt)
    #;(sec:claims)
    #;(sec:main-result)
    #;(pslide #:go CENTER-COORD (make-section-break "Three Strategies"))
    #;(sec:type-boundary) ;; LOW PRIORITY
    #;(pslide #:go CENTER-COORD (make-section-break "Survey Design"))
    (void)))

;; -----------------------------------------------------------------------------

(define (sec:title)
  (pslide)
  (void))

;; -----------------------------------------------------------------------------

(module+ test
  (require rackunit))

;; -----------------------------------------------------------------------------

(module+ raco-pict
  ;; ... could be a "pict-provider" and all exports combined?
  (provide raco-pict)
  (define p
    (let ()
      (blank)
  ))
  (define (add-bg p)
    (cc-superimpose (blank (+ 100 (pict-width p)) (+ 100 (pict-height p))) p))
  (define raco-pict (add-bg p))
  (void))
