#lang at-exp slideshow

(require
  pict-abbrevs
  ppict/2
  pict/shadow
  (only-in racket/list make-list first second third fourth fifth sixth)
  "author.rkt"
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
    (sec:contribution)
    #;(pslide #:go CENTER-COORD (make-section-break "Three Strategies"))
    #;(sec:type-boundary) ;; LOW PRIORITY
    #;(pslide #:go CENTER-COORD (make-section-break "Survey Design"))
    (void)))

;; -----------------------------------------------------------------------------

(define (sec:title)
  (pslide
    #:go (coord SLIDE-LEFT 20/100 'lt)
    (parameterize ((current-main-font (list* 'bold 'italic TITLE-FONT))
                   (current-font-size TITLE-FONT-SIZE))
      (lines-append
        @t{How to Evaluate the Performance of}
        @t{Gradual Type Systems}))
    #:go (coord SLIDE-RIGHT 45/100 'rt)
    (tag-pict
      (parameterize ((current-main-font ALL-CAPS-FONT)
                     (current-font-size SMALL-FONT-SIZE))
        (columns-append
          (lines-append
            @t{Ben Greenman *}
            @t{Max S. New}
            @t{Robert Bruce Findler}
            @t{Matthias Felleisen})
          (lines-append
            @t{Asumu Takikawa}
            @t{Daniel Feltey}
            @t{Jan Vitek}))) 'the-authors)
    #:go (coord 1/2 SLIDE-BOTTOM 'cc)
    (apply hb-append 40 all-logo*))
  (void))

(define (sec:contribution)
  (pslide
    #:go HEADING-COORD
    (subtitle-text "Contribution:")
    #:go (coord 1/10 30/100 'lt)
    (lines-append
      (hb-append @t{We propose a } @bt{systematic method} @t{ to measure})
      (hb-append @t{the } @bt{performance implications} @t{ of a gradual})
      (hb-append @t{typing system})))
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
