#lang at-exp slideshow

(require
  data/bit-vector
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
    #;(sec:title)
    #;(sec:contribution)
    (sec:gt-cost)
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
    #:go (coord 1/10 20/100 'lt)
    (lines-append
      (hb-append @t{We propose a systematic } @bt{method} @t{ to measure})
      (hb-append @t{the } @bt{performance implications} @t{ of a gradual})
      (hb-append @t{typing system})))
  (void))

(define (sec:gt-cost)
  ;; program === components, gt => mixed-typed, mixed-typed => cost
  (pslide
    #:go CENTER-COORD
    (make-program-pict/mixed))
  (void))

;; -----------------------------------------------------------------------------

(define (make-component-pict/sta #:body [body (blank)]
                                 #:width [pre-width #f]
                                 #:height [pre-height #f])
  (make-component-pict/tu #:body body
                          #:width pre-width
                          #:height pre-height
                          #:color STAT-COLOR))

(define (make-component-pict/dyn #:body [body (blank)]
                                 #:width [pre-width #f]
                                 #:height [pre-height #f])
  (make-component-pict/tu #:body body
                          #:width pre-width
                          #:height pre-height
                          #:color DYN-COLOR))

(define (make-component-pict/blank #:body [body-blank #f]
                                   #:width [pre-width #f]
                                   #:height [pre-height #f])
  (make-component-pict/tu #:body (or body-blank (blank))
                          #:width pre-width
                          #:height pre-height
                          #:color BLANK-COLOR))

(define (make-component-pict/tu #:body body 
                                #:width w
                                #:height h
                                #:color c)
  (make-component-pict #:body body
                       #:width w
                       #:height h
                       #:color c
                       #:border-width 2
                       #:border-color "black"))

(define (make-component-pict #:body [body (blank)]
                             #:width [pre-width #f]
                             #:height [pre-height #f]
                             #:color [color WHITE]
                             #:border-width [border-width 4]
                             #:border-color [border-color "black"])
  (define m/2 (/ margin 2))
  (define w (or pre-width (+ m/2 (pict-width body))))
  (define h (or pre-height (+ m/2 (pict-height body))))
  (define bg
    (if border-width
      (filled-rounded-rectangle
        w h SMALL-ROUND
        #:draw-border? #true
        #:color color
        #:border-color border-color
        #:border-width border-width)
      (filled-rounded-rectangle w h SMALL-ROUND #:color color #:draw-border? #false)))
  (cc-superimpose bg body))

(define (make-program-pict/sta)
  (add-program-edges (list->component* '(#t #t #t #t))))

(define (make-program-pict/dyn)
  (add-program-edges (list->component* '(#f #f #f #f))))

(define (make-program-pict/mixed #:show-boundary? [show-boundary? #false])
  (add-program-edges (list->component* '(#f #t #t #f))
                     #:show-boundary? show-boundary?))

(define (make-program-pict/blank)
  (add-program-edges (list->component* '(0 0 0 0)) #:show-boundary? #false))

(define (list->component* bool*)
  (define-values [blank-body-pict sta-body-pict dyn-body-pict]
    (apply values (pict-bbox-sup (blank) (small-tau-icon) (small-lambda-icon))))
  (for/list ((b (in-list bool*)))
    (cond
      [(eq? b #true)
       (tag-pict (make-component-pict/sta #:body sta-body-pict) STA-TAG)]
      [(eq? b #false)
       (tag-pict (make-component-pict/dyn #:body dyn-body-pict) DYN-TAG)]
      [(equal? b 0)
       (make-component-pict/blank #:body blank-body-pict)]
      [else
        (raise-argument-error 'list->component* "(or/c 0 #true #false)" b)])))

(define (add-program-edges c* #:show-boundary? [show-boundary? #false])
  (match c*
    [(list c0 c1 c2 c3)
     (define the-pict (apply hb-append COMPONENT-MARGIN c*))
     (for/fold ((acc the-pict))
               ((edge-spec (in-list `(((,c0 ,rt-find) (,c2 ,lt-find)  50)
                                      ((,c0 ,rb-find) (,c3 ,lb-find) -50)
                                      ((,c1 ,rc-find) (,c2 ,lc-find)   0)
                                      ((,c2 ,rc-find) (,c3 ,lc-find)   0)))))
       (define dom-spec (car edge-spec))
       (define cod-spec (second edge-spec))
       (define dom-pict (car dom-spec))
       (define cod-pict (car cod-spec))
       (define boundary-color
         (if (and show-boundary? (not (eq? (pict-tag dom-pict) (pict-tag cod-pict))))
           TYPE-BOUNDARY-COLOR
           BLACK))
       (pin-arrows-line COMPONENT-ARROW-SIZE
                        acc
                        dom-pict (second dom-spec)
                        cod-pict (second cod-spec)
                        #:start-angle (- (caddr edge-spec))
                        #:end-angle (caddr edge-spec)
                        #:line-width COMPONENT-ARROW-WIDTH
                        #:color boundary-color))]
    [_
      (raise-argument-error 'add-program-edges "(list pict? pict? pict? pict?)" c*)]))

(define (make-boundary-pict #:l [pre-l-body-pict #f]
                            #:c [pre-c-body-pict #f]
                            #:r [pre-r-body-pict #f])
  (define-values [l-body-pict r-body-pict _spacer]
    (apply values (pict-bbox-sup
                    (or pre-l-body-pict (blank))
                    (or pre-r-body-pict (blank))
                    (blank 90 90))))
  (define c-body-pict (or pre-c-body-pict (blank)))
  (define l-pict (make-component-pict/dyn #:body l-body-pict))
  (define r-pict (make-component-pict/sta #:body r-body-pict))
  (define arrow-margin 2)
  (add-labeled-arrow #:direction 'right
                     #:label (vc-append arrow-margin c-body-pict (blank))
                     #:x-sep (/ client-w 4)
                     (hb-append arrow-margin l-pict (blank))
                     (hb-append arrow-margin (blank) r-pict)))

(define (add-labeled-arrow l-pict
                           r-pict
                           #:x-sep [pre-x-sep #f]
                           #:direction [dir 'right]
                           #:label [pre-lbl #f])
  (define x-sep (or pre-x-sep (/ client-w 4)))
  (define bg (hc-append x-sep l-pict r-pict))
  (define lbl-pict (or pre-lbl (blank)))
  (define-values [dom-pict dom-find cod-pict cod-find]
    (case dir
      ((left)
       (values r-pict lc-find l-pict rc-find))
      ((right)
       (values l-pict rc-find r-pict lc-find))
      (else
        (raise-argument-error 'add-labeled-arrow "direction?" dir))))
  (pin-arrow-line TYPE-BOUNDARY-ARROW-SIZE
                  bg
                  dom-pict dom-find
                  cod-pict cod-find
                  #:label lbl-pict
                  #:color TYPE-BOUNDARY-COLOR
                  #:line-width TYPE-BOUNDARY-ARROW-WIDTH))

(define (make-lattice total-bits make-node)
  (define posn* (build-list total-bits values))
  (define LATTICE-X-MARGIN 10)
  (define LATTICE-Y-MARGIN 20)
  (define level-picts
    (for/list ([on-bits (in-range total-bits -1 -1)])
      (apply hc-append LATTICE-X-MARGIN
       (for/list ([combo (in-combinations posn* on-bits)])
         (define bv (for/list ((b (in-list posn*))) (and (member b combo) #true)))
         (make-node bv)))))
  (apply vc-append LATTICE-Y-MARGIN level-picts))

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

  (ppict-do
    (blank client-w client-h)
    #:go CENTER-COORD
    (let ((x (make-program-pict/blank)))
      (vc-append
      (make-lattice 1 (lambda (i) x))
      (make-lattice 2 (lambda (x) (apply hc-append 4 (list->component* x))) #;(lambda (i) x))
      (make-lattice 3 (lambda (i) x)))
      )
    )
  ))
  (define (add-bg p)
    (cc-superimpose (blank (+ 100 (pict-width p)) (+ 100 (pict-height p))) p))
  (define raco-pict (add-bg p))
  (void))
