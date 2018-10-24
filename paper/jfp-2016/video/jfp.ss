#lang at-exp slideshow

(require
  (only-in gtp-util natural->bitstring)
  (only-in racket/math exact-floor)
  (only-in scribble-abbrevs/scribble add-commas)
  gtp-plot/configuration-info
  gtp-plot/performance-info
  gtp-plot/plot
  gtp-plot/typed-racket-info
  pict-abbrevs
  pict/balloon
  ppict/2
  pict/shadow
  (only-in plot/utils ->brush-color)
  (only-in racket/list make-list first second third fourth fifth sixth)
  "author.rkt"
  "two-tone.rkt"
  "util.rkt")

;; -----------------------------------------------------------------------------

(module+ main
  (set-page-numbers-visible! #true)
  (set-spotlight-style! #:size 60 #:color (color%-update-alpha HIGHLIGHT-COLOR 0.6))
  (parameterize ([current-main-font MONO-FONT]
                 [current-font-size NORMAL-FONT-SIZE]
                 [current-titlet string->title])
    (void)
    ;(sec:title)
    ;(sec:contribution)
    ;(sec:gt-cost)
    ;;(sec:anecdotes)
    ;(pslide (make-section-break "The Method"))
    ;(sec:lattice)
    ;(sec:exhaustive-method)
    ;(pslide (make-section-break "Presenting the Data (???)"))
    (sec:dead-plot)
    #;(pslide #:go CENTER-COORD (make-section-break "Three Strategies"))
    #;(sec:type-boundary) ;; LOW PRIORITY
    #;(pslide #:go CENTER-COORD (make-section-break "Survey Design"))
    (void)))

;; -----------------------------------------------------------------------------

(define FSM-DATA (make-typed-racket-info "./src/fsm-6.4.rktd"))

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
  (define MT (make-program-pict/mixed #:show-boundary? #true))
  (pslide
    #:go (coord 1/2 1/4 'ct)
    #:alt [(make-notation-table
             (list @t{Program} (make-program-pict/blank)
                   @t{Component} (car (list->component* '(0)))
                   @t{Dependency} (make-sample-boundary-arrow)))]
    (let ((sd* (list->component* '(#true #false))))
      (make-notation-table
        (list @t{Mixed-Typed Program} MT
              @t{Statically-typed Component} (car sd*)
              @t{Dynamically-typed Component} (cadr sd*)
              @t{Type Boundary} (make-sample-boundary-arrow #:boundary? #true)))))
  (let* ((sd* (list->component* '(#true #false)))
         (sta-pict (car sd*))
         (dyn-pict (cadr sd*))
         (validate-coord (coord 1/2 1/3 'cc))
         (acc (apply hb-append (* 2 COLUMN-MARGIN) sd*))
         (edge-spec `((,sta-pict ,rc-find) (,dyn-pict ,lc-find) 0))
         (sd (scale (add-boundary-arrows acc edge-spec #:color TYPE-BOUNDARY-COLOR) 2))
         (sd (vc-append (blank (pict-width sd) (pict-height sd)) sd))
         (sd/int (add-thought/sta sd sta-pict (need-txt "Integer")))
         (sd/int-0 (add-thought/dyn sd/int dyn-pict @t{42}))
         (sd/int-1 (add-thought/dyn sd/int dyn-pict @t{'NaN}))
         (sd/los (add-thought/sta sd sta-pict (need-txt "Listof(Symbol)")))
         (sd/los-0 (add-thought/dyn sd/los dyn-pict @t{'(A B 3 D)}))
         (sd/ii (add-thought/sta sd sta-pict (need-txt "Bool->Bool")))
         (sd/ii-0 (add-thought/dyn sd/ii dyn-pict @t{#<function>})))
    (pslide
      #:go (coord 1/2 1/2 'cb)
      #:alt [sd]
      #:alt [sd/int]
      #:alt [sd/int-0 #:next #:go validate-coord (large-check-icon)]
      #:alt [sd]
      #:alt [sd/int]
      #:alt [sd/int-1 #:next #:go validate-coord (large-x-icon)]
      #:alt [sd]
      #:alt [sd/los]
      #:alt [sd/los-0 #:next #:go validate-coord (large-x-icon)]
      #:alt [sd]
      #:alt [sd/ii]
      #:alt [sd/ii-0 #:next #:go validate-coord (large-?-icon)]
      sd
      #:go (coord 1/2 3/5 'ct)
      (vc-append (h%->pixels 1/20)
        @t{Type boundaries impose a run-time cost!*}
        @smallt{* in a sound gradual typing system})))
  (let ((y-sep (h%->pixels 1/15))
        (x-offset (pict-width @t{1. })))
    (pslide
      #:go (coord 1/2 1/4 'ct)
      (tag-pict (bigger-program MT) 'MT)
      #:go (at-find-pict 'MT lb-find 'lt #:abs-y y-sep #:abs-x (- x-offset))
      #:next
      (lines-append
        (hb-append @t{   What is the overall cost of})
        (hb-append @t{   boundaries in a } @bt{gradual})
        (hb-append @t{   } @bt{typing system} @t{?}))
      ; #:next
      ; (tag-pict (lines-append (hb-append @t{1. What is the cost of the } @bt{type}) (hb-append @t{   } @bt{boundaries} @t{ in a program?}) (blank) (blank)) 'Q1)
      ; #:next
      ; #:go (at-find-pict 'Q1 lb-find 'lt #:abs-y y-sep)
      ; (lines-append (hb-append @t{2. What is the overall cost of}) (hb-append @t{   boundaries in a } @bt{gradual}) (hb-append @t{   } @bt{typing system} @t{?}))
      #:next
      #:go (coord 1/2 1/2 'cc)
      (add-rectangle-background
        #:color "plum"
        #:draw-border? #true
        #:x-margin 2/10
        #:y-margin 2
        (hb-append @t{Need a } @bt{method} @t{ to measure performance!}))))
  (void))

(define (sec:anecdotes)
  (define note*
    '(("About twice as slow on common queries" burns)
      #;("Areas for improvement: untyped matrix performance" setti)
      ("1275x slowdown" ballantyne)
      ("12 seconds slower" clements)
      ("The end-product appears to be a 50% performance hybrid due to boundary contracts" JG)
      ("50x slower" toronto)))
  (void))

(define (sec:lattice)
  (let* ((p0 (bigger-program (list->program '(#f #t #t #f))))
         (the-lattice-coord (coord 1/2 SLIDE-TOP 'ct))
         (the-step-coord (coord SLIDE-LEFT SLIDE-TOP 'lb #:abs-y -4))
         (-the-step-coord (coord SLIDE-RIGHT SLIDE-TOP 'rb #:abs-y -4))
         (p-typed (make-node '(#t #t #t #t)))
         (time-y-sep -2)
         (total-bits 4)
         (make-overhead (lambda (cfg) (overhead FSM-DATA (configuration-info->mean-runtime cfg))))
         (cfg->o-coord (lambda (tag) (at-find-pict tag rt-find 'rb #:abs-y time-y-sep)))
        )
    (pslide
      #:alt [#:go CENTER-COORD
             p0
             #:next
             #:go the-lattice-coord
             p-typed
             #:set (let ((pp ppict-do-state)
                         (lbl ((make-make-step-label) @t{Add types})))
                     (pin-arrow-line MIGRATION-ARROW-SIZE
                                     pp
                                     p0 ct-find
                                     p-typed cb-find
                                     #:line-width MIGRATION-ARROW-WIDTH
                                     #:color BLACK
                                     #:x-adjust-label (* 5/7 (pict-width lbl))
                                     #:label lbl))]
      #:go the-lattice-coord
      #:alt [p-typed]
      (make-lattice 4 make-node
                    #:x-margin (w%->pixels 1/70)
                    #:y-margin (h%->pixels 1/9))
      ;#:alt [#:go the-step-coord (make-step-label @t{Make configurations})]
      #:next
      #:alt [#:set (for/fold ((acc ppict-do-state))
                             ((cfg (in-configurations FSM-DATA))
                              (i (in-naturals)))
                     (define tag (bitstring->tag (natural->bitstring i #:bits total-bits)))
                     (ppict-do
                       acc
                       #:go (at-find-pict tag lt-find 'lb #:abs-y time-y-sep)
                       (runtime->pict (configuration-info->mean-runtime cfg))))
                    ;#:go -the-step-coord (make-step-label @t{Measure runtime})
                    ]
      #:alt [#:set (ppict-do
                     ppict-do-state
                     #:go (cfg->o-coord 'cfg-0000)
                     (overhead->pict 1))]
      #:set (for/fold ((acc ppict-do-state))
                      ((cfg (in-configurations FSM-DATA))
                       (i (in-naturals)))
              (define tag (bitstring->tag (natural->bitstring i #:bits total-bits)))
              (ppict-do
                acc
                #:go (cfg->o-coord tag)
                (overhead->pict (make-overhead cfg))))
      #:next
      #:set (for/fold ((acc (cellophane ppict-do-state 0.4)))
                      ((cfg (in-configurations FSM-DATA))
                       (i (in-naturals)))
              (define tag (bitstring->tag (natural->bitstring i #:bits total-bits)))
              (ppict-do
                acc
                #:go (at-find-pict tag cc-find 'cc)
                (if (< (make-overhead cfg) 2)
                  (large-check-icon)
                  (large-x-icon))))))
  (void))

(define (sec:exhaustive-method)
  (let* ((make-step (make-make-step-label 1))
         (good-pict @t{"good"})
         (tp*
           (list
             (cons @t{1. Typed program}
                   (make-node '(#t #t #t #t)))
             (cons @t{2. Measure all configurations}
                   (make-lattice-icon))
             (cons (hb-append @t{3. Count % of } good-pict @t{ configs.})
                   (make-check-x-fraction)))))
    (pslide
      (blank client-w client-h)
      #:go HEADING-COORD
      (subtitle-text "Method: exhaustive perf. eval.")
      #:go (coord 1/2 1/4 'ct)
      (make-notation-table
        #:col-align (list lc-superimpose cc-superimpose)
        tp*)
      #:go (coord 1/2 SLIDE-BOTTOM 'ct)
      @t{Repeat for other programs}
      #:next
      #:go (at-find-pict good-pict lb-find 'lt #:abs-y 4)
      (rule (pict-width good-pict) 6 #:color HIGHLIGHT-COLOR)))
  (void))

(define (sec:dead-plot)
  (let* ((x-sep 50)
         (ra (right-arrow #:color HIGHLIGHT-COLOR))
         (lhs-pict
           (hc-append 4 (make-lattice-icon) @subtitle-text{+} @bt{D}))
         (rhs-pict
           (make-check-x-fraction)))
    (pslide
      #:go HEADING-COORD
      (subtitle-text "D-deliverable")
      #:go (coord 1/10 20/100 'lt)
      (lines-append
        (hb-append @t{A configuration is } @bt{D}@it{-deliverable} @t{ if its})
        (hb-append @t{performance is no worse than a factor })
        (hb-append @t{of } @bt{D} @t{ slowdown compared to the baseline}))
      #:go (coord 1/10 50/100 'lt)
      (hc-append x-sep lhs-pict ra rhs-pict)))
  (let* ((the-plot-w (w%->pixels 1/2))
         (the-plot-h (h%->pixels 1/2))
         (the-max 20)
         (the-fsm-plot
           (parameterize ((*OVERHEAD-MAX* the-max)
                          (*OVERHEAD-PLOT-WIDTH* the-plot-w)
                          (*OVERHEAD-PLOT-HEIGHT* the-plot-h)
                          (*OVERHEAD-SHOW-RATIO* #false)
                          (*OVERHEAD-LEGEND?* #false))
             (frame (overhead-plot FSM-DATA))))
         (the-blank-plot (blank the-plot-w the-plot-h)))
    (pslide
      #:go CENTER-COORD
      #:alt [(add-overhead-axis-labels the-blank-plot #:bounds? #false)]
      #:alt [(add-overhead-axis-labels the-blank-plot #:bounds? #true)]
      #:alt [(add-overhead-axis-labels the-blank-plot #:bounds? #true #:x-max (number->string the-max))]
      (add-overhead-axis-labels the-fsm-plot #:x-max (number->string the-max))
      ))
  (void))

;; -----------------------------------------------------------------------------

(define (add-overhead-axis-labels pp [legend? #t] #:bounds? [bounds? #t] #:x-max [x-max-str "N>1"])
  (define xy-margin 20)
  (define label-margin 60)
  (define y-label (make-check-x-fraction))
  (define x-label @bt{D})
  (define y-max @t{100%})
  (define y-min @t{0%})
  (define x-min @t{1})
  (define x-max (t (format "~a" x-max-str)))
  (define fp (frame-plot pp))
  (if legend?
    (let ((p (vc-append label-margin
                        (hc-append label-margin y-label fp)
                        (hc-append (+ label-margin (pict-width y-label)) (blank) x-label))))
      (if bounds?
        (for/fold ((acc p))
                  ((pin-spec (list (list y-max lt-find 'rt (- xy-margin) 0)
                                   (list y-min lb-find 'rb (- xy-margin) 0)
                                   (list x-max rb-find 'rt 0 xy-margin)
                                   (list x-min lb-find 'lt 0 xy-margin))))
          (ppict-do
            acc
            #:go (at-find-pict fp (second pin-spec) (third pin-spec) #:abs-x (fourth pin-spec) #:abs-y (fifth pin-spec))
            (first pin-spec)))
        p))
    fp))

(define (frame-plot p)
  (add-axis-arrow (add-axis-arrow p 'x) 'y))

(define (add-axis-arrow p xy)
  (define find-dest
    (case xy
      ((x)
       rb-find)
      ((y)
       lt-find)
      (else (raise-argument-error 'add-axis-arrow "(or/c 'x 'y)" 1 p xy))))
  (pin-arrow-line 20 p p lb-find p find-dest #:line-width 6))

(define (bool*->tag b*)
  (bitstring->tag (bool*->bitstring b*)))

(define (make-node b*)
  (define pp (list->program b* #:margin COMPONENT-LATTICE-MARGIN))
  (define pp/bg
    (add-rectangle-background pp
                              #:radius 4
                              #:color SURVEY-COLOR
                              #:draw-border? #true
                              #:x-margin 1/16
                              #:y-margin 1/7))
  (tag-pict pp/bg (bool*->tag b*)))

(define (bitstring->tag str)
  (string->symbol (string-append "cfg-" str)))

(define make-section-break
  (let ((*i (box 1)))
    (lambda (str)
      (define the-font-size TITLE-FONT-SIZE)
      (define curr-i (unbox *i))
      (set-box! *i (+ 1 curr-i))
      (define txt
        (text str TITLE-FONT the-font-size))
      (define bg
        (rectangle/2t (+ (* margin 2) client-w)
                      (* 5 the-font-size)
                      #:border-width 1
                      #:border-color BLACK
                      #:color-1 (rgb-triplet->color% (->brush-color curr-i))
                      #:color-2 WHITE))
      (cc-superimpose bg txt))))

(define (make-notation-table kv**
                             #:col-align [col-align lc-superimpose]
                             #:row-align [row-align cc-superimpose])
  (table 2
         (flatten kv**)
         col-align row-align
         (w%->pixels 1/15) (h%->pixels 1/10)))

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

(define (list->program bool* #:margin [margin COMPONENT-MARGIN])
  (define c* (list->component* bool*))
  (apply hb-append margin c*))

(define (add-program-edges c* #:show-boundary? [show-boundary? #false])
  (match c*
    [(list c0 c1 c2 c3)
     (define the-pict (apply hb-append COMPONENT-MARGIN c*))
     (for/fold ((acc the-pict))
               ((edge-spec (in-list `(((,c0 ,rt-find) (,c2 ,lt-find)  50)
                                      ((,c0 ,rb-find) (,c3 ,lb-find) -50)
                                      ((,c1 ,rc-find) (,c2 ,lc-find)   0)
                                      ((,c2 ,rc-find) (,c3 ,lc-find)   0)))))
       (define boundary-color
         (if (and show-boundary? (not (eq? (pict-tag (caar edge-spec))
                                           (pict-tag (caadr edge-spec)))))
           TYPE-BOUNDARY-COLOR
           BLACK))
       (add-boundary-arrows acc edge-spec #:color boundary-color))]
    [_
      (raise-argument-error 'add-program-edges "(list pict? pict? pict? pict?)" c*)]))

(define (add-boundary-arrows acc edge-spec #:color [boundary-color BLACK])
  (add-boundary-X pin-arrows-line acc edge-spec #:color boundary-color))

(define (add-boundary-arrow acc edge-spec #:color [boundary-color BLACK])
  (add-boundary-X pin-arrow-line acc edge-spec #:color boundary-color))

(define (add-boundary-X arrow-fn acc edge-spec #:color [boundary-color BLACK])
  (define dom-spec (car edge-spec))
  (define cod-spec (second edge-spec))
  (define dom-pict (car dom-spec))
  (define cod-pict (car cod-spec))
  (define the-angle (caddr edge-spec))
  (arrow-fn
    COMPONENT-ARROW-SIZE
    acc
    dom-pict (second dom-spec)
    cod-pict (second cod-spec)
    #:start-angle (- the-angle)
    #:end-angle the-angle
    #:line-width COMPONENT-ARROW-WIDTH
    #:color boundary-color))

(define (add-thought/sta base tgt txt
                         #:adjust-x [adjust-x values]
                         #:adjust-y [adjust-y #f])
  (add-thought base tgt txt WHITE #:adjust-x adjust-x #:adjust-y (or adjust-y (lambda (n) (* 2 n)))))

(define (add-thought/dyn base tgt txt
                         #:adjust-x [adjust-x #f]
                         #:adjust-y [adjust-y values])
  (add-thought base tgt txt WHITE #:adjust-x (or adjust-x -) #:adjust-y adjust-y))

(define (add-thought base tgt txt color
                     #:adjust-x [adjust-x values]
                     #:adjust-y [adjust-y values])
  (define b-x (adjust-x (* 1/10 (pict-width txt))))
  (define b-y (adjust-y (pict-height txt)))
  (define b-pict (wrap-balloon txt 's b-x b-y color BALLOON-RADIUS))
  (pin-balloon b-pict base tgt ct-find))

(define (need-txt str)
  (hb-append @t{need } (bt str)))

(define (make-sample-boundary-arrow #:boundary? [boundary? #false])
  (define dom-pict (blank))
  (define cod-pict (blank))
  (define acc (hc-append COLUMN-MARGIN dom-pict cod-pict))
  (define edge-spec `((,dom-pict ,rc-find) (,cod-pict ,lc-find) 0))
  (define b-color (if boundary? TYPE-BOUNDARY-COLOR BLACK))
  (add-boundary-arrows acc edge-spec #:color b-color))

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

(define (make-lattice total-bits make-node
                      #:x-margin [x-margin LATTICE-NODE-X-MARGIN]
                      #:y-margin [y-margin LATTICE-NODE-Y-MARGIN])
  (define posn* (build-list total-bits values))
  (define level-picts
    (for/list ([on-bits (in-range total-bits -1 -1)])
      (apply hc-append x-margin
       (for/list ([combo (in-combinations posn* on-bits)])
         (define bv (for/list ((b (in-list posn*))) (and (member b combo) #true)))
         (make-node bv)))))
  (apply vc-append y-margin level-picts))

(define (bool*->bitstring b*)
  (list->string
    (for/list ((b (in-list b*)))
      (if b #\1 #\0))))

(define (runtime->pict n)
  (t (string-append (add-commas (exact-floor n)) " ms")))

(define (overhead->pict n)
  (define n-str
    (if (< n 0)
      (~r n #:precision '(= 2))
      (add-commas (exact-floor n))))
  (bt (string-append n-str "x")))

(define (make-make-step-label [n 0])
  (let ((*count (box n)))
    (lambda (p)
      (define current-count (unbox *count))
      (set-box! *count (+ 1 current-count))
      (add-rectangle-background
        #:color LABEL-COLOR
        #:draw-border? #true
        #:x-margin 1/10
        #:y-margin 16/15
        (hb-append (t (format "~a. " (number->string current-count))) p)))))

(define (smallt str)
  (parameterize ((current-font-size SMALL-FONT-SIZE)) (t str)))

(define (make-fraction t-pict b-pict)
  (vc-append t-pict (rule (pict-width t-pict) 8) b-pict))

(define (make-check-x-fraction)
  (apply make-fraction
         (pict-bbox-sup (small-check-icon)
                        (hc-append 6 (small-check-icon) @subtitle-text{+} (small-x-icon)))))

(define (make-lattice-icon)
  (define y-max (h%->pixels 1/6))
  (define x-max (* 3 y-max))
  (scale-to-fit (make-lattice 4 make-node #:x-margin 4 #:y-margin 2) x-max y-max))

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

  #;(let* ((the-plot-w (w%->pixels 1/2))
         (the-plot-h (h%->pixels 1/2))
         (the-max 20)
         (the-fsm-plot
           (parameterize ((*OVERHEAD-MAX* the-max)
                          (*OVERHEAD-PLOT-WIDTH* the-plot-w)
                          (*OVERHEAD-PLOT-HEIGHT* the-plot-h)
                          (*OVERHEAD-SHOW-RATIO* #false)
                          (*OVERHEAD-LEGEND?* #false))
             (frame (overhead-plot FSM-DATA))))
         (the-blank-plot (blank the-plot-w the-plot-h)))
    (ppict-do
      (blank client-w client-h)
      #:go CENTER-COORD
      #:alt [(add-overhead-axis-labels the-blank-plot #:bounds? #false)]
      #:alt [(add-overhead-axis-labels the-blank-plot #:bounds? #true)]
      #:alt [(add-overhead-axis-labels the-blank-plot #:bounds? #true #:x-max (number->string the-max))]
      (add-overhead-axis-labels the-fsm-plot #:x-max (number->string the-max))
      ))

  ))
  (define (add-bg p)
    (cc-superimpose (blank (+ 100 (pict-width p)) (+ 100 (pict-height p))) p))
  (define raco-pict (add-bg p))
  (void))
