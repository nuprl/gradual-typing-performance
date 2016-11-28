#lang typed/racket/base

;; Create L-N/M plots for .rktd files
;;
;; These plot overhead factors against the number of "acceptable"
;;  configurations relative to the overhead,
;;  subject to at most L additional conversion steps

(provide
  count-configurations/mean ;; TODO not a great interface

  ;; TODO
  ;; load a summary to an interactive plot
  ;; summary = vectorof listof unixtime

  SHIM-FOR-BARCHART-ALIGNMENT
  integer->pen-style
  integer->brush-style
  integer->line-width
  integer->symbol

  plot-mean-bars
  plot-deliverable
  plot-exact-configurations
  plot-typed/untyped-ratio
  plot-indirection-cost
  plot-karst
  plot-srs-sound
  plot-srs-precise
  plot-delta

  ;; ---

  plot-traces
  ;; (-> Path-String Pict)
  ;; Plot convergence for each configuration

  lnm-plot
  ;; Create an L-NM plot based on the given parameters
  ;; Returns a list of plots:
  ;; - 1 for each given value of L (unless `#:single-plot?` is `#t`
  ;; - each plot has 1 line for each given Summary

  lnm-bar

  path-plot
  ;; Plots the number of acceptable paths.
  ;; Acceptable paths have no overhead greater than the cutoff along any point
  ;; This number is usually DISMAL

  #;(death-plot (->* [(Listof Summary)
                    #:L (U Index (Listof Index))]
                   [#:N Index
                    #:M Index
                    #:max-overhead Index
                    #:num-samples Positive-Integer
                    #:font-face String
                    #:labels? Boolean
                    #:cutoff-proportion Real
                    #:plot-width Positive-Integer
                    #:plot-height Positive-Integer]
                    (Listof pict)))
  ;; Plot a bunch of summaries to the same grid.
  ;; - y-axis is the proportion of good configs
  ;; - aggregation is plain old (arithmetic) mean
  ;; TODO try more kinds y-axes
)

;; -----------------------------------------------------------------------------

(require
  racket/match
  racket/set
  racket/string
  racket/sequence
  (only-in racket/file file->value)
  plot/typed/no-gui
  plot/typed/utils
  typed/pict
  racket/list
  (only-in typed/racket/random random-sample)
  (only-in racket/math exact-floor exact-ceiling exact-round)
  (only-in math/statistics mean stddev/mean)
  (only-in racket/format ~r)
  gtp-summarize/bitstring
  gtp-summarize/lnm-parameters
  gtp-summarize/path-util
  gtp-summarize/summary
)
;(define-type Pict pict)

(define ERRLOC 'lnm-plot)

(define-type rTree (Rec t (U renderer2d (Listof rTree))))

;; -----------------------------------------------------------------------------
;; --- plotting

(: lnm-plot
  (->* [(U Summary (Listof Summary))]
       [#:L (U Natural
               ;; To make 1 plot
               (Listof Natural)
               ;; To make multiple plots
               (Listof (List Natural Plot-Pen-Style)))
               ;; To make multiple plots, but set the pen for each.
               ;; (Typically when `#:single-plot?` is `#t`)
        #:N (U #f Natural)
        #:M (U #f Natural)
        #:cutoff-proportion (U #f Real)
        #:max-overhead (U #f Natural)
        #:num-samples Positive-Integer
        #:pdf? Boolean
        #:plot-height Positive-Integer
        #:plot-width Positive-Integer
        #:single-plot? Boolean]
       (Listof pict)))
(define (lnm-plot S*-arg
                  #:L [L*-arg (*L*)]
                  #:N [N (*N*)]
                  #:M [M (*M*)]
                  #:cutoff-proportion [cutoff-proportion (*CUTOFF-PROPORTION*)]
                  #:font-face [font-face (*PLOT-FONT-FACE*)]
                  #:max-overhead [xmax (*MAX-OVERHEAD*)]
                  #:num-samples [num-samples (*NUM-SAMPLES*)]
                  #:pdf? [pdf? (*PDF?*)]
                  #:plot-height [height (*PLOT-HEIGHT*)]
                  #:plot-width [width (*PLOT-WIDTH*)]
                  #:single-plot? [single-plot? (*SINGLE-PLOT?*)])
  (define S*
    (cond
     [(null? S*-arg)
      (error ERRLOC "Cannot make plot for 0 Summary objects")]
     [(list? S*-arg)
      S*-arg]
     [else
      (list S*-arg)]))
  (define L*
    (cond
     [(null? L*-arg)
      (error 'lnm-plot "Cannot make picture for empty list of L-values")]
     [(not (list? L*-arg))
      (list (list L*-arg (line-style)))]
     [(andmap pair? L*-arg)
      L*-arg]
     [else
      (for/list : (Listof (List Natural Plot-Pen-Style))
                ([l (in-list L*-arg)])
        (list l (line-style)))]))
  (define ymax : Index
    ;; Assert that all summaries have the same number of configurations
    ;; (Sorry the error-handling is a little wild)
    (assert (get-num-configurations (car S*)) index?))
    ;(or
    ; (for/fold : (Option Index)
    ;           ([prev : (Option Index) #f])
    ;           ([S (in-list S*)]
    ;            [i (in-naturals)])
    ;   (define nv (get-num-configurations S))
    ;   (if (and prev (not (= prev nv)))
    ;     (let ([p1 (get-project-name (list-ref S* (assert (- i 1) index?)))]
    ;           [p2 (get-project-name S)])
    ;       (raise-user-error 'lnm (format "datasets for '~a' and '~a' have ~a and ~a modules, cannot plot on same graph" p1 p2 nv prev)))
    ;     (assert nv index?)))
    ; (raise-user-error 'lnm "got 0 datasets to summarize")))
  (define cutoff-point (and cutoff-proportion (* cutoff-proportion ymax)))
  ;; Make renderers for the lines
  (define N-line
    (and N (not pdf?)
         (vrule N
                #f ymax
                #:color (*N-COLOR*)
                #:style (*N-STYLE*)
                #:width (*N-WIDTH*))))
  (define M-line
    (and M (not pdf?)
         (vrule (assert M real?)
                #f ymax
                #:color (*M-COLOR*)
                #:style (*M-STYLE*)
                #:width (*M-WIDTH*))))
  (define cutoff-line
    (and cutoff-point (not pdf?)
         (hrule cutoff-point
                #f xmax
                #:color (*CUTOFF-COLOR*)
                #:style (*CUTOFF-STYLE*)
                #:width (*CUTOFF-WIDTH*))))
  (define elem* (for/list : (Listof renderer2d)
                          ([x (list N-line M-line cutoff-line)] #:when x) x))
  ;; Get ticks
  (define x-major-ticks (compute-xticks (*X-TICKS*) (*X-NUM-TICKS*)))
  (define y-major-ticks
    (case (*Y-STYLE*)
     [(count)
      (if pdf?
        (plot-y-ticks)
        (compute-yticks ymax (*Y-NUM-TICKS*)
          #:exact (if cutoff-point (list cutoff-point ymax) (list ymax))))]
     [(%)
      (if pdf?
        (raise-user-error ERRLOC "Don't know how to plot PDF with % on y-axis")
        (compute-yticks 100 (*Y-NUM-TICKS*)
          #:units "%"
          #:exact (if cutoff-proportion (list (* 100 cutoff-proportion) 100) (list 100))))]
     [else
      (raise-user-error ERRLOC (format "Unexpected value '~a' for *Y-STYLE* parameter" (*Y-STYLE*)))]))
  (define y-label
    (case (*Y-STYLE*)
     [(count)
      "Count"]
     [(%)
      (if pdf?
        "???"
        "% Configs.")]
     [else
      (raise-user-error ERRLOC (format "Unexpected value '~a' for *Y-STYLE* parameter" (*Y-STYLE*)))]))
  ;; Set plot parameters ('globally', for all picts)
  (parameterize (
    [plot-x-ticks (ticks-add? x-major-ticks (*X-MINOR-TICKS*))]
    [plot-x-transform (if (*LOG-TRANSFORM?*) log-transform id-transform)]
    [plot-y-ticks (ticks-add? y-major-ticks (*Y-MINOR-TICKS*))]
    [plot-x-far-ticks no-ticks]
    [plot-y-far-ticks no-ticks]
    [plot-tick-size (*TICK-SIZE*)]
    [plot-font-face (*PLOT-FONT-FACE*)]
    [plot-font-size (* (*PLOT-FONT-SCALE*) (*PLOT-WIDTH*))])
    (define F-config**
      (let ([next-color (make-palette (length S*))]
            [next-shape (make-shapegen)])
        (for/list : (Listof (Listof renderer2d))
                  ([L+style (in-list L*)])
          (define L (car L+style))
          (define st (cadr L+style)) ;; TODO
          (for/fold : (Listof renderer2d)
                  ([acc : (Listof renderer2d) '()])
                  ([S (in-list ((inst sort Summary String) S* string<? #:key summary->version))]
                   [i (in-naturals 1)])
            (define lbl (and (*LINE-LABELS?*)
                             (format "~a~a"
                               (if (*GROUP-BY-TITLE?*)
                                 (summary->version S)
                                 (summary->label S))
                               (if (< 1 (length L*))
                                 (format " (L=~a)" L)
                                 ""))))
            (define c (next-color))
            (define shape (next-shape))
            (define sty (integer->pen-style i))
            (define w (integer->line-width i))
            (cond
             [(*HISTOGRAM?*)
              (cons
                (area-histogram
                  (count-configurations/mean
                    S (assert L index?)
                    #:cache-up-to (assert xmax index?)
                    #:percent? (eq? (*Y-STYLE*) '%)
                    #:pdf? pdf?)
                  (linear-seq 0 xmax num-samples)
                  #:x-min 0
                  #:x-max xmax
                  #:samples num-samples
                  #:label (and (*LINE-LABELS?*) lbl)
                  #:line-style st
                  #:line-color c
                  #:line-width w) acc)]
             [(*ERROR-BAR?*)
              (append
                (discrete-function
                  (count-configurations/standard-error
                  ;count-configurations/confidence-interval
                    S (assert L index?)
                    #:cache-up-to (assert xmax index?)
                    #:percent? (eq? (*Y-STYLE*) '%)
                    #:pdf? pdf?)
                  0 xmax
                  #:color c
                  #:label (and (*LINE-LABELS?*) lbl)
                  #:samples num-samples
                  #:style sty
                  #:sym shape
                  #:width w) acc)]
             [else
              (cons
                (function
                  (count-configurations/mean
                    S (assert L index?)
                    #:cache-up-to (assert xmax index?)
                    #:percent? (eq? (*Y-STYLE*) '%)
                    #:pdf? pdf?)
                  0 xmax
                  #:color c
                  #:label (and (*LINE-LABELS?*) lbl)
                  #:samples num-samples
                  #:style sty
                  #:width w) acc)]
              )))))
    (define ticks-renderer : (Listof renderer2d)
      (let ([x? (*X-TICK-LINES?*)]
            [y? (*Y-TICK-LINES?*)])
        (cond [(and x? y?) (tick-grid)]
              [x?          (list (x-tick-lines))]
              [y?          (list (y-tick-lines))]
              [else        '()])))
    (: make-plot (-> (Listof renderer2d) pict))
    (define (make-plot LNM)
      (cast ;; dammit, Neil re-defined 'Pict'
       (plot-pict (append ticks-renderer LNM elem*)
        #:x-min 1
        #:x-max xmax
        #:y-min 0
        #:y-max (if pdf? #f (if (eq? '% (*Y-STYLE*)) 100 ymax))
        #:x-label (and (*AXIS-LABELS?*) "Overhead (vs. untyped)")
        #:y-label (and (*AXIS-LABELS?*) y-label)
        #:title (and (*SINGLE-PLOT?*) (get-project-name (car S*)))
        #:legend-anchor (*LEGEND-ANCHOR*)
        #:width width
        #:height height) pict))
    (if single-plot?
      (list (make-plot (apply append F-config**)))
      ;; 2016-04-13 : Drawing y-axis on all picts because otherwise the sizing gets messy
      (for/list : (Listof pict)
                ([F-config* (in-list F-config**)])
        (make-plot F-config*)))))

(: plot-mean-bars (-> (Listof Summary) pict))
(define (plot-mean-bars S*)
  (define num-series (length S*))
  (define p
    (plot-pict
      (for/list : (Listof renderer2d)
                ([S (in-list S*)]
                 [i (in-naturals 2)])
        (define c (->pen-color i))
        (define lbl (format "~a" (summary->version S)))
        (discrete-histogram
          (for/list : (Listof (List Any Real))
                    ([cfg (all-configurations S)])
            (list (bitstring->natural cfg) (configuration->mean-runtime S cfg)))
          #:add-ticks? #t
          #:style (integer->brush-style i)
          #:label lbl
          #:line-width 1
          #:line-color c
          #:color c
          #:x-min i
          #:skip (+ 1 num-series)))
      #:x-label "Config #"
      #:y-label "Time (ms)"
      #:legend-anchor 'bottom-right
      #:y-min 0
      #:width (*PLOT-WIDTH*)
      #:height (*PLOT-HEIGHT*)))
  (cast p pict))

;; D-deliverable is a count
;; Each config included in the count is included with some error ratio
;; Make a histogram with error bars, error determined by each config's runtime
;; e.g. "95% certain this config is D-deliverable", for each config
(: plot-deliverable (-> Nonnegative-Real (Listof (Vectorof String)) pict))
(define (plot-deliverable D vec*)
  (define num-bm (length vec*))
  (define num-versions (vector-length (car vec*)))
  (define RW (*RECTANGLE-WIDTH*))
  (define x-offset (cast (+ (/ (*RECTANGLE-BORDER-WIDTH*) 2) RW) Nonnegative-Real))
  (parameterize ([plot-x-ticks (alphabet-ticks num-bm #:offset (+ 1 x-offset) #:skip (*RECTANGLE-SKIP*))]
                 [plot-x-far-ticks no-ticks]
                 [plot-y-ticks (compute-yticks 100 (*Y-NUM-TICKS*) #:units "%")])
    (define p
      (plot-pict
        (for/list : (Listof (Listof (List renderer2d renderer2d)))
                  ([vec (in-list vec*)]
                   [benchmark-index (in-naturals)])
          ;; -- TODO could 1st get counts for 3 versions, the draw histogram + error bars
          (for/list : (Listof (List renderer2d renderer2d))
                    ([rktd (in-vector vec)]
                     [i (in-naturals 1)])
            (define c (->pen-color i))
            (define S (from-rktd rktd))
            (define num-configs (get-num-configurations S))
            ;; -- get mean count + lo count + hi count for each config
            ;; TODO does it matter if I use "baseline" or get a ratio for each?
            ;;  (just experiment, then try researching)
            (define u* (untyped-runtimes S))
            (define-values (lo-count mean-count hi-count)
              (for/fold : (Values Real Real Real)
                        ([lo-count 0] [mean-count 0] [hi-count 0])
                        ([cfg (all-configurations S)])
                (define r* ;; List of overheads for `cfg`
                  (for/list : (Listof Real)
                            ([r (in-list (configuration->runtimes S cfg))]
                             [u (in-list u*)])
                    (/ r u)))
                (define m (mean r*))
                (define s (error-bound r*))
                (values (add1-when (<= (- m s) D) mean-count)
                        (add1-when (<= m D) mean-count)
                        (add1-when (<= (+ m s) D) mean-count))))
            (define y-max (* 100 (/ mean-count num-configs)))
            (define series-x-min (* benchmark-index (*RECTANGLE-SKIP*)))
            (define version-x-offset (* x-offset (- i 1)))
            (define x-center (+ series-x-min version-x-offset))
            (list
              (let ([y-hi (* 100 (/ hi-count num-configs))]
                    [y-lo (* 100 (/ lo-count num-configs))])
                (lnm-error-bar x-center y-max (ivl y-hi y-lo) #:color c))
              (rectangles
                (let ([RW/2 (/ RW 2)])
                  (list (list (ivl (- x-center RW/2) (+ x-center RW/2))
                              (ivl 0 y-max))))
                #:color c
                #:style (integer->brush-style i)
                #:line-color c
                #:line-width (*RECTANGLE-BORDER-WIDTH*)
                #:alpha (*POINT-ALPHA*)
                #:label #f))))
        #:x-label "Benchmark"
        #:y-label #f ;(format "% ~a-deliverable" D)
        #:legend-anchor 'bottom-right
        #:y-min 0
        #:y-max 100
        #:width (*PLOT-WIDTH*)
        #:height (*PLOT-HEIGHT*)))
    (cast p pict)))

;; Possibly-asymmetric error bars
(: lnm-error-bar (->* [Real Real (U ivl Real)] [#:color (List Real Real Real)] renderer2d))
(define (lnm-error-bar x y v #:color [c (list 0 0 0)])
  (if (ivl? v)
    (let ([W/2 (/ (*ERROR-BAR-WIDTH*) 2)]
          [y-min (or (ivl-min v) (error 'lnm-error-bar "Bad interval ~a" v))]
          [y-max (or (ivl-max v) (error 'lnm-error-bar "Bad interval ~a" v))])
      (lines
        (list
          (list (- x W/2) y-min)
          (list (+ x W/2) y-min)
          (list x         y-min)
          (list x         y-max)
          (list (- x W/2) y-max)
          (list (+ x W/2) y-max))
        #:alpha 1
        #:color c
        #:width (*ERROR-BAR-LINE-WIDTH*)))
    (error-bars (list (list x y v))
      #:alpha 1
      #:color c
      #:line-width (*ERROR-BAR-LINE-WIDTH*) ;(*RECTANGLE-BORDER-WIDTH*)
      #:width (*ERROR-BAR-WIDTH*))))

;; Return upper & lower confidence interval, or upper and lower stddev
;; - stddev usually gives smaller interval than CI
(: error-bound (-> (Listof Real) Real))
(define (error-bound v*)
  ;(define s (stddev/mean m v*))
  ;(* s 2)
  (define cv
    (case (*CONFIDENCE-LEVEL*)
     [(95) 1.96]
     [(98) 2.326]
     [else (error 'error-bounds "Unknown confidence level '~a'" (*CONFIDENCE-LEVEL*))]))
  (confidence-interval v* #:cv cv))

(: confidence-interval (->* [(Listof Real)] [#:cv Nonnegative-Real] Nonnegative-Real))
(define (confidence-interval x* #:cv [cv 1.96])
  (define u (mean x*))
  (define n (length x*))
  (define s (stddev/mean u x*))
  (define cv-offset (/ (* cv s) (sqrt n)))
  (if (negative? cv-offset)
    (raise-user-error 'confidence-interval "got negative cv offset ~a\n" cv-offset)
    cv-offset))

(define-syntax-rule (add1-when p v)
  (if p (+ v 1) v))

;; Make a line for each configuration in the dataset
(: plot-traces (-> (Listof Summary) pict))
(define (plot-traces S*)
  (define S
    (if (and (not (null? S*)) (null? (cdr S*)))
      (car S*)
      (raise-user-error 'plot-traces "Sorry expected exactly 1 summary, got '~a'" S*)))
  (define lo (min-runtime S))
  (define hi (max-runtime S))
  (define num-iters (get-num-iterations S))
  (define num-configs (get-num-configurations S))
  (define band-size : Real
    (ceiling (/ (- hi lo) (*TRACE-NUM-COLORS*))))
  (define (runtime->line-color (r : Real)) : Natural
    (let loop : Natural ([acc : Natural 0])
      (if (< r (* (+ 1 acc) band-size))
        (assert (max (- (*TRACE-NUM-COLORS*) acc) 0) index?)
        (loop (+ acc 1)))))
  (define p
    (plot-pict
      (for/list : (Listof renderer2d)
                ([cfg (all-configurations S)])
        (define r* (configuration->runtimes S cfg))
        (lines
          (for/list : (Listof (List Real Real))
                    ([r (in-list r*)]
                     [i (in-naturals)])
            (list i r))
          #:color (runtime->line-color (car r*))
          ;#:label (and (< num-configs 64) cfg)
          #:alpha 0.6))
      #:x-min 0
      #:x-max num-iters
      #:x-label "Iteration #"
      #:y-label "Time (ms)"
      ;#:legend-anchor (*LEGEND-ANCHOR*)
      #:width (*PLOT-WIDTH*)
      #:height (*PLOT-HEIGHT*)))
  (cast p pict))

(define config-x-jitter : Real 0.4)

(: plot-exact-configurations (-> (Listof Summary) pict))
(define (plot-exact-configurations S*)
  (define num-configs (get-num-configurations (car S*)))
  (define small-enough? (< num-configs (expt 2 6)))
  (parameterize ([plot-x-ticks (if small-enough? (list->ticks (range num-configs)) no-ticks)]
                 [plot-x-far-ticks no-ticks]
                 [plot-y-ticks (linear-ticks #:number 3)])
    (define p
      (plot-pict
        (append
          (if small-enough?
            (make-vrule* num-configs)
            '())
          (for/list : (Listof renderer2d)
                    ([S (in-list S*)]
                     [i (in-naturals (*COLOR-OFFSET*))])
              (lnm-points i  #:label (format "~a" (summary->version S))
                (append*
                  (for/list : (Listof (Listof (List Real Real)))
                            ([cfg (all-configurations S)])
                    (define t* (configuration->runtimes S cfg))
                    (define x-center (bitstring->natural cfg))
                    (define x* (linear-seq (- x-center config-x-jitter)
                                           (+ x-center config-x-jitter)
                                           (length t*)))
                    (for/list : (Listof (List Real Real))
                              ([x (in-list x*)]
                               [t (in-list t*)])
                      (list x t)))))))
        #:x-label "Configuration"
        #:y-label "Time (ms)"
        #:y-min 0
        #:y-max (*Y-MAX*)
        #:x-max (- num-configs 0.5)
        #:legend-anchor (*LEGEND-ANCHOR*)
        #:width (*PLOT-WIDTH*)
        #:height (*PLOT-HEIGHT*)))
    (cast p pict)))

(: plot-typed/untyped-ratio (-> (Listof (Vectorof String)) pict))
(define (plot-typed/untyped-ratio vec*)
  (define num-benchmarks (length vec*))
  (parameterize ([plot-x-ticks (alphabet-ticks num-benchmarks)]
                 [plot-x-far-ticks no-ticks]
                 [plot-y-ticks (compute-yticks 3 (*Y-NUM-TICKS*) #:units "x")])
    (define p
      (plot-pict
        (append
          (make-vrule* num-benchmarks)
          (for/list : (Listof (Listof (Listof renderer2d)))
                    ([vec (in-list vec*)]
                     [x-center (in-naturals)])
            (for/list : (Listof (Listof renderer2d))
                      ([rktd (in-vector vec)]
                       [i (in-naturals (*COLOR-OFFSET*))])
              (define S (from-rktd rktd))
              (define t/u* ;; NOTE lists may not have same size
                (for/list : (Listof Real)
                          ([t (in-list (typed-runtimes S))]
                           [u (in-list (untyped-runtimes S))])
                  (/ u t)))
              (define m (mean t/u*))
              (define s (error-bound t/u*))
              (define x* (linear-seq (- x-center config-x-jitter)
                                     (+ x-center config-x-jitter)
                                     (length t/u*)))
              (list
                (lnm-error-bar x-center m s #:color (->pen-color i))
                (lnm-points i ;; no legend for now
                  (for/list : (Listof (List Real Real))
                            ([x (in-list x*)]
                             [t/u (in-list t/u*)])
                    (list x t/u)))))))
        #:x-label "Benchmark"
        #:y-label #f ;"τ/λ ratio"
        #:y-min #f ;0.5
        #:y-max (*Y-MAX*)
        #:x-max (- num-benchmarks 0.5)
        #:legend-anchor (*LEGEND-ANCHOR*)
        #:width (*PLOT-WIDTH*)
        #:height (*PLOT-HEIGHT*)))
    (cast p pict)))

(: plot-indirection-cost (-> Path-String pict))
(define (plot-indirection-cost rktd)
  (define s** (cast (file->value rktd)
                    (Listof (List Symbol (Pairof 'typed (Listof Real))
                                         (Pairof 'no-adapt (Listof Real))))))
  (define num-benchmarks (length s**))
  (define labels
    (for/list : (Listof String)
              ([x (in-list s**)])
      (symbol->string (car x))))
  (parameterize ([plot-x-ticks (labels->ticks labels)]
                 [plot-x-far-ticks no-ticks]
                 [plot-y-ticks (compute-yticks 5 (*Y-NUM-TICKS*) #:units "ms")])
    (define p
      (plot-pict
        (list
          (make-vrule* num-benchmarks)
          (for/list : (Listof (Listof (Listof renderer2d)))
                    ([bm (in-list s**)]
                     [x-center (in-naturals)])
            (define benchmark-name (car bm))
            (define baseline (car (cdr (car (cdr bm))))) ;; -- random data point
            (for/list : (Listof (Listof renderer2d))
                      ([tag+data : (Pairof (U 'typed 'no-adapt) (Listof Real)) (in-list (cdr bm))]
                       [x-pos (linear-seq (- x-center config-x-jitter) (+ x-center config-x-jitter) 2)])
              (define color-idx 4)
              (define color (->pen-color color-idx))
              (define v* (cdr tag+data))
              (list
                ;; -- error bar
                (let* ([m (mean v*)]
                       [s (error-bound v*)])
                  (lnm-error-bar x-pos m s #:color color))
                (lnm-points color-idx
                  (for/list : (Listof (List Real Real))
                            ([v (in-list v*)])
                    (list x-pos v)))))))
        #:x-label "Benchmark"
        #:y-label "Absolute Runtime"
        #:y-min 0
        #:y-max #f
        #:x-min -0.5
        #:x-max (- num-benchmarks 0.5)
        #:legend-anchor (*LEGEND-ANCHOR*)
        #:width (*PLOT-WIDTH*)
        #:height (*PLOT-HEIGHT*)))
      (cast p pict)))

(: plot-karst (-> Path-String pict))
(define (plot-karst csv)
  (define bm** (split-by-group csv))
  (define all-jobid* (parse-jobid* bm**))
  (define JOBS/COL 3)
  (define num-benchmarks (length (car bm**)))
  ;; TODO get configuration number (hm, will be more work to explain)
  (parameterize ([plot-x-ticks (alphabet-ticks num-benchmarks)]
                 [plot-x-far-ticks no-ticks]
                 [plot-y-ticks (compute-yticks 5 (*Y-NUM-TICKS*) #:units "x")])
    (define bm* (car bm**))
    (define n0 0)
    (define my-jobid*
      (let ([tl (drop all-jobid* n0)])
        (drop-right tl (max 0 (- (length tl) JOBS/COL)))))
    (define p
      (plot-pict
        (list
          (make-vrule* num-benchmarks)
          (for/list : (Listof (Listof (Listof renderer2d)))
                    ([bm (in-list bm*)]
                     [x-center (in-range (length bm*))])
            (for/list : (Listof (Listof renderer2d))
                      ([i (in-range (length my-jobid*))]
                       [x-pos (linear-seq (- x-center (- config-x-jitter 0.1)) (+ x-center (- config-x-jitter 0.1)) JOBS/COL)])
              (define color-idx 3)
              (define color (->pen-color color-idx))
              (define v* (jobid->row bm (list-ref my-jobid* i)))
              (define m (mean v*))
              (list
                ;; -- line to mark 1st point
                ;(let ([v-first (/ (car v*) m)])
                ;  (lines
                ;    (list (list (- x-pos 0.2) v-first)
                ;          (list (+ x-pos 0.2) v-first))
                ;    #:color color
                ;    #:width 0.6))
                ;(let* ([m (mean v*)]
                ;       [s (error-bound v*)])
                ;  (lnm-error-bar x-pos 1 (/ s m) #:color color))
                (lnm-points color-idx
                  (for/list : (Listof (List Real Real))
                            ([v (in-list v*)])
                    (list x-pos (/ v m))))))))
        #:x-label "Benchmark"
        #:y-label "Normalized Runtime"
        #:y-min 0
        #:y-max #f
        #:x-min -0.5
        #:x-max (- num-benchmarks 0.5)
        #:legend-anchor (*LEGEND-ANCHOR*)
        #:width (*PLOT-WIDTH*)
        #:height (*PLOT-HEIGHT*)))
      (cast p pict)))

(struct karst (
  [config : String]
  [jobid : String]
  [data : (Listof Real)]
) #:transparent )
(define-type Karst-Row karst)
(define-type KarstBM (Pairof String (Listof Karst-Row)))

(: parse-line (-> String (List String Karst-Row)))
(define (parse-line str)
  (match-define (list-rest benchmark job samples mean stddev color dat*) (string-split str ","))
  (match-define (list name config) (string-split benchmark "+"))
  (list name (karst config job (string*->real* dat*))))

(: string*->real* (-> (Listof String) (Listof Real)))
(define (string*->real* str*)
  (for/list : (Listof Real)
            ([str (in-list str*)])
    (assert (string->number str) real?)))

(: parse-jobid* (-> (Listof (Listof KarstBM)) (Listof String)))
(define (parse-jobid* bm**)
  (define jobid*
    (for*/list : (Listof String)
               ([k (in-list (cdr (ann (caar bm**) KarstBM)))])
      (karst-jobid k)))
  (define jobid-set (list->set jobid*))
  (unless (= (length jobid*) (set-count jobid-set))
    (raise-user-error 'karst "duplicate job ID in ~a" jobid*))
  (for* ([bm* (in-list bm**)]
         [bm : KarstBM (in-list bm*)])
    (unless (set=? jobid-set
                   (list->set (for/list : (Listof String)
                            ([k (in-list (cdr bm))])
                     (karst-jobid k))))
      (raise-user-error 'karst "inconsisten jobids across benchmarks problem in '~a'" bm)))
  jobid*)

;; Read a csv file
;; Parse each line of the csv to a `karst` struct
;; Group rows by benchmark, and again my configuration
;;  (the config thing makes this more confusing, should probably be separate function)
(: split-by-group (-> Path-String (Listof (Listof KarstBM))))
(define (split-by-group csv)
  (define H : (HashTable String (HashTable String (Listof Karst-Row)))
    (with-input-from-file csv
      (lambda ()
        (void (read-line))
        (for/fold : (HashTable String (HashTable String (Listof Karst-Row)))
                  ([acc : (HashTable String (HashTable String (Listof Karst-Row))) (hash)])
                  ([ln (in-lines)])
          (match-define (list nm k) (parse-line ln))
          (define cfg (karst-config k))
          (define cfg-hash (or (hash-ref acc nm #f)
                               (ann (hash) (HashTable String (Listof Karst-Row)))))
          (define row : (Listof Karst-Row)
                      (or (hash-ref cfg-hash cfg #f)
                          '()))
          (hash-set acc nm (hash-set cfg-hash cfg (cons k row)))))))
  (define (extractor (h : (HashTable String (HashTable String (Listof Karst-Row)))) (i : Integer)) : (Listof KarstBM)
    (for/list : (Listof KarstBM)
               ([(nm h2) (in-hash h)])
      (ann (cons nm
            (car (for/list : (Listof (Listof Karst-Row))
                           ([(_c k*) (in-hash h2)]
                            [j (in-naturals)]
                           #:when (= i j))
                   k*))) KarstBM)))
  (list
    (extractor H 0)
    (extractor H 1)))

(: jobid->row (-> KarstBM String (Listof Real)))
(define (jobid->row bm s)
  (or
    (for/or : (U #f (Listof Real))
            ([k (in-list (cdr bm))])
      (and (string=? s (karst-jobid k)) (karst-data k)))
    (error 'jobid->row "jobid ~a not found in ~a" s bm)))

(: plot-srs-sound (-> (Listof Summary) Natural pict))
(define (plot-srs-sound S* sample-size)
  (define x-major-ticks (compute-xticks (*X-TICKS*) (*X-NUM-TICKS*)))
  (define y-major-ticks
    (compute-yticks 100 (*Y-NUM-TICKS*)
      #:units "%"
      #:exact (list 100)))
  (parameterize ([plot-x-ticks (ticks-add? x-major-ticks (*X-MINOR-TICKS*))]
                 [plot-x-transform (if (*LOG-TRANSFORM?*) log-transform id-transform)]
                 [plot-y-ticks (ticks-add? y-major-ticks (*Y-MINOR-TICKS*))]
                 [plot-x-far-ticks no-ticks]
                 [plot-y-far-ticks no-ticks]
                 [plot-tick-size (*TICK-SIZE*)]
                 [plot-font-face (*PLOT-FONT-FACE*)]
                 [plot-font-size (* (*PLOT-FONT-SCALE*) (*PLOT-WIDTH*))])
    (define p
      (plot-pict
        (append
          (if (*X-TICK-LINES?*) (list (x-tick-lines)) '())
          (for/list : (Listof (List renderer2d (Listof (Listof renderer2d))))
                    ([S (in-list S*)]
                     [i (in-list '(1 3))]) ;; 2016-09-22 hardcoded for 6.2/6.4
            (define baseline (untyped-mean S))
            (define srs**
              ;; -- 2016-09-21 : only doing "without replacement", but there's code for adding "with?"
              (for/list : (Listof (Listof (Listof Real)))
                        ([r? (in-list (list #f #;#t))])
                (for/list : (Listof (Listof Real))
                          ([i (in-range (*NUM-SIMPLE-RANDOM-SAMPLES*))])
                (summary-random-sample S sample-size #:replacement? r?))))
            (list
              (function (count-configurations/mean S 0 #:cache-up-to (assert (*MAX-OVERHEAD*) index?) #:percent? #t)
                0 (*MAX-OVERHEAD*)
                #:color i
                #:label #f
                #:samples (*NUM-SAMPLES*)
                #:style (integer->pen-style i)
                #:width (*LNM-WIDTH*))
              (parameterize ([line-alpha 0.6])
                (for/list : (Listof (Listof renderer2d))
                          ([srs* (in-list srs**)])
                  (for/list : (Listof renderer2d)
                            ([srs (in-list srs*)])
                    (lnm-overhead srs
                                  #:base baseline
                                  #:width (assert (- (*LNM-WIDTH*) 0.2) positive?)
                                  #:color (assert i index?))))))))
        #:x-min 1
        #:x-max (*MAX-OVERHEAD*)
        #:y-min 0
        #:y-max 100
        #:x-label #f
        #:y-label #f
        #:title #f ;(format "~a samples" sample-size)
        #:legend-anchor (*LEGEND-ANCHOR*)
        #:width (*PLOT-WIDTH*)
        #:height (*PLOT-HEIGHT*)))
    (cast p pict)))

(: plot-srs-precise (-> (Listof Summary) Natural pict))
(define (plot-srs-precise S* sample-size)
  (unless (= 2 (length S*))
    (raise-user-error 'srs:precise "expected 2 versions to compare"))
  ;; -- plot difference between versions
  ;; -- plot difference between random samples
  (define x-major-ticks (compute-xticks (*X-TICKS*) (*X-NUM-TICKS*)))
  (define y-major-ticks
    (compute-yticks 100 (*Y-NUM-TICKS*)
      #:units "%"
      #:exact (list 100)))
  (parameterize ([plot-x-ticks (ticks-add? x-major-ticks (*X-MINOR-TICKS*))]
                 [plot-x-transform (if (*LOG-TRANSFORM?*) log-transform id-transform)]
                 [plot-y-ticks (list->ticks '(-10 50 100) #:units "%")]
                 ;[plot-y-ticks (linear-ticks #:number (assert (*Y-NUM-TICKS*) positive?))]
                 [plot-x-far-ticks no-ticks]
                 [plot-y-far-ticks no-ticks]
                 [plot-tick-size (*TICK-SIZE*)]
                 [plot-font-face (*PLOT-FONT-FACE*)]
                 [plot-font-size (* (*PLOT-FONT-SCALE*) (*PLOT-WIDTH*))])
    (define p
      (plot-pict
        (list*
          (let* ([get-samples (lambda ([S : Summary])
                                (for/list : (Listof (Listof Real))
                                          ([i (in-range (*NUM-SIMPLE-RANDOM-SAMPLES*))])
                                  (summary-random-sample S sample-size #:replacement? #f)))]
                 [get-props (lambda ([S : Summary])
                              (define base (untyped-mean S))
                              (define s** (get-samples S))
                              (lambda ([r : Real])
                                (for/list : (Listof Real)
                                           ([s* (in-list s**)])
                                   (* 100
                                      (/ (for/sum : Integer
                                                  ([s (in-list s*)])
                                           (if (<= (/ s base) r) 1 0))
                                         sample-size)))))]
                 [f-6.2 (get-props (car S*))]
                 [f-6.4 (get-props (cadr S*))]
                 [samples-color "brown"])
            ;; -- mean
            (list
              (let ([lo-ci (lambda ([r* : (Listof Real)]) (- (mean r*) (error-bound r*)))]
                    [hi-ci (lambda ([r* : (Listof Real)]) (+ (mean r*) (error-bound r*)))])
                (function-interval
                  (lambda ([r : Real])
                    (- (lo-ci (f-6.4 r)) (hi-ci (f-6.2 r))))
                  (lambda ([r : Real])
                    (- (hi-ci (f-6.4 r)) (lo-ci (f-6.2 r))))
                  #:color samples-color
                  #:samples (*NUM-SAMPLES*)
                  #:style 'solid
                  #:line1-color 0
                  #:line2-color 0
                  #:line1-width 1
                  #:line2-width 1
                  #:alpha (*INTERVAL-ALPHA*)
                  #:label #f))
              (function (lambda ([r : Real])
                          (- (mean (f-6.4 r)) (mean (f-6.2 r))))
                #:color samples-color
                #:label #f
                #:samples (*NUM-SAMPLES*)
                #:style 'long-dash
                #:width (*LNM-WIDTH*) )))
          (let* ([real->percent (lambda ([S : Summary]) (count-configurations/mean S 0 #:cache-up-to (assert (*MAX-OVERHEAD*) index?) #:percent? #t))]
                 [f-6.2 (real->percent (car S*))]
                 [f-6.4 (real->percent (cadr S*))])
            (function (lambda ([r : Real])
                        ;; -- difference between percentages
                        (- (f-6.4 r) (f-6.2 r)))
              #:color "DarkViolet"
              #:label #f
              #:samples (*NUM-SAMPLES*)
              #:style 'solid
              #:width (*LNM-WIDTH*) ))
          (hrule 0 #:color 0 #:width 1 #:style 'solid)
          (if (*X-TICK-LINES?*) (list (x-tick-lines)) '()))
        #:x-min 1
        #:x-max (*MAX-OVERHEAD*)
        #:x-label #f
        #:y-label #f
        #:y-min -10
        #:y-max 100
        #:title #f ;(format "~a samples" sample-size)
        #:legend-anchor (*LEGEND-ANCHOR*)
        #:width (*PLOT-WIDTH*)
        #:height (*PLOT-HEIGHT*)))
    (cast p pict)))

(: plot-delta (-> (Listof (Listof Summary)) pict))
(define (plot-delta S**)
  (for ([S* (in-list S**)])
    (unless (= 2 (length S*))
      (raise-user-error 'srs:precise "expected 2 versions to compare")))
  ;; -- plot difference between versions
  (parameterize ([plot-y-ticks (list->ticks '(-50 0 50 100) #:units "%")]
                 [plot-x-ticks (alphabet-ticks (length S**) #:offset 5 #:skip 10)]
                 [plot-x-far-ticks no-ticks]
                 [plot-y-far-ticks no-ticks]
                 [plot-font-face (*PLOT-FONT-FACE*)]
                 [plot-font-size (* (*PLOT-FONT-SCALE*) (*PLOT-WIDTH*))])
    (define (real->percent (S : Summary)) : (-> Real Real)
      (count-configurations/mean S 0 #:cache-up-to (assert (*MAX-OVERHEAD*) index?) #:percent? #t))
    (define p
      (plot-pict
        (cons
          (hrule 0 #:color 0 #:width (line-width) #:style 'short-dash)
          (for/list : (Listof (Listof renderer2d))
                    ([S* (in-list S**)]
                     [i (in-naturals)])
            (define x-min (* i 10))
            (define x-max (+ x-min 10))
            (define f-6.2 (real->percent (car S*)))
            (define f-6.4 (real->percent (cadr S*)))
            (define log-diff
              (- (log (*MAX-OVERHEAD*)) (log 1)))
            (list
              (vrule x-min #:color 0 #:width (* 0.5 (line-width)))
              (function (lambda ([pre-r : Real])
                          (define pct (/ (- pre-r x-min) 10))
                          (define r (exp (* log-diff pct)))
                          (- (f-6.4 r) (f-6.2 r)))
                x-min x-max
                #:color "DarkViolet"
                #:label #f
                #:samples (*NUM-SAMPLES*)
                #:style 'solid
                #:width (*LNM-WIDTH*)))))
        #:x-min 0
        #:x-label #f
        #:y-label #f
        #:y-min -50
        #:y-max 100
        #:title #f ;(format "~a samples" sample-size)
        #:legend-anchor (*LEGEND-ANCHOR*)
        #:width (*PLOT-WIDTH*)
        #:height (*PLOT-HEIGHT*)))
    (cast p pict)))

(: lnm-overhead (->* [(Listof Real) #:base Real #:color Index] [#:width (U #f Positive-Real)] renderer2d))
(define (lnm-overhead r* #:base baseline #:color color #:width [w #f])
  (define num-samples (length r*))
  (function (lambda ([x : Real])
              (define num-good
                (for/sum : Real
                         ([r (in-list r*)])
                  (if (<= (/ r baseline) x) 1 0)))
              (* 100 (/ num-good num-samples)))
    0 (*MAX-OVERHEAD*)
    #:color color
    #:style (integer->pen-style color)
    #:samples (*NUM-SAMPLES*)
    #:width (or w (integer->line-width color))
    #:label #f))

(: lnm-points (->* [Integer (Listof (List Real Real))] [#:label (U #f String)] renderer2d))
(define (lnm-points i data #:label [label #f])
  (points data
          #:color (->pen-color i)
          #:alpha (*POINT-ALPHA*)
          #:sym (integer->symbol i)
          #:size (*POINT-SIZE*)
          #:label (and (*LEGEND?*) label)))

(: lnm-bar (-> (Listof (Listof Real)) BarType pict))
(define (lnm-bar r** btype)
  (define overhead? (not (eq? btype 'ratio)))
  (define y-major-ticks (bar-type->major-ticks btype))
  (define y-minor-ticks (bar-type->minor-ticks btype))
  (define units (bar-type->units btype))
  (define select (bar-type->selector btype))
  (parameterize ([plot-x-axis? #t]
                 [plot-y-axis? #t]
                 [plot-font-face (*PLOT-FONT-FACE*)]
                 [plot-font-size 8]
                 [plot-y-ticks (ticks-add? (list->ticks y-major-ticks #:units units) y-minor-ticks)]
                 [rectangle-alpha 0.9]
                 [plot-x-far-axis? #f]
                 [plot-y-far-axis? #f]
                 [plot-y-transform (if (*LOG-TRANSFORM?*) log-transform id-transform)]
                 [plot-x-far-ticks no-ticks])
    (define num-series (length (car r**)))
    (define all-time-max : (Boxof Real) (box 0))
    ;; -- series   = bars that touch each other
    ;; -- datasets = separate groups of bars
    (cast (plot-pict (append
      (if (eq? btype 'runtime) (list (y-tick-lines)) '())
      (for/list : (Listof renderer2d)
                ([series-num (in-range 1 (+ 1 (length (car r**))))])
        ;; 2016-04-18: brush colors are lighter
        (define color (->pen-color series-num))
        (discrete-histogram
          (for/list : (Listof (List Any Real))
                    ([r* (in-list r**)]
                     [dataset-num (in-naturals 1)])
            (define v (select r* (- series-num 1)))
            (when (> v (unbox all-time-max))
              (set-box! all-time-max v))
            (list (integer->letter dataset-num) v))
          #:x-min series-num
          #:skip (+ 1 num-series)
          #:gap 0.2
          #:add-ticks? #t
          #:style (integer->brush-style series-num)
          #:line-color series-num ;color
          #:line-width 1
          #:color color)))
      #:x-label #f ;(and (*AXIS-LABELS?*) "foo")
      #:y-label #f ;(*AXIS-LABELS?*) ylabel)
      #:y-min (bar-type->y-min btype)
      #:y-max (bar-type->y-max btype)
      #:width (assert (- (*PLOT-WIDTH*) (if overhead? 0 SHIM-FOR-BARCHART-ALIGNMENT)) positive?)
      #:height (*PLOT-HEIGHT*)) pict)))

(define SHIM-FOR-BARCHART-ALIGNMENT 16)

;; Configure via parameters
(: path-plot (-> (U Summary (Listof Summary)) (Listof pict)))
(define (path-plot S*)
  (when (*PDF?*)
    (printf "Warning: path-plot cannot make PDF, making CDF anyway\n"))
  (define S : Summary
    (cond
     [(not (list? S*))
      S*]
     [(null? S*)
      (raise-user-error ERRLOC "Got empty list of Summary objects, cannot make path plot")]
     [else
      (unless (null? (cdr S*))
        (printf "Warning: path-plot ignoring extra arguments ~a\n" (cdr S*)))
      (car S*)]))
  (define L* : (Listof Natural)
    (let ([L*-param (*L*)])
      (cond
       [(integer? L*-param)
        (list L*-param)]
       [(null? L*-param)
        (raise-user-error 'lnm-plot "No L specified, cannot make plot")]
       [(list? (car L*-param))
        (for/list : (Listof Natural)
                 ([x : (List Natural Any) (in-list L*-param)])
          (car x))]
       [else
        L*-param])))
  (define num-paths (get-num-paths S))
  (define next-color (make-palette))
  (define xmax (*MAX-OVERHEAD*))
  (define xticks (compute-xticks (*X-TICKS*) (*X-NUM-TICKS*)))
  (define num-samples (*NUM-SAMPLES*))
  ;; Here's where to generalize for more L
  (for/list : (Listof pict)
            ([L : Natural (in-list L*)])
    (define f (count-paths S L #:cache-up-to (assert xmax index?)))
    (define ymax 0)
    (define pts
      (points (for/list : (Listof (List Real Real))
                        ([n : Real (linear-seq 0 xmax num-samples)])
                (define y (f n))
                (when (> y ymax) (set! ymax y))
                (list n y))
              #:x-min 0 #:x-max xmax
              #:y-min 0
              #:color (next-color)
              #:sym 'dot))
    (define cutoff-proportion (*CUTOFF-PROPORTION*))
    (define cutoff-point
      (match cutoff-proportion
       [(? real? n)  (* n ymax)]
       [_ #f]))
    ;; Make renderers for the lines
    (define cutoff-line
      (and cutoff-point
           (hrule cutoff-point
                  #f xmax
                  #:color (*CUTOFF-COLOR*)
                  #:style (*CUTOFF-STYLE*)
                  #:width (*CUTOFF-WIDTH*))))
    (define yticks
      (case (*Y-STYLE*)
       [(count)
        (compute-yticks (assert ymax index?) (assert ymax index?)
                        #:exact (if cutoff-point
                                  (list cutoff-point ymax)
                                  (list ymax)))]
       [(%)
        (compute-yticks 100 (*Y-NUM-TICKS*)
          #:units "%"
          #:exact (if cutoff-proportion
                      (list (* 100 cutoff-proportion) 100)
                      (list 100)))]
       [else
        (raise-user-error ERRLOC (format "Unexpected value '~a' for *Y-STYLE* parameter" (*Y-STYLE*)))]))
    ;; Set plot parameters ('globally', for all picts)
    (parameterize (
      [plot-x-ticks xticks]
      [plot-y-ticks yticks]
      [plot-x-far-ticks no-ticks]
      [plot-y-far-ticks no-ticks]
      [plot-tick-size (*TICK-SIZE*)]
      [plot-font-face (*PLOT-FONT-FACE*)]
      [plot-font-size (* (*PLOT-FONT-SCALE*) (*PLOT-WIDTH*))])
      (cast
        (plot-pict pts
          #:x-min 1
          #:x-max xmax
          #:y-min 0
          #:y-max ymax
          #:legend-anchor 'top-right
          #:x-label (and (*AXIS-LABELS?*) "Overhead (vs. untyped)")
          #:y-label (and (*AXIS-LABELS?*) "#Paths")
          #:width (*PLOT-WIDTH*)
          #:height (*PLOT-HEIGHT*)
          )
        pict))))

;(define (death-plot S*
;                  #:L L ;; (U Index (Listof Index)), L-values to plot
;                  #:N [N DEFAULT_N]  ;; Index, recommened N limit
;                  #:M [M DEFAULT_M] ;; Index, recommended M limit
;                  #:max-overhead [xmax DEFAULT_XLIMIT] ;; Index, max. x-value
;                  #:num-samples [num-samples DEFAULT_SAMPLES] ;; Index
;                  #:font-face [font-face DEFAULT_FACE]
;                  #:cutoff-proportion [cutoff-proportion DEFAULT_CUTOFF] ;; Flonum, between 0 and 1.
;                  #:plot-width [width (plot-width)] ;; Index
;                  #:plot-height [height (plot-height)]) ;; Index
;  (when (null? S*) (error 'lnm-plot "Cannot make picture for empty list of input"))
;  (define L-list (or (and (list? L) L) (list L)))
;  (define ymax 100)
;  (define cutoff-point (* ymax cutoff-proportion))
;  ;; Make renderers for the lines
;  (define N-line (vertical-line N #:y-max ymax
;                                  #:color 'forestgreen
;                                  #:width THIN))
;  (define M-line (vertical-line M #:y-max ymax
;                                  #:color 'goldenrod
;                                  #:width THIN))
;  (define cutoff-line (horizontal-line cutoff-point #:x-max xmax
;                                                    #:color 'orangered
;                                                    #:style 'short-dash
;                                                    #:width THICK))
;  ;; Get yticks
;  ;; Set plot parameters ('globally', for all picts)
;  (parameterize (
;    [plot-x-ticks (compute-xticks 5)]
;    [plot-y-ticks (compute-yticks ymax 6 #:exact (list cutoff-point ymax))]
;    [plot-x-far-ticks no-ticks]
;    [plot-y-far-ticks no-ticks]
;    [plot-tick-size 4]
;    [plot-font-face font-face]
;    [plot-font-size font-size])
;    ;; Create 1 pict for each value of L
;    (for/list ([L (in-list L-list)])
;      (define F-config
;        (let* ([get-proportion*
;                ;; For each Summary, count proportion of good configs
;                (for/list : (Listof (-> Real Real))
;                          ([S : Summary (in-list S*)])
;                  (define counter (count-configurations S L #:cache-up-to xmax))
;                  (define vars (get-num-configurations S))
;                  (lambda ([n : Real])
;                    (/ (counter n) vars)))]
;               [F
;                ;; Take the mean of all proportions for all configs
;                (lambda ([n : Real])
;                   (define prop*
;                     (for/list : (Listof Real)
;                               ([gp : (-> Real Real) (in-list get-proportion*)])
;                       (gp n)))
;                   (round (* 100 (mean prop*))))])
;          ((if (*HISTOGRAM?*) area-histogram function)
;            F 0 xmax
;            #:samples num-samples
;            #:color 'navy
;            #:width THICK)))
;      (assert
;        (plot-pict (list N-line M-line cutoff-line F-config)
;            #:x-min 1 #:x-max xmax
;            #:y-min 0 #:y-max ymax
;            #:x-label (and labels? "Overhead (vs. untyped)")
;            #:y-label (and labels? "Avg. % Acceptable")
;            #:width (* 3 width) #:height (* 3 height)) pict?))))

;; =============================================================================

(: count-configurations/standard-error (->* [Summary Index] [#:pdf? Boolean #:percent? Boolean #:cache-up-to (U #f Index)] (Listof (-> Real Natural))))
(define (count-configurations/standard-error S L #:cache-up-to [lim #f] #:pdf? [pdf? #f] #:percent? [percent? #f])
  (list
   (count-configurations S L configuration->mean-runtime
    #:cache-up-to lim #:pdf? pdf? #:percent? percent?)
   (count-configurations S L (lambda ([S : Summary] [str : String]) (- (configuration->mean-runtime S str) (configuration->standard-error S str)))
    #:cache-up-to lim #:pdf? pdf? #:percent? percent?)
   (count-configurations S L (lambda ([S : Summary] [str : String]) (+ (configuration->mean-runtime S str) (configuration->standard-error S str)))
    #:cache-up-to lim #:pdf? pdf? #:percent? percent?)))

(: count-configurations/confidence (->* [Summary Index] [#:pdf? Boolean #:percent? Boolean #:cache-up-to (U #f Index)] (Listof (-> Real Natural))))
(define (count-configurations/confidence S L #:cache-up-to [lim #f] #:pdf? [pdf? #f] #:percent? [percent? #f])
  (list
   (count-configurations S L configuration->mean-runtime
    #:cache-up-to lim #:pdf? pdf? #:percent? percent?)
   (count-configurations S L configuration->confidence-lo
    #:cache-up-to lim #:pdf? pdf? #:percent? percent?)
   (count-configurations S L configuration->confidence-hi
    #:cache-up-to lim #:pdf? pdf? #:percent? percent?)))

(: count-configurations/mean (->* [Summary Index] [#:pdf? Boolean #:percent? Boolean #:cache-up-to (U #f Index)] (-> Real Natural)))
(define (count-configurations/mean S L #:cache-up-to [lim #f] #:pdf? [pdf? #f] #:percent? [percent? #f])
  (count-configurations S L configuration->mean-runtime
    #:cache-up-to lim #:pdf? pdf? #:percent? percent?))

;; Return a function (-> Real Index) on argument `N`
;;  that counts the number of configurations
;;  which can reach, in L or fewer steps,
;;  a configuration with overhead no more than `N`
(: count-configurations (->* [Summary Index (-> Summary String Real)] [#:pdf? Boolean #:percent? Boolean #:cache-up-to (U #f Index)] (-> Real Natural)))
(define (count-configurations S L f #:cache-up-to [lim #f] #:pdf? [pdf? #f] #:percent? [percent? #f])
  (define baseline (f S (untyped-configuration S)))
  (define cache (and lim (cache-init S f lim #:L L)))
  (define num-configs (get-num-configurations S))
  (: prev-good (Boxof Natural))
  (define prev-good (box 0)) ;; For computing pdf graphs (instead of cumulative)
  (lambda ([N-raw : Real]) ;; Real, but we assume non-negative
    (: N Nonnegative-Real)
    (define N (if (>= N-raw 0) N-raw (error 'count-configurations)))
    (define good? (make-configuration->good? S (* N baseline) f #:L L))
    (define num-good
      (if (and cache lim (<= N lim))
        ;; Use cache to save some work, only test the configurations
        ;; in the next bucket
        (cache-lookup cache N good?)
        ;; No cache, need to test all configurations
        (sequence-length (predicate->configurations S good?))))
    (cond
     [pdf?
      (begin0 (assert (- num-good (unbox prev-good)) index?) (set-box! prev-good num-good))]
     [percent?
      (exact-round (* 100 (/ num-good num-configs)))]
     [else
      num-good])))

;; Return a function (-> Real Index) on argument `N`
;;  that counts the number of acceptable paths
;; Parameter `L` is ignored for now
(: count-paths (->* [Summary Index] [#:cache-up-to (U #f Index)] (-> Real Natural)))
(define (count-paths S L #:cache-up-to [lim #f])
  (define baseline (untyped-mean S))
  (define path-overheads
    (for/list : (Listof Real)
              ([p* (all-paths S)])
      (/ (path->max-runtime S p*) baseline)))
  ;; TODO use a cache to avoid walking the stream each time
  ;(define cache (and lim (cache-init S lim #:L L)))
  (lambda ([N : Real])
    (for/sum : Natural ([o (in-list path-overheads)]
                       #:when (<= o N)) 1)))

;; Make a predicate checking whether a configuration is good.
;; Good = no more than `L` steps away from a configuration
;;        with average runtime less than `good-threshold`.
(: make-configuration->good? (->* [Summary Real (-> Summary String Real)] [#:L Index] (-> Bitstring Boolean)))
(define (make-configuration->good? S good-threshold f #:L [L 0])
  (lambda ([var : String])
    (for/or ([var2 (cons var (in-reach var L))])
      (<= (f S var2)
         good-threshold))))

;; -----------------------------------------------------------------------------
;; -- cache
(define-type Cache (Vectorof (Listof Bitstring)))

;; Create a cache that saves the configurations between discrete overhead values
(: cache-init (->* [Summary (-> Summary String Real) Index] [#:L Index] Cache))
(define (cache-init S f max-overhead #:L [L 0])
  (define base-overhead (f S (untyped-configuration S)))
  (: unsorted-configurations (Boxof (Sequenceof Bitstring)))
  (define unsorted-configurations (box (all-configurations S)))
  ;; For each integer-overhead-range [0, 1] [1, 2] ... [max-1, max]
  ;; save the configurations within that overhead to a cache entry
  (for/vector : Cache ([i (in-range (add1 max-overhead))])
    (define good? (make-configuration->good? S (* i base-overhead) f #:L L))
    (define-values (good-vars rest)
      (sequence-partition good? (unbox unsorted-configurations)))
    (set-box! unsorted-configurations rest)
    (sequence->list good-vars)))

;; Count the number of configurations with running time less than `overhead`.
;; Use `test-fun` to manually check configurations we aren't sure about
(: cache-lookup (-> Cache Nonnegative-Real (-> Bitstring Boolean) Natural))
(define (cache-lookup $$$ overhead test-fun)
  (: lo-overhead Natural)
  (define lo-overhead (exact-floor overhead))
  (: hi-overhead Natural)
  (define hi-overhead (exact-ceiling overhead))
  (define num-known
    (for/sum : Natural ([i (in-range (add1 lo-overhead))])
      (length (vector-ref $$$ i))))
  (if (= hi-overhead lo-overhead)
      ;; Short circuit, because original overhead was an integer
      num-known
      ;; Else test all the configurations in the "next" bucket
      (+ num-known
         (for/sum : Natural ([var (in-list (vector-ref $$$ hi-overhead))]
                   #:when (test-fun var)) 1))))

(: sequence-partition (-> (-> Bitstring Boolean) (Sequenceof Bitstring) (Values (Sequenceof Bitstring) (Sequenceof Bitstring))))
(define (sequence-partition f sequence)
  (define not-f (lambda ([x : Bitstring]) (not (f x))))
  (values (sequence-filter f sequence)
          (sequence-filter not-f sequence)))

;; -----------------------------------------------------------------------------
;; --- plotting utils

;; Like plot's `function`, but does not connect points with a line
;; Assumes parameter *ERROR-BAR?* is #t
(: discrete-function (-> (Listof (-> Real Natural)) Real Real #:color Natural
                                                  #:label (U #f String)
                                                  #:samples Natural
                                                  #:style Plot-Pen-Style
                                                  #:sym Point-Sym
                                                  #:width Nonnegative-Real
                                                  (Listof renderer2d)))
(define (discrete-function f* lo hi #:color c
                                    #:label lbl
                                    #:samples num-samples
                                    #:style sty
                                    #:sym sym
                                    #:width w)
  (define point**
    (for/list : (Listof (Listof Real))
              ([x (in-list (linear-seq lo hi num-samples))])
      (cons x
        (for/list : (Listof Real)
                  ([f (in-list f*)])
          (f x)))))
  (define pts
    (let ([xy*
           (for/list : (Listof (List Real Real))
                     ([x* (in-list point**)])
             (list (car x*) (cadr x*)))])
      (if (*DISCRETE?*)
        (points xy*
         #:color c
         #:label lbl
         #:sym sym
         #:size (* 3 w))
        (lines xy*
         #:color c
         #:label lbl
         #:style sty
         #:width w))))
  (list
    pts
    (error-bars
      (for/list : (Listof (Listof Real))
                ([x* (in-list point**)])
        (define x (car x*))
        (define lo (caddr x*))
        (define hi (cadddr x*))
        ;(unless (>= lo hi)
        ;  (raise-user-error 'lnm-plot "Bad error bounds [~a , ~a]. Should have lower count on the right (i.e. fewer acceptable configs using the upper-bound mean)." lo hi))
        (define diff (- lo hi))
        (list x (+ hi (/ diff 2)) diff))
      #:alpha 0.5
      #:color c
      #:line-width (*ERROR-BAR-LINE-WIDTH*)
      #:width (*ERROR-BAR-WIDTH*))))

(: exponential-seq (-> Real Real (Listof Real)))
(define (exponential-seq lo hi)
  (for*/list : (Listof Real)
             ([e (in-range lo hi)])
    (expt 10 e)))

;; Compute `num-ticks` evenly-spaced y ticks between 0 and `max-y`.
;; Round all numbers down a little, except for numbers in the optional
;;  list `exact`.
(: compute-yticks (->* [Natural Natural] [#:units (U #f String) #:exact (U Real (Listof Real))] ticks))
(define (compute-yticks max-y num-ticks #:exact [exact '()] #:units [units #f])
  (define unit-str (or units ""))
  (define exact-list (or (and (list? exact) exact) (list exact)))
  (define round-y (if (< max-y 1000) ;;TODO
                      round
                      (lambda ([n : Real]) (* 100 (exact-floor (/ n 100))))))
  (ticks (lambda ([ax-min : Real] [ax-max : Real])
           (for/list : (Listof pre-tick) ([y (in-list (linear-seq ax-min ax-max num-ticks #:end? #t))])
             (define rounded (round-y y))
             (define ex (findf (lambda ([n : Real]) (= rounded (round-y n)))
                               exact-list))
             (pre-tick (or (and ex (round ex))
                           rounded)
                       #t)))
         (lambda ([ax-min : Real] [ax-max : Real] [pre-ticks : (Listof pre-tick)])
           (for/list : (Listof String) ([pt (in-list pre-ticks)])
             (define v (pre-tick-value pt))
             (define str (number->string v))
             (if (= v ax-max)
               (string-append str unit-str)
               str)))))

(: make-vrule* (-> Natural (Listof renderer2d)))
(define (make-vrule* count)
  (for/list : (Listof renderer2d)
            ([i (in-range (+ 1 count))])
    (vrule (- i 0.5)
           #:width 0.6
           #:color 0)))

(: alphabet-ticks (->* [Natural] [#:offset Nonnegative-Real #:skip Nonnegative-Real] ticks))
(define (alphabet-ticks n #:offset [offset 0] #:skip [skip 1])
  (labels->ticks #:offset offset #:skip skip
    (for/list : (Listof String)
              ([i (in-range 1 (+ 1 n))])
      (string (integer->letter i)))))

(: labels->ticks (->* [(Listof String)] [#:offset Nonnegative-Real #:skip Nonnegative-Real] ticks))
(define (labels->ticks str* #:offset [offset 0] #:skip [skip 1])
  (ticks (lambda ([ax-min : Real] [ax-max : Real])
           (for/list : (Listof pre-tick)
                     ([i (in-range (length str*))])
             (pre-tick (+ offset (* i skip)) #t)))
         (lambda ([ax-min : Real] [ax-max : Real] [pre-ticks : (Listof pre-tick)])
           (for/list : (Listof String)
                     ([pt (in-list pre-ticks)]
                      [str (in-list str*)])
             str))))

(: list->ticks (->* [(Listof Real)] [#:units (U #f String)] ticks))
(define (list->ticks r* #:units [units-arg #f])
  (define units (or units-arg ""))
  (define max-r (apply max r*))
  (ticks (lambda ([ax-min : Real] [ax-max : Real])
           (for/list : (Listof pre-tick)
                     ([r (in-list r*)])
             (pre-tick r #t)))
         (lambda ([ax-min : Real] [ax-max : Real] [pre-ticks : (Listof pre-tick)])
           (for/list : (Listof String) ([pt (in-list pre-ticks)])
             (define v (pre-tick-value pt))
             (define str (~r v #:precision 2))
             (if (= v max-r)
               (string-append str units)
               str)))))

(: compute-xticks (-> (U #f (Listof Real)) Natural ticks))
(define (compute-xticks exact-x-ticks num-ticks)
  (define tolerance 1/10)
  (define round? (if exact-x-ticks #f #t))
  (define unit-str "x")
  (ticks (lambda ([ax-min : Real] [ax-max : Real])
           (for/list : (Listof pre-tick)
                     ([i (in-list (or exact-x-ticks (linear-seq 1 ax-max num-ticks)))])
             (pre-tick (rationalize i tolerance) #t)))
         (lambda ([ax-min : Real] [ax-max : Real] [pre-ticks : (Listof pre-tick)])
           (for/list : (Listof String) ([pt (in-list pre-ticks)])
             (define v (pre-tick-value pt))
             (define str (format "~a"
               (cond [round?        (round v)]
                     [(integer? v)          v]
                     [else (exact->inexact v)])))
             (if (= v ax-max)
               (string-append str "x")
               str)))))

(: ticks-add? (-> ticks (U #f (Listof Real)) ticks))
(define (ticks-add? ts xs)
  (if xs (ticks-add ts xs #f) ts))

(: unknown-bar-type (All (A) (-> Any A)))
(define (unknown-bar-type bt)
  (raise-user-error 'lnm "Unknown bar type '~a'" bt))

(: unknown-y-style (All (A) (-> A)))
(define (unknown-y-style)
  (raise-user-error 'lnm "Unknown y-style type '~a'" (*Y-STYLE*)))

(: bar-type->selector (-> BarType (-> (Listof Real) Integer Real)))
(define (bar-type->selector bt)
  (cond
   [(eq? bt 'runtime)
    (cond
     [(eq? (*Y-STYLE*) '%)
      (lambda ([r* : (Listof Real)] [i : Integer])
        (let ([a (car r*)]
              [b (list-ref r* i)])
          (/ (- b a) a)))]
     [(eq? (*Y-STYLE*) 'X)
      (lambda ([r* : (Listof Real)] [i : Integer])
        (/ (list-ref r* i) (car r*)))]
     [else
      (unknown-y-style)])]
   [(eq? bt 'runtime)
    list-ref]
   [else
    (unknown-bar-type bt)]))

(: bar-type->y-min (-> BarType Real))
(define (bar-type->y-min bt)
  (cond
   [(or (eq? bt 'ratio)
        (eq? bt 'runtime))
    (if (*LOG-TRANSFORM?*) 0.1 0)]
   [(eq? bt 'overhead)
    1]
   [else
    (unknown-bar-type bt)]))

(: bar-type->y-max (-> BarType (U #f Real)))
(define (bar-type->y-max bt)
  (cond
   [(or (eq? bt 'ratio)
        (and (eq? (*Y-STYLE*) '%)
             (eq? bt 'runtime)))
    1]
   [(and (eq? bt 'runtime)
         (eq? (*Y-STYLE*) 'X))
    #f]
   [(or (eq? bt 'overhead)
        (eq? bt 'runtime))
    (* 13 (expt 10 3))]
   [else
    (unknown-bar-type bt)]))

(: bar-type->units (-> BarType String))
(define (bar-type->units bt)
  (cond
   [(eq? bt 'ratio)
    ""]
   [(eq? bt 'overhead)
    "x"]
   [(eq? bt 'runtime)
    (case (*Y-STYLE*)
     [(%) "%"]
     [(X) "x"]
     [(count) "ms"]
     [else (unknown-y-style)])]
   [else
    (unknown-bar-type bt)]))

(: bar-type->major-ticks (-> BarType (Listof Real)))
(define (bar-type->major-ticks bt)
  (cond
   [(eq? bt 'ratio)
    (if (*LOG-TRANSFORM?*)
      (exponential-seq -1 2)
      (range 10))]
   [(and (eq? bt 'runtime)
         (eq? (*Y-STYLE*) '%))
    (range 0 2 1/10)]
   [(eq? bt 'runtime)
    (range 1 11)]
   [(eq? bt 'overhead)
    (exponential-seq 0 5)]
   [else
    (unknown-bar-type bt)]))

(: bar-type->minor-ticks (-> BarType (Listof Real)))
(define (bar-type->minor-ticks bt)
  (cond
   [(or (eq? bt 'ratio)
        (and (eq? (*Y-STYLE*) 'X)
             (eq? bt 'runtime)))
    (if (*LOG-TRANSFORM?*)
      '(1/5 2/5 3/5 4/5 2 4 6 8)
      (range 0 2 1/10))]
   [(and (eq? (*Y-STYLE*) '%)
         (eq? bt 'runtime))
    '()]
   [(or (eq? bt 'overhead)
        (eq? bt 'runtime))
    '()]
   [else
    (unknown-bar-type bt)]))

;; -----------------------------------------------------------------------------
;; --- Very Miscellaneous

(: make-palette (->* [] [Natural] (-> Index)))
(define (make-palette [num-colors #f])
  (let ([c : (Boxof Natural) (box 0)]
        [incr : (-> Natural Natural) (if num-colors
                                       (lambda ([n : Natural])
                                         (add1 (modulo n num-colors)))
                                       add1)])
    (lambda ()
      (begin
        (set-box! c (incr (unbox c)))
        (assert (unbox c) index?)))))

(: make-shapegen (-> (-> Point-Sym)))
(define (make-shapegen)
  (let ([c : (Boxof Natural) (box 0)])
    (lambda ()
      (begin
        ;; Maybe want to cycle at some point, or throw helpful error if too big
        (set-box! c (modulo (+ 1 (unbox c)) 6))
        (case (unbox c)
         [(0) 'diamond]
         [(1) 'fullsquare]
         [(2) 'fulltriangle]
         [(3) 'fullcircle]
         [(4) 'fulltriangleup]
         [else 'full6star])))))

(: integer->pen-style (-> Integer (U 'dot 'short-dash 'solid)))
(define (integer->pen-style i)
  (case i
   [(1) 'dot]
   [(2) 'short-dash]
   [else 'solid]))

(: integer->symbol (-> Integer Point-Sym))
(define (integer->symbol i)
  (case i
   [(1) 'fulltriangledown]
   [(2) 'fullcircle]
   [(3) 'fullsquare]
   [(4) 'plus]
   [(5) 'plus]
   [(6) 'plus]
   [else '5star]
 ))

(: integer->brush-style (-> Integer (U 'bdiagonal-hatch 'crossdiag-hatch 'solid)))
(define (integer->brush-style i)
  (case i
   [(1) 'bdiagonal-hatch]
   [(2) 'crossdiag-hatch]
   [else 'solid]))

(: integer->line-width (-> Integer Nonnegative-Real))
(define (integer->line-width i)
  (cast (+ (* 1/2 i) (*LNM-WIDTH*)) Nonnegative-Real))

;; =============================================================================

(module+ test
  (require typed/rackunit)

  (let* ([csv "./test/sample-karst.csv"]
         [bm** (split-by-group csv)]
         [jobid* (parse-jobid* bm**)]
         [r (jobid->row (caar bm**) "1704881.m2")])
    ;; -- 2 groups, 2 benchmarks
    (check-equal? (length bm**) 2)
    (check-equal? (length (car bm**)) (length (cadr bm**)))
    (check-equal? (length (car bm**)) 2)
    ;; --
    (check-equal? (sort jobid* string<?) '("1704875.m2" "1704881.m2"))
    (check-equal? r '(145 147 146 146 146 145 146 146 146 146 147 146 147 147 145 146 145 145 145 145))
    (void))

  (check-exn exn:fail?
    (lambda () (parse-line "foobar")))

  (let ([k (parse-line "dungeon+11101,1704881.m2,20,8.85,0.36,0,9,9,9,9,9,9,9,9,9,9,8,9,9,8,9,9,9,8,9,9")])
    (check-equal? k (list "dungeon" (karst "11101" "1704881.m2" '(9 9 9 9 9 9 9 9 9 9 8 9 9 8 9 9 9 8 9 9)))))

;  ;; Create the graph for a 'large' file
;  (define DATA "../data/")
;  (define synth (string-append DATA "synth-2015-07-02T01:47:43.rktd"))
;  (define gregor (string-append DATA "gregor-2015-07-02.rktd"))
;  (define quad (string-append DATA "quad-placeholder.rktd"))
;
;  (: make-graph (-> String Void))
;  (define (make-graph fname)
;    (define summary (from-rktd fname))
;    (lnm-plot summary #:L '(0 1 2))
;    (void))
;
;  ;(time (make-graph synth)) ;;3,000ms
;  (time (make-graph gregor)) ;;
)
