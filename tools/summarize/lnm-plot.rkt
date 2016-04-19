#lang typed/racket/base

;; Create L-N/M plots for .rktd files
;;
;; These plot overhead factors against the number of "acceptable"
;;  configurations relative to the overhead,
;;  subject to at most L additional conversion steps

(provide

  ;; TODO
  ;; load a summary to an interactive plot
  ;; summary = vectorof listof unixtime

  integer->pen-style
  integer->line-width

  ;; ---

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
  racket/sequence
  (for-syntax racket/base syntax/parse)
  plot/typed/no-gui
  plot/typed/utils
  typed/pict
  (only-in racket/list range)
  (only-in racket/math exact-floor exact-ceiling exact-round)
  (only-in math/statistics mean)
  (only-in racket/format ~r)
  gtp-summarize/bitstring
  gtp-summarize/lnm-parameters
  gtp-summarize/stats-helpers
  gtp-summarize/path-util
  gtp-summarize/summary
)
;(define-type Pict pict)

(define ERRLOC 'lnm-plot)

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
    (or
     (for/fold : (Option Index)
               ([prev : (Option Index) #f])
               ([S (in-list S*)]
                [i (in-naturals)])
       (define nv (get-num-configurations S))
       (if (and prev (not (= prev nv)))
         (let ([p1 (get-project-name (list-ref S* (assert (- i 1) index?)))]
               [p2 (get-project-name S)])
           (raise-user-error 'lnm (format "datasets for '~a' and '~a' have ~a and ~a modules, cannot plot on same graph" p1 p2 nv prev)))
         (assert nv index?)))
     (raise-user-error 'lnm "got 0 datasets to summarize")))
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
                               (summary->version S)
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

(: lnm-bar (-> (Listof (Listof Real)) (U 'overhead 'ratio) pict))
(define (lnm-bar r** type)
  (define yticks
    (let-values (((lo hi)
                  (if (eq? type 'overhead)
                    (values 0 6)
                    (values -1 2))))
      (for*/list : (Listof Real)
                 ([mag (in-range lo hi)]
                  [val (in-range 10 11 5)])
        (* (expt 10 mag) val))))
  (define units (if (eq? type 'overhead) "x" ""))
  (parameterize ([plot-x-axis? #t]
                 [plot-y-axis? #t]
                 [plot-font-face (*PLOT-FONT-FACE*)]
                 [plot-font-size 8]
                 [plot-y-ticks (list->ticks yticks #:units units)]
                 [rectangle-alpha 0.9]
                 [plot-x-far-axis? #f]
                 [plot-y-far-axis? #f]
                 [plot-y-transform (if (*LOG-TRANSFORM?*) log-transform id-transform)]
                 [plot-x-far-ticks no-ticks])
    (define num-series (length (car r**)))
    (define all-time-max : (Boxof Real) (box 0))
    (cast (plot-pict
      (for/list : (Listof renderer2d)
                ([series-num (in-range 1 (+ 1 (length (car r**))))])
        ;; 2016-04-18: brush colors are lighter
        (define color (->pen-color series-num))
        (discrete-histogram
          (for/list : (Listof (List Any Real))
                    ([r* (in-list r**)]
                     [dataset-num (in-naturals 1)])
            (define v (list-ref r* (- series-num 1)))
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
          #:color color))
      #:x-label #f ;(and (*AXIS-LABELS?*) "foo")
      #:y-label #f ;(*AXIS-LABELS?*) ylabel)
      #:y-min (if (eq? type 'overhead) 1 0.1)
      #:y-max (* 13 (expt 10 (if (eq? type 'overhead) 3 0)))
      #:width (*PLOT-WIDTH*)
      #:height (*PLOT-HEIGHT*)) pict)))

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
               (string-append str "%")
               str)))))

(: list->ticks (->* [(Listof Real)] [#:units (U #f String)] ticks))
(define (list->ticks r* #:units [units-arg #f])
  (define units (or units-arg ""))
  (define max-r (apply max r*))
  (ticks (lambda ([ax-min : Real] [ax-max : Real])
           (for/list : (Listof pre-tick)
                     ([r (in-list r*)])
             (pre-tick r #t)))
         (lambda ([ax-min : Real] [ax-max : Real] [pre-ticks : (Listof pre-tick)])
           (define hi (min max-r ax-max))
           (for/list : (Listof String) ([pt (in-list pre-ticks)])
             (define v (pre-tick-value pt))
             (define str (format "~a" v))
             (if (= v hi)
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

(: integer->brush-style (-> Integer (U 'bdiagonal-hatch 'crossdiag-hatch 'solid)))
(define (integer->brush-style i)
  (case i
   [(1) 'bdiagonal-hatch]
   [(2) 'crossdiag-hatch]
   [else 'solid]))

(: integer->line-width (-> Integer Nonnegative-Real))
(define (integer->line-width i)
  (cast (+ i (*LNM-WIDTH*)) Nonnegative-Real))

;; =============================================================================

;(module+ test
;  (require typed/rackunit)
;
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
;)
