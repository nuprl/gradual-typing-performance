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

  lnm-plot
  ;; Create an L-NM plot based on the given parameters
  ;; Returns a list of plots:
  ;; - 1 for each given value of L (unless `#:single-plot?` is `#t`
  ;; - each plot has 1 line for each given Summary

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
                    #:font-size Positive-Integer
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
                  #:font-size [font-size (*PLOT-FONT-SIZE*)]
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
  (define xticks (compute-xticks (assert (*X-NUM-TICKS*) index?)))
  (define yticks
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
    [plot-x-ticks xticks]
    [plot-x-transform (if (*LOG-TRANSFORM?*) log-transform id-transform)]
    [plot-y-ticks yticks]
    [plot-x-far-ticks no-ticks]
    [plot-y-far-ticks no-ticks]
    [plot-tick-size (*TICK-SIZE*)]
    [plot-font-face (*PLOT-FONT-FACE*)]
    [plot-font-size (*PLOT-FONT-SIZE*)])
    (define F-config**
      (let ([next-color (make-palette)])
        (for/list : (Listof (Listof renderer2d))
                  ([L+style (in-list L*)])
          (define L (car L+style))
          (define st (cadr L+style))
          (for/list : (Listof renderer2d)
                  ([S (in-list ((inst sort Summary String) S* string<? #:key summary->version))])
            (define f (count-configurations S (assert L index?)
                        #:cache-up-to (assert xmax index?)
                        #:percent? (eq? (*Y-STYLE*) '%)
                        #:pdf? pdf?))
            (define lbl (and (*LINE-LABELS?*)
                             (format "~a (L=~a)" (summary->version S) L)))
            (define c (next-color))
            (define w (*LNM-WIDTH*))
            (if (*HISTOGRAM?*)
              (area-histogram f (linear-seq 0 xmax num-samples)
                #:x-min 0
                #:x-max xmax
                #:samples num-samples
                #:label lbl
                #:line-style st
                #:line-color c
                #:line-width w)
              (function f 0 xmax
                #:label lbl
                #:samples num-samples
                #:style st
                #:color c
                #:width w))
              ))))
    (: make-plot (-> (Listof renderer2d) pict))
    (define (make-plot LNM)
      (cast ;; dammit, Neil re-defined 'Pict'
       (plot-pict (append LNM elem*)
        #:x-min 1
        #:x-max xmax
        #:y-min 0
        #:y-max (if pdf? #f (if (eq? '% (*Y-STYLE*)) 100 ymax))
        #:x-label (and (*AXIS-LABELS?*) "Overhead (vs. untyped)")
        #:y-label (and (*AXIS-LABELS?*) y-label)
        #:title (and (*TITLE?*) (get-project-name (car S*)))
        #:legend-anchor 'bottom-right
        #:width width
        #:height height) pict))
    (if single-plot?
      (list (make-plot (apply append F-config**)))
      (for/list : (Listof pict)
                ([F-config* (in-list F-config**)])
        (make-plot F-config*)))))

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
  (define xticks (compute-xticks (assert (*X-NUM-TICKS*) index?)))
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
      [plot-font-size (*PLOT-FONT-SIZE*)])
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
          #:height (*PLOT-HEIGHT*))
        pict))))

;(define (death-plot S*
;                  #:L L ;; (U Index (Listof Index)), L-values to plot
;                  #:N [N DEFAULT_N]  ;; Index, recommened N limit
;                  #:M [M DEFAULT_M] ;; Index, recommended M limit
;                  #:max-overhead [xmax DEFAULT_XLIMIT] ;; Index, max. x-value
;                  #:num-samples [num-samples DEFAULT_SAMPLES] ;; Index
;                  #:font-face [font-face DEFAULT_FACE]
;                  #:font-size [font-size DEFAULT_SIZE]
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

;; Return a function (-> Real Index) on argument `N`
;;  that counts the number of configurations
;;  which can reach, in L or fewer steps,
;;  a configuration with overhead no more than `N`
(: count-configurations (->* [Summary Index] [#:pdf? Boolean #:percent? Boolean #:cache-up-to (U #f Index)] (-> Real Natural)))
(define (count-configurations sm L #:cache-up-to [lim #f] #:pdf? [pdf? #f] #:percent? [percent? #f])
  (define baseline (untyped-mean sm))
  (define cache (and lim (cache-init sm lim #:L L)))
  (define num-configs (get-num-configurations sm))
  (: prev-good (Boxof Natural))
  (define prev-good (box 0)) ;; For computing pdf graphs (instead of cumulative)
  (lambda ([N-raw : Real]) ;; Real, but we assume non-negative
    (: N Nonnegative-Real)
    (define N (if (>= N-raw 0) N-raw (error 'count-configurations)))
    (define good? (make-configuration->good? sm (* N baseline) #:L L))
    (define num-good
      (if (and cache lim (<= N lim))
        ;; Use cache to save some work, only test the configurations
        ;; in the next bucket
        (cache-lookup cache N good?)
        ;; No cache, need to test all configurations
        (sequence-length (predicate->configurations sm good?))))
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
(: make-configuration->good? (->* [Summary Real] [#:L Index] (-> Bitstring Boolean)))
(define (make-configuration->good? summary good-threshold #:L [L 0])
  (lambda ([var : String])
    (for/or ([var2 (cons var (in-reach var L))])
      (<= (configuration->mean-runtime summary var2)
         good-threshold))))

;; -----------------------------------------------------------------------------
;; -- cache
(define-type Cache (Vectorof (Listof Bitstring)))

;; Create a cache that saves the configurations between discrete overhead values
(: cache-init (->* [Summary Index] [#:L Index] Cache))
(define (cache-init summary max-overhead #:L [L 0])
  (define base-overhead (untyped-mean summary))
  (: unsorted-configurations (Boxof (Sequenceof Bitstring)))
  (define unsorted-configurations (box (all-configurations summary)))
  ;; For each integer-overhead-range [0, 1] [1, 2] ... [max-1, max]
  ;; save the configurations within that overhead to a cache entry
  (for/vector : Cache ([i (in-range (add1 max-overhead))])
    (define good? (make-configuration->good? summary (* i base-overhead) #:L L))
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

;; Compute `num-ticks` evenly-spaced y ticks between 0 and `max-y`.
;; Round all numbers down a little, except for numbers in the optional
;;  list `exact`.
(: compute-yticks (->* [Index Index] [#:exact (U Real (Listof Real))] ticks))
(define (compute-yticks max-y num-ticks #:exact [exact '()])
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
                 (for/list ([pt (in-list pre-ticks)])
                   (number->string (pre-tick-value pt))))))

(: compute-xticks (-> Index ticks))
(define (compute-xticks num-ticks)
  (ticks (lambda ([ax-min : Real] [ax-max : Real])
           (for/list : (Listof pre-tick) ([i (in-list (linear-seq 1 ax-max num-ticks))])
             (pre-tick (round i) #t)))
         (lambda ([ax-min : Real] [ax-max : Real] [pre-ticks : (Listof pre-tick)])
           (for/list : (Listof Bitstring) ([pt (in-list pre-ticks)])
             (format "~ax" (pre-tick-value pt))))))

(: make-palette (-> (-> Index)))
(define (make-palette)
  (let ([c : (Boxof Natural) (box 0)])
    (lambda ()
      (begin
        ;; Maybe want to cycle at some point, or throw helpful error if too big
        (set-box! c (+ 1 (unbox c)))
        (assert (unbox c) index?)))))

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
