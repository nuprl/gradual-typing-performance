#lang typed/racket/base

;; Create L-N/M plots for .rktd files
;;
;; At a high level:
;; Input:
;; - Raw experimental data (.rktd)
;;   (Optionally a list)
;; Output:
;; - Pict showing an L-N/M plot for the data
;;   (Or, a list of such plots)

(provide
  lnm-plot
  ;; Create an L-NM plot based on the given parameters (see function for params)
  ;; Builds one plot for each given value of L.

  path-plot
  ;; Plots the number of acceptable paths.
  ;; Acceptable paths have no overhead greater than the cutoff along any point

  death-plot
  ;; Plot a bunch of summaries to the same grid.
  ;; - y-axis is the proportion of good variations
  ;; - aggregation is plain old (arithmetic) mean
)

;; -----------------------------------------------------------------------------

(require
  plot/typed/pict
  (only-in racket/math exact-floor)
  (only-in plot/utils linear-seq)
  (only-in racket/math exact-floor exact-ceiling exact-round)
  (only-in math/statistics mean)
  (only-in racket/format ~r)
  "bitstring.rkt"
  "summary.rkt"
  "pict-types.rkt"
  "stream-types.rkt"
)

;; =============================================================================
;; --- constants

(define THIN (* 0.8 (line-width)))
(define THICK (* 1.25 (line-width)))

(define DEFAULT_SAMPLES 100)
(define DEFAULT_FACE "bold")
(define DEFAULT_SIZE 20)
(define DEFAULT_XMAX 20)

;; -----------------------------------------------------------------------------
;; --- plotting

(: lnm-plot (->* [Summary #:L (U Natural (Listof Natural) (Listof (List Natural Plot-Pen-Style)))]
                 [#:summary (Option Summary)
		  #:N (U Natural #f) #:M (U Natural #f)
                  #:max-overhead Index
                  #:num-samples Positive-Integer
                  #:font-face String
                  #:font-size Positive-Integer
                  #:labels? Boolean
                  #:pdf? Boolean
                  #:split-plot? Boolean
                  #:cutoff-proportion (U Real #f)
                  #:plot-width Positive-Integer
                  #:plot-height Positive-Integer
                  ]
                  (Listof Pict)))

(define (lnm-plot summary
                  #:L L
		  #:summary [S2 #f]
                  #:N [N #f]  ;; Index, recommened N limit
                  #:M [M #f] ;; Index, recommended M limit
                  #:max-overhead [xmax DEFAULT_XMAX] ;; Index, max. x-value
                  #:num-samples [num-samples DEFAULT_SAMPLES] ;; Index
                  #:font-face [font-face DEFAULT_FACE]
                  #:font-size [font-size DEFAULT_SIZE]
                  #:pdf? [pdf? #f]
                  #:split-plot? [split-plot? #f]
                  #:labels? [labels? #t]
                  #:cutoff-proportion [cutoff-proportion #f]
                  #:plot-width [width (* 2 (plot-width))] ;; Index
                  #:plot-height [height (* 2 (plot-height))]) ;; Index
  (define L-list
    (cond
     [(not (list? L))
      (list (list L (line-style)))]
     [(andmap pair? L)
      L]
     [else
       (for/list : (Listof (List Natural Plot-Pen-Style))
                 ([l (in-list L)])
         (list l (line-style)))]))
  (define num-vars (get-num-configurations summary))
  (define cutoff-point (and cutoff-proportion (* cutoff-proportion num-vars)))
  ;; Make renderers for the lines
  (define N-line (and N (vertical-line N #:y-max 100
                                         #:color 'forestgreen
                                         #:width THIN)))
  (define M-line (and M (vertical-line M #:y-max num-vars
                                         #:color 'goldenrod
                                         #:width THIN)))
  (define cutoff-line (and cutoff-point (horizontal-line cutoff-point #:x-max xmax
                                                         #:color 'orangered
                                                         #:style 'short-dash
                                                         #:width THICK)))
  ;; Get yticks
  ;; Set plot parameters ('globally', for all picts)
  (parameterize (
    [plot-x-ticks (compute-xticks 5)]
    [plot-y-ticks (if pdf?
                    (plot-y-ticks)
                    (compute-yticks 100 6))]
    [plot-x-far-ticks no-ticks]
    [plot-y-far-ticks no-ticks]
    [plot-tick-size 4]
    [plot-font-face font-face]
    [plot-font-size font-size])
    (define F-config*
      (for/list : (Listof renderer2d)
                ([L+style (in-list L-list)])
        (function (count-configurations summary (car L+style) #:cache-up-to xmax #:pdf? pdf?) 0 xmax
                          #:samples num-samples
                          #:style (cadr L+style)
                          #:color 'navy
                          #:width THICK)))
    (define F-config*2
      (if S2
          (for/list : (Listof renderer2d)
                    ([L+style (in-list L-list)])
            (function (count-configurations S2 (car L+style) #:cache-up-to xmax #:pdf? pdf?) 0 xmax
                              #:samples num-samples
                              #:style (cadr L+style)
                              #:color 'orangered
                              #:width THICK))
	  null))
    (define elem* (append F-config*2
			  (for/list : (Listof renderer2d)
                            ([x (list N-line M-line cutoff-line)] #:when x) x)))
    (if (not split-plot?)
        (let ([res
               (plot-pict (append F-config* elem*)
                           #:x-min 1
                           #:x-max xmax
                           #:y-min 0
                           #:y-max (if pdf? #f 100)
                           #:x-label (and labels? "Overhead (vs. untyped)")
                           #:y-label (and labels? "Percentage deliverable")
                           #:width width
                           #:height height)])
          (if res
            (cast (list res) (Listof Pict))
            (error 'pictfail)))
      (for/list ([F-config (in-list F-config*)])
        (define res (plot-pict (cons F-config elem*)
                               #:x-min 1
                               #:x-max xmax
                               #:y-min 0
                               #:y-max (if pdf? #f num-vars)
                               #:x-label (and labels? "Overhead (vs. untyped)")
                               #:y-label (and labels? "Percentage deliverable")
                               #:width width
                               #:height height))
        (if (pict? res) res (error 'notapict))))))

(: death-plot (->* [(Listof Summary) #:L (U Index (Listof Index))]
                   [#:N Index #:M Index #:max-overhead Index
                    #:num-samples Positive-Integer
                    #:font-face String
                    #:font-size Positive-Integer
                    #:labels? Boolean
                    #:cutoff-proportion Real
                    #:plot-width Positive-Integer
                    #:plot-height Positive-Integer
                    ]
                    (Listof Pict)))
(define (death-plot S*
                  #:L L ;; (U Index (Listof Index)), L-values to plot
                  #:N [N #f]  ;; Index, recommened N limit
                  #:M [M #f] ;; Index, recommended M limit
                  #:max-overhead [xmax DEFAULT_XMAX] ;; Index, max. x-value
                  #:num-samples [num-samples DEFAULT_SAMPLES] ;; Index
                  #:font-face [font-face DEFAULT_FACE]
                  #:font-size [font-size DEFAULT_SIZE]
                  #:labels? [labels? #t]
                  #:cutoff-proportion [cutoff-proportion #f] ;; Flonum, between 0 and 1.
                  #:plot-width [width (plot-width)] ;; Index
                  #:plot-height [height (plot-height)]) ;; Index
  (when (null? S*) (error 'lnm-plot "Cannot make picture for empty list of input"))
  (define L-list (or (and (list? L) L) (list L)))
  (define ymax 100)
  (define cutoff-point (and cutoff-proportion (* ymax cutoff-proportion)))
  ;; Make renderers for the lines
  (define N-line (and N (vertical-line N #:y-max ymax
                                  #:color 'forestgreen
                                  #:width THIN)))
  (define M-line (and M (vertical-line M #:y-max ymax
                                  #:color 'goldenrod
                                  #:width THIN)))
  (define cutoff-line (and cutoff-point (horizontal-line cutoff-point #:x-max xmax
                                                    #:color 'orangered
                                                    #:style 'short-dash
                                                    #:width THICK)))
  ;; Get yticks
  ;; Set plot parameters ('globally', for all picts)
  (parameterize (
    [plot-x-ticks (compute-xticks 5)]
    [plot-y-ticks (compute-yticks ymax 6 #:exact (if cutoff-point (list cutoff-point ymax) (list ymax)))]
    [plot-x-far-ticks no-ticks]
    [plot-y-far-ticks no-ticks]
    [plot-tick-size 4]
    [plot-font-face font-face]
    [plot-font-size font-size])
    ;; Create 1 pict for each value of L
    (for/list ([L (in-list L-list)])
      (define F-config
        (let* ([get-proportion*
                ;; For each Summary, count proportion of good configs
                (for/list : (Listof (-> Real Real))
                          ([S : Summary (in-list S*)])
                  (define counter (count-configurations S L #:cache-up-to xmax))
                  (define vars (get-num-configurations S))
                  (lambda ([n : Real])
                    (/ (counter n) vars)))]
               [F
                ;; Take the mean of all proportions for all configs
                (lambda ([n : Real])
                   (define prop*
                     (for/list : (Listof Real)
                               ([gp : (-> Real Real) (in-list get-proportion*)])
                       (gp n)))
                   (round (* 100 (mean prop*))))])
          (function F
                  0 xmax
                  #:samples num-samples
                  #:color 'navy
                  #:width THICK)))
      (assert
        (plot-pict (for/list : (Listof renderer2d)
                     ([x (in-list (list N-line M-line cutoff-line F-config))] #:when x) x)
            #:x-min 1 #:x-max xmax
            #:y-min 0 #:y-max ymax
            #:x-label (and labels? "Overhead (vs. untyped)")
            #:y-label (and labels? "Avg. % Acceptable")
            #:width (* 3 width) #:height (* 3 height)) pict?))))

(: path-plot (->* [(Listof Summary) #:L (U Index (Listof Index))]
                 [#:N Index
                  #:M Index
                  #:max-overhead Index
                  #:num-samples Positive-Integer
                  #:font-face String
                  #:font-size Positive-Integer
                  #:labels? Boolean
                  #:cutoff-proportion Real
                  #:plot-width Positive-Integer
                  #:plot-height Positive-Integer
                  ]
                  (Listof Pict)))
(define (path-plot S* #:L L* ;; L* is ignored
                  #:N [N #f]  ;; Index, recommened N limit
                  #:M [M #f] ;; Index, recommended M limit
                  #:max-overhead [xmax DEFAULT_XMAX] ;; Index, max. x-value
                  #:num-samples [num-samples DEFAULT_XMAX] ;; Index
                  #:font-face [font-face DEFAULT_FACE]
                  #:font-size [font-size DEFAULT_SIZE]
                  #:labels? [labels? #t]
                  #:cutoff-proportion [cutoff-proportion #f] ;; Flonum, between 0 and 1.
                  #:plot-width [width (plot-width)] ;; Index
                  #:plot-height [height (plot-height)]) ;; Index
  (define L-list (list 0)) ;; TODO eventually generalize
  (when (null? S*) (error 'path-plot "Expected at least one summary object"))
  (define num-paths (get-num-paths (car S*)))
  (define ymax 50)
  (define cutoff-point (and cutoff-proportion (* cutoff-proportion ymax)))
  ;; Make renderers for the lines
  (define N-line (and N (vertical-line N #:y-max ymax
                                         #:color 'forestgreen
                                         #:width THIN)))
  (define M-line (and M (vertical-line M #:y-max ymax
                                         #:color 'goldenrod
                                         #:width THIN)))
  (define cutoff-line (and cutoff-point
                           (horizontal-line cutoff-point #:x-max xmax
                                                         #:color 'orangered
                                                         #:style 'short-dash
                                                         #:width THICK)))
  ;; Get yticks
  ;; Set plot parameters ('globally', for all picts)
  (parameterize (
    [plot-x-ticks (compute-xticks 5)]
    [plot-y-ticks (compute-yticks ymax 6 #:exact (if cutoff-point (list cutoff-point ymax) (list ymax)))]
    [plot-x-far-ticks no-ticks]
    [plot-y-far-ticks no-ticks]
    [plot-tick-size 4]
    [plot-font-face font-face]
    [plot-font-size font-size])
    ;; Create 1 pict for each value of L
    (for/list ([L (in-list L-list)])
      (define path-points*
        (for/list : (Listof renderer2d)
                  ([S (in-list S*)] [c (in-naturals)])
          (let ([f (count-paths S L #:cache-up-to xmax)])
            (points (for/list : (Listof (List Real Real))
                              ([n : Real (linear-seq 0 xmax num-samples)])
                      (list n (f n)))
                    #:x-min 0 #:x-max xmax
                    #:y-min 0 #:y-max ymax
                    #:color c
                    #:sym 'dot))))
      (define res
        #f ;; TODO fix
        #;(if (null? (cdr S*))
        (plot-pict (for/list : (Listof renderer2d)
                             ([x (append (list N-line M-line cutoff-line) path-points*)] #:when x) x)
            #:x-min 1 #:x-max xmax
            #:y-min 0 #:y-max ymax
            #:x-label (and labels? "Overhead (vs. untyped)")
            #:y-label (and labels? "#Paths")
            #:width width #:height height)
        (plot-pict (for/list : (Listof renderer2d)
                             ([x (list N-line M-line cutoff-line path-points*)] #:when x) x)
            #:x-min 1 #:x-max xmax
            #:legend-anchor 'top-right
            #:y-min 0 #:y-max ymax
            #:x-label (and labels? "Overhead (vs. untyped)")
            #:y-label (and labels? "#Paths")
            #:width width #:height height)))
      (if (pict? res) res (error 'notapict)))))

;; =============================================================================

;; Return a function (-> Real Index) on argument `N`
;;  that counts the number of configurations
;;  which can reach, in L or fewer steps,
;;  a configuration with overhead no more than `N`
(: count-configurations (->* [Summary Natural] [#:cache-up-to (U #f Natural) #:pdf? Boolean] (-> Real Natural)))
(define (count-configurations sm L #:cache-up-to [lim #f] #:pdf? [pdf? #f])
  (define baseline (untyped-mean sm))
  (define cache (and lim (cache-init sm lim #:L L)))
  (define total (get-num-configurations sm))
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
          (stream-length (predicate->configurations sm good?))))
    (if pdf?
      (begin0 (assert (- num-good (unbox prev-good)) index?) (set-box! prev-good num-good))
      (exact-round (* 100 (/ num-good total))))))

;; Return a function (-> Real Index) on argument `N`
;;  that counts the number of acceptable paths
;; Parameter `L` is ignored for now
(: count-paths (->* [Summary Natural] [#:cache-up-to (U #f Natural)] (-> Real Natural)))
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
(: make-configuration->good? (->* [Summary Real] [#:L Natural] (-> Bitstring Boolean)))
(define (make-configuration->good? summary good-threshold #:L [L 0])
  (lambda ([var : String])
    (for/or ([var2 (cons var (in-reach var (assert L index?)))])
      (<= (configuration->mean-runtime summary var2)
         good-threshold))))

;; -----------------------------------------------------------------------------
;; -- cache
(define-type Cache (Vectorof (Listof Bitstring)))

;; Create a cache that saves the configurations between discrete overhead values
(: cache-init (->* [Summary Natural] [#:L Natural] Cache))
(define (cache-init summary max-overhead #:L [L 0])
  (define base-overhead (untyped-mean summary))
  (: unsorted-configurations (Boxof (Sequenceof Bitstring)))
  (define unsorted-configurations (box (all-configurations summary)))
  ;; For each integer-overhead-range [0, 1] [1, 2] ... [max-1, max]
  ;; save the configurations within that overhead to a cache entry
  (for/vector : Cache ([i (in-range (add1 max-overhead))])
    (define good? (make-configuration->good? summary (* i base-overhead) #:L L))
    (define-values (good-vars rest)
      (stream-partition good? (unbox unsorted-configurations)))
    (set-box! unsorted-configurations rest)
    (stream->list good-vars)))

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

(: stream-partition (-> (-> Bitstring Boolean) (Sequenceof Bitstring) (Values (Sequenceof Bitstring) (Sequenceof Bitstring))))
(define (stream-partition f stream)
  (define not-f (lambda ([x : Bitstring]) (not (f x))))
  (values (stream-filter f stream)
          (stream-filter not-f stream)))

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

(: horizontal-line (->* [Real]
                        [#:x-min Index
                         #:x-max Index
                         #:color Symbol
                         #:width Nonnegative-Real
                         #:style Plot-Pen-Style]
                        renderer2d))
(define (horizontal-line y-val
                         #:x-min [x-min 0]
                         #:x-max [x-max 1]
                         #:color [c 'black]
                         #:width [w (line-width)]
                         #:style [s 'solid])
  (lines (list (list x-min y-val)
               (list x-max y-val))
         #:color c
         #:width w
         #:style s))

(: vertical-line (->* [Real]
                      [#:y-min Index
                       #:y-max Index
                       #:color Symbol
                       #:width Nonnegative-Real
                       #:style Plot-Pen-Style]
                      renderer2d))
(define (vertical-line x-val
                       #:y-min [y-min 0]
                       #:y-max [y-max 1]
                       #:color [c 'black]
                       #:width [w (line-width)]
                       #:style [s 'solid])
  (lines (list (list x-val y-min)
               (list x-val y-max))
         #:color c
         #:width w
         #:style s))

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
