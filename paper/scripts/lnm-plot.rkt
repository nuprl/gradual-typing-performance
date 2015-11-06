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
)

;; -----------------------------------------------------------------------------

(require
  plot/typed/pict
  (only-in racket/math exact-floor)
  (only-in plot/utils linear-seq)
  (only-in racket/math exact-floor exact-ceiling)
  (only-in racket/format ~r)
  "bitstring.rkt"
  "summary.rkt"
  "pict-types.rkt"
  "stream-types.rkt"
)

;; =============================================================================
;; --- constants

(define DEFAULT_N 3)
(define DEFAULT_M 10)
(define DEFAULT_XLIMIT 20)
(define DEFAULT_CUTOFF 0.6)
(define DEFAULT_SAMPLES 60)

(define THIN (* 0.8 (line-width)))
(define THICK (* 1.25 (line-width)))

(define DEFAULT_FACE "bold")
(define DEFAULT_SIZE 20)

;; -----------------------------------------------------------------------------
;; --- plotting

(: lnm-plot (->* [Summary #:L (U Index (Listof Index))]
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
(define (lnm-plot summary
                  #:L L ;; (U Index (Listof Index)), L-values to plot
                  #:N [N DEFAULT_N]  ;; Index, recommened N limit
                  #:M [M DEFAULT_M] ;; Index, recommended M limit
                  #:max-overhead [xmax DEFAULT_XLIMIT] ;; Index, max. x-value
                  #:num-samples [num-samples DEFAULT_SAMPLES] ;; Index
                  #:font-face [font-face DEFAULT_FACE]
                  #:font-size [font-size DEFAULT_SIZE]
                  #:labels? [labels? #t]
                  #:cutoff-proportion [cutoff-proportion DEFAULT_CUTOFF] ;; Flonum, between 0 and 1.
                  #:plot-width [width (plot-width)] ;; Index
                  #:plot-height [height (plot-height)]) ;; Index
  (define L-list (or (and (list? L) L) (list L)))
  (define num-vars (get-num-configurations summary))
  (define cutoff-point (* cutoff-proportion num-vars))
  ;; Make renderers for the lines
  (define N-line (vertical-line N #:y-max num-vars
                                  #:color 'forestgreen
                                  #:width THIN))
  (define M-line (vertical-line M #:y-max num-vars
                                  #:color 'goldenrod
                                  #:width THIN))
  (define cutoff-line (horizontal-line cutoff-point #:x-max xmax
                                                    #:color 'orangered
                                                    #:style 'short-dash
                                                    #:width THICK))
  ;; Get yticks
  ;; Set plot parameters ('globally', for all picts)
  (parameterize (
    [plot-x-ticks (compute-xticks 5)]
    [plot-y-ticks (compute-yticks num-vars 6 #:exact (list cutoff-point num-vars))]
    [plot-x-far-ticks no-ticks]
    [plot-y-far-ticks no-ticks]
    [plot-tick-size 4]
    [plot-font-face font-face]
    [plot-font-size font-size])
    ;; Create 1 pict for each value of L
    (for/list ([L (in-list L-list)])
      (define F-configs
        (function (count-configurations summary L #:cache-up-to xmax) 0 xmax
                          #:samples num-samples
                          #:color 'navy
                          #:width THICK))
      (define res
        (plot-pict (list N-line M-line cutoff-line F-configs)
                   #:x-min 1
                   #:x-max xmax
                   #:y-min 0
                   #:y-max num-vars
                   #:x-label (and labels? "Overhead (vs. untyped)")
                   #:y-label (and labels? "Count")
                   #:width width
                   #:height height))
      (if (pict? res) res (error 'notapict)))))

(: path-plot (->* [Summary #:L (U Index (Listof Index))]
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
(define (path-plot summary #:L L* ;; L* is ignored
                  #:N [N DEFAULT_N]  ;; Index, recommened N limit
                  #:M [M DEFAULT_M] ;; Index, recommended M limit
                  #:max-overhead [xmax DEFAULT_XLIMIT] ;; Index, max. x-value
                  #:num-samples [num-samples DEFAULT_XLIMIT] ;; Index
                  #:font-face [font-face DEFAULT_FACE]
                  #:font-size [font-size DEFAULT_SIZE]
                  #:labels? [labels? #t]
                  #:cutoff-proportion [cutoff-proportion DEFAULT_CUTOFF] ;; Flonum, between 0 and 1.
                  #:plot-width [width (plot-width)] ;; Index
                  #:plot-height [height (plot-height)]) ;; Index
  (define L-list (list 0)) ;; TODO eventually generalize
  (define num-paths (get-num-paths summary))
  (define ymax 50)
  (define cutoff-point (* cutoff-proportion ymax))
  ;; Make renderers for the lines
  (define N-line (vertical-line N #:y-max ymax
                                  #:color 'forestgreen
                                  #:width THIN))
  (define M-line (vertical-line M #:y-max ymax
                                  #:color 'goldenrod
                                  #:width THIN))
  (define cutoff-line (horizontal-line cutoff-point #:x-max xmax
                                                    #:color 'orangered
                                                    #:style 'short-dash
                                                    #:width THICK))
  ;; Get yticks
  ;; Set plot parameters ('globally', for all picts)
  (parameterize (
    [plot-x-ticks (compute-xticks 5)]
    [plot-y-ticks (compute-yticks ymax 6 #:exact (list cutoff-point ymax))]
    [plot-x-far-ticks no-ticks]
    [plot-y-far-ticks no-ticks]
    [plot-tick-size 4]
    [plot-font-face font-face]
    [plot-font-size font-size])
    ;; Create 1 pict for each value of L
    (for/list ([L (in-list L-list)])
      (define path-points
        (let ([f (count-paths summary L #:cache-up-to xmax)])
          (points (for/list : (Listof (List Real Real))
                            ([n : Real (linear-seq 0 xmax num-samples)])
                    (list n (f n)))
                  #:x-min 0 #:x-max xmax
                  #:y-min 0 #:y-max ymax
                  #:color 'violet
                  #:sym 'dot)))
      (define res
        (plot-pict (list N-line M-line cutoff-line path-points)
                   #:x-min 1
                   #:x-max xmax
                   #:y-min 0
                   #:y-max ymax
                   #:x-label (and labels? "Overhead (vs. untyped)")
                   #:y-label (and labels? "#Paths")
                   #:width width
                   #:height height))
      (if (pict? res) res (error 'notapict)))))


;; Return a function (-> Real Index) on argument `N`
;;  that counts the number of configurations
;;  which can reach, in L or fewer steps,
;;  a configuration with overhead no more than `N`
(: count-configurations (->* [Summary Index] [#:cache-up-to (U #f Index)] (-> Real Natural)))
(define (count-configurations sm L #:cache-up-to [lim #f])
  (define baseline (untyped-mean sm))
  (define cache (and lim (cache-init sm lim #:L L)))
  (lambda ([N-raw : Real]) ;; Real, but we assume non-negative
    (: N Nonnegative-Real)
    (define N (if (>= N-raw 0) N-raw (error 'count-configurations)))
    (define good? (make-configuration->good? sm (* N baseline) #:L L))
    (if (and cache lim (<= N lim))
        ;; Use cache to save some work, only test the configurations
        ;; in the next bucket
        (cache-lookup cache N good?)
        ;; No cache, need to test all configurations
        (stream-length (predicate->configurations sm good?)))))

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
(: make-configuration->good? (->* [Summary Real] [#:L Index] (-> String Boolean)))
(define (make-configuration->good? summary good-threshold #:L [L 0])
  (lambda ([var : String])
    (for/or ([var2 (cons var (in-reach var L))])
      (<= (configuration->mean-runtime summary var2)
         good-threshold))))

;; -----------------------------------------------------------------------------
;; -- cache
(define-type Cache (Vectorof (Listof String)))

;; Create a cache that saves the configurations between discrete overhead values
(: cache-init (->* [Summary Index] [#:L Index] Cache))
(define (cache-init summary max-overhead #:L [L 0])
  (define base-overhead (untyped-mean summary))
  (: unsorted-configurations (Boxof (Sequenceof String)))
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
(: cache-lookup (-> Cache Nonnegative-Real (-> String Boolean) Natural))
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

(: stream-partition (-> (-> String Boolean) (Sequenceof String) (Values (Sequenceof String) (Sequenceof String))))
(define (stream-partition f stream)
  (define not-f (lambda ([x : String]) (not (f x))))
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
           (for/list : (Listof String) ([pt (in-list pre-ticks)])
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

;(module+ main
;  (require
;    racket/cmdline
;    (only-in pict pict->bitmap)
;    (only-in racket/class send)
;    (only-in racket/sequence sequence->list)
;  )
;  (define l-param (box 2))
;  (command-line #:program "l-n/m plotter"
;                #:once-each
;                [("-l") l-value
;                        "Set max value of L"
;                        (set-box! l-param (cast l-value Index))]
;                #:args (filename)
;    (define summary (from-rktd filename))
;    (define name (get-project-name summary))
;    (define l-list (sequence->list (in-range 0 (add1 l-param))))
;    (define picts (lnm-plot summary #:L l-list
;                                    #:plot-height 300
;                                    #:plot-width 400))
;    (for/list : (Listof Any) ([pic (in-list picts)]
;               [i (in-list l-list)])
;      (define fname (format "output/~a~a.png" name i))
;      (send (pict->bitmap pic) save-file fname 'png)))
;)

;; -----------------------------------------------------------------------------

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
