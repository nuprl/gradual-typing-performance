#lang racket/base

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
  ;; Create an L-NM plot based on the given parameters (see function for params)
  ;; Builds one plot for each given value of L.
  ;; (-> Summary #:L (Listof Index) <OPTIONS> (Listof Pict))
  lnm-plot
)

;; -----------------------------------------------------------------------------

(require
  "bitstring.rkt"
  "summary.rkt"
  plot/pict
  (only-in racket/math exact-floor)
  (only-in plot/utils linear-seq)
  (only-in racket/math exact-floor exact-ceiling)
  (only-in racket/stream stream-length stream->list stream-filter)
  (only-in racket/format ~r)
)

;; =============================================================================
;; --- constants

(define DEFAULT_N 3)
(define DEFAULT_M 10)
(define DEFAULT_XLIMIT 20)
(define DEFAULT_CUTOFF 0.6)
(define DEFAULT_SAMPLES 60)

(define THIN (* 0.8 (line-width)))
(define THICK (* 1.4 (line-width)))

(define DEFAULT_FACE "bold")
(define DEFAULT_SIZE 20)

;; -----------------------------------------------------------------------------
;; --- plotting

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
  (define num-vars (get-num-variations summary))
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
    [plot-font-face font-face]
    [plot-font-size font-size])
    ;; Create 1 pict for each value of L
    (for/list ([L (in-list L-list)])
      (define F (function (count-variations summary L #:cache-up-to xmax) 0 xmax
                          #:samples num-samples
                          #:color 'navy
                          #:width THICK))
      (plot-pict (list N-line M-line cutoff-line F)
                 #:x-min 1
                 #:x-max xmax
                 #:y-min 0
                 #:y-max num-vars
                 #:x-label (and labels? "Overhead (vs. untyped)")
                 #:y-label (and labels? "Count")
                 #:width width
                 #:height height))))

;; Return a function (-> Real Index) on argument `N`
;;  that counts the number of variations
;;  which can reach, in L or fewer steps,
;;  a variation with overhead no more than `N`
;; (: count-variations (-> Summary Index (-> Real Index)))
(define (count-variations sm L #:cache-up-to [lim #f])
  (define baseline (untyped-mean sm))
  (define cache (and lim (cache-init sm lim #:L L)))
  (lambda (N) ;; Real, but we assume non-negative
    (define good? (make-variation->good? sm (* N baseline) #:L L))
    (if (and cache (<= N lim))
        ;; Use cache to save some work, only test the variations
        ;; in the next bucket
        (cache-lookup cache N good?)
        ;; No cache, need to test all variations
        (stream-length (predicate->variations sm good?)))))

;; Make a predicate checking whether a variation is good.
;; Good = no more than `L` steps away from a variation
;;        with average runtime less than `good-threshold`.
(define (make-variation->good? summary good-threshold #:L [L 0])
  (lambda (var)
    (for/or ([var2 (cons var (in-reach var L))])
      (<= (variation->mean-runtime summary var2)
         good-threshold))))

;; -----------------------------------------------------------------------------
;; -- cache

;; Create a cache that saves the configurations between discrete overhead values
(define (cache-init summary max-overhead #:L [L 0])
  (define base-overhead (untyped-mean summary))
  (define unsorted-variations (box (all-variations summary)))
  ;; For each integer-overhead-range [0, 1] [1, 2] ... [max-1, max]
  ;; save the variations within that overhead to a cache entry
  (for/vector ([i (in-range (add1 max-overhead))])
    (define good? (make-variation->good? summary (* i base-overhead) #:L L))
    (define-values (good-vars rest)
      (stream-partition good? (unbox unsorted-variations)))
    (set-box! unsorted-variations rest)
    (stream->list good-vars)))

;; Count the number of variations with running time less than `overhead`.
;; Use `test-fun` to manually check variations we aren't sure about
(define (cache-lookup $$$ overhead test-fun)
  (define lo-overhead (exact-floor overhead))
  (define hi-overhead (exact-ceiling overhead))
  (define num-known
    (for/sum ([i (in-range (add1 lo-overhead))])
      (length (vector-ref $$$ i))))
  (if (= hi-overhead lo-overhead)
      ;; Short circuit, because original overhead was an integer
      num-known
      ;; Else test all the variations in the "next" bucket
      (+ num-known
         (for/sum ([var (in-list (vector-ref $$$ hi-overhead))]
                   #:when (test-fun var)) 1))))

(define (stream-partition f stream)
  (define not-f (lambda (x) (not (f x))))
  (values (stream-filter f stream)
          (stream-filter not-f stream)))

;; -----------------------------------------------------------------------------
;; --- plotting utils

;; Compute `num-ticks` evenly-spaced y ticks between 0 and `max-y`.
;; Round all numbers down a little, except for numbers in the optional
;;  list `exact`.
(define INVISIBLE "\u00A0") ;; Unicode for non-breaking space
(define (compute-yticks max-y num-ticks #:exact [exact '()])
  (define exact-list (or (and (list? exact) exact) (list exact)))
  (define round-y (if (< max-y 1000) ;;TODO
                      round
                      (lambda (n) (* 100 (exact-floor (/ n 100))))))
  (ticks (lambda (ax-min ax-max)
           (for/list ([y (in-list (linear-seq ax-min ax-max num-ticks #:end? #t))])
             (define rounded (round-y y))
             (define ex (findf (lambda (n) (= rounded (round-y n)))
                               exact-list))
             (pre-tick (or (and ex (round ex))
                           rounded)
                       #t)))
         (lambda (ax-min ax-max pre-ticks)
                 (for/list ([pt (in-list pre-ticks)])
                   (~r (pre-tick-value pt) #:min-width 5 #:pad-string INVISIBLE)))))

(define (compute-xticks num-ticks)
  (ticks (lambda (ax-min ax-max)
           (for/list ([i (in-list (linear-seq 1 ax-max num-ticks))])
             (pre-tick (round i) #t)))
         (lambda (ax-min ax-max pre-ticks)
           (for/list ([pt (in-list pre-ticks)])
             (format "~ax" (pre-tick-value pt))))))

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

(module+ main
  (require
    racket/cmdline
    (only-in pict pict->bitmap)
    (only-in racket/class send)
  )
  (define l-param (box 2))
  (command-line #:program "l-n/m plotter"
                #:once-each
                [("-l") l-value
                        "Set max value of L"
                        (set-box! l-param l-value)]
                #:args (filename)
    (define summary (from-rktd filename))
    (define name (get-project-name summary))
    (define l-list (for/list ([i (in-range (add1 (unbox l-param)))]) i))
    (define picts (lnm-plot summary #:L l-list
                                    #:plot-height 300
                                    #:plot-width 400))
    (for/list ([pic (in-list picts)]
               [i (in-list l-list)])
      (define fname (format "output/~a~a.png" name i))
      (send (pict->bitmap pic) save-file fname 'png)))
)

;; -----------------------------------------------------------------------------

(module+ test
  (require rackunit)
)
