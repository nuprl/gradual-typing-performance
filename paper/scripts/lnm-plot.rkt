#lang racket/base

;; Create L-N/M plots for .rktd files

;; Input:
;; - Raw experimental data (.rktd)
;;   (Optionally a list)
;; Output:
;; - Pict showing an L-N/M plot for the data
;;   (Or, a list of such plots)

(provide
  ;; (->* [Summary] [TODO] Pict)
  lnm-plot
)

;; -----------------------------------------------------------------------------

(require
  "bitstring.rkt"
  "summary.rkt"
  plot/pict
  (only-in plot/utils linear-seq)
  ;(only-in racket/stream stream-length)
  racket/stream
)

;; =============================================================================
;; --- constants

(define DEFAULT_N 3)
(define DEFAULT_M 10)
(define DEFAULT_XLIMIT 20)
(define DEFAULT_CUTOFF 0.6)
(define DEFAULT_SAMPLES 20)

(define THIN (* 2 (line-width)))
(define THICK (* 5 (line-width)))

;; -----------------------------------------------------------------------------
;; --- plotting

(define (lnm-plot summary
                  #:L L      ;; (U Index (Listof Index)), L-values to plot
                  #:N [N DEFAULT_N]  ;; Index, recommened N limit
                  #:M [M DEFAULT_M] ;; Index, recommended M limit
                  #:max-overhead [xmax DEFAULT_XLIMIT]
                  #:num-samples [num-samples DEFAULT_SAMPLES]
                  #:cutoff-proportion [cutoff-proportion DEFAULT_CUTOFF])
  (define L-list (or (and (list? L) L) (list L)))
  (define num-vars (get-num-variations summary))
  (define cutoff-point (* cutoff-proportion num-vars))
  ;; Make renderers for the lines
  (define N-line (vertical-line N #:ymax num-vars
                                  #:color 'green
                                  #:width THIN))
  (define M-line (vertical-line M #:ymax num-vars
                                  #:color 'yellow
                                  #:width THIN))
  (define cutoff-line (horizontal-line cutoff-point #:xmax xmax
                                                    #:color 'red
                                                    #:style 'short-dash
                                                    #:width THICK))
  ;; Get yticks
  (define yticks (compute-yticks num-vars 6 #:exact (list cutoff-point)))
  ;; Create 1 pict for each value of L
  (for/list ([L (in-list L-list)])
    (define F (function (count-variations summary L) 0 xmax
                        #:samples num-samples
                        #:color 'blue
                        #:width THICK))
    (plot-pict (list N-line M-line cutoff-line F)
               #:x-min 0
               #:x-max xmax
               #:y-min 0
               #:y-max num-vars
               #:x-label "Overhead (vs. untyped)"
               #:y-label "Count")))

;; Return a function (-> Real Index) on argument `N`
;;  that counts the number of variations
;;  which can reach, in L or fewer steps,
;;  a variation with overhead no more than `N`
;; (: count-variations (-> Summary Index (-> Real Index)))
(define (count-variations sm L)
  ;; TODO add cache?
  (define baseline (untyped-mean sm))
  (lambda (N)
    (define (good? var)
      (for/or ([var2 (cons var (in-reach var L))])
        (< (variation->mean-runtime sm var2)
           (* N baseline))))
    (define good-variations (predicate->variations sm good?))
    (length (stream->list good-variations))))

;; Compute `num-ticks` evenly-spaced y ticks between 0 and `max-y`.
;; Round all numbers down a little, except for numbers in the optional
;;  list `exact`.
;; (: compute-yticks (->* [Index Index] [#:exact (Listof Index)] (Listof Index)))
(define (compute-yticks max-y num-ticks #:exact [other-exact '()])
  (linear-ticks #:number num-ticks))
;  (define exact (cons 0 (cons max-y other-exact)))
;  (define round-to
;  ;; From docs: determines num.ticks and where placed
;  ;; (-> real real (listof pre-tick))
;  (define (layout-fun ax-min ax-max)
;    ;; TODO remove assertion
;    (unless (= ax-max max-y) (internal-error "expected ymax '~a' does not match arg '~a'" max-y ax-max))
;    (for/list ([i (linear-seq ax-min ax-max num-ticks)])
;      (if (member i exact)
;          i
;          (round i rount-to)))))
;  ;; Formatter is the identity (may want to add boldface, but whatevs)
;  ;; (-> real real (listof pre-tick) (listof string))
;  (define (format-fun ax-min ax-max pre-ticks)
;    (for/list ([pt (in-list pre-ticks)])
;      (number->string (pre-tick value pt))))
;  (ticks layout-fun format-fun))

;; -----------------------------------------------------------------------------
;; --- other

(define (horizontal-line y-val
                         #:xmin [x-min 0]
                         #:xmax [x-max 1]
                         #:color [c 'black]
                         #:width [w (line-width)]
                         #:style [s 'solid])
  (lines (list (list x-min y-val)
               (list x-max y-val))
         #:color c
         #:width w
         #:style s))

(define (vertical-line x-val
                       #:ymin [y-min 0]
                       #:ymax [y-max 1]
                       #:color [c 'black]
                       #:width [w (line-width)]
                       #:style [s 'solid])
  (lines (list (list x-val y-min)
               (list x-val y-max))
         #:color c
         #:width w
         #:style s))


;; =============================================================================

(module+ test
  (require rackunit)
)

