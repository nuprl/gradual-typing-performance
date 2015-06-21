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
  (only-in racket/math exact-floor)
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
  (define N-line (vertical-line N #:y-max num-vars
                                  #:color 'green
                                  #:width THIN))
  (define M-line (vertical-line M #:y-max num-vars
                                  #:color 'yellow
                                  #:width THIN))
  (define cutoff-line (horizontal-line cutoff-point #:x-max xmax
                                                    #:color 'red
                                                    #:style 'short-dash
                                                    #:width THICK))
  ;; Get yticks
  (define yticks (compute-yticks num-vars 6 #:exact (list cutoff-point)))
  ;; Create 1 pict for each value of L
  (parameterize (
    [plot-x-ticks (compute-xticks 5)]
    [plot-y-ticks (compute-yticks num-vars 6 #:exact cutoff-point)]
    [plot-x-far-ticks no-ticks]
    [plot-y-far-ticks no-ticks]
    )
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
                 #:y-label "Count"))))

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
;; TODO
(define (compute-yticks max-y num-ticks #:exact [exact '()])
  (define exact-list (or (and (list? exact) exact) (list exact)))
  (define round-y (if (< max-y 1000)
                      round
                      (lambda (n) (* 100 (exact-floor (/ n 100))))))
  (ticks (lambda (ax-min ax-max)
           (for/list ([y (in-list (linear-seq ax-min ax-max num-ticks))])
             (define rounded (round-y y))
             (define ex (findf (lambda (n) (= rounded (round-y n)))
                               exact-list))
             (pre-tick (or (and ex (round ex))
                           rounded)
                       #t)))
         (lambda (ax-min ax-max pre-ticks)
                 (for/list ([pt (in-list pre-ticks)])
                   (number->string (pre-tick-value pt))))))

(define (compute-xticks num-ticks)
  (ticks (lambda (ax-min ax-max)
           (for/list ([i (in-list (linear-seq 1 ax-max num-ticks))])
             (pre-tick (round i) #t)))
         (lambda (ax-min ax-max pre-ticks)
           (for/list ([pt (in-list pre-ticks)])
             (format "~ax" (pre-tick-value pt))))))

;; -----------------------------------------------------------------------------
;; --- other

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

;(define (no-ticks)
;  (ticks (lambda (ax-min ax-max) '())
;         (lambda (ax-min ax-max pre-ticks) '())))

;; =============================================================================

(module+ test
  (require rackunit)
)

