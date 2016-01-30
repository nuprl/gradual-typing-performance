#lang racket

(require
  ;"data.rkt"
  glob
  math/statistics
  plot/no-gui
)

;(define data* (list
;  GREGOR-DATA
;  KCFA-DATA
;  LNM-DATA
;  MBTA-DATA
;  MORSECODE-DATA
;  QUAD-DATA
;  SIEVE-DATA
;  SNAKE-DATA
;  SUFFIXTREE-DATA
;  SYNTH-DATA
;  TETRIS-DATA
;  ZORDOZ-DATA
;))

;(for ([d (in-list data*)])
;  (system (format "racket6.3 scripts/sort-configurations.rkt '~a'" d)))

;(displayln "order:")
;(for ([g (in-glob "*-worst*")])
;  (displayln g))
;
;(define d2 (for/list ([g (in-glob "*-worst.rktd")]) (file->value g)))
;
;(displayln "mean")
;(for ([d (in-list d2)])
;  (displayln (mean (map cadddr d))))
;
;(displayln "median")
;(for ([d (in-list d2)])
;  (displayln (median < (map cadddr d))))

(define proj* '(
;fsmv2
gregor
kcfa
lnm
mbta
morsecode
quad
sieve
snake
suffixtree
synth
tetris
zombie
zordoz
))

(define mean* '(
;2723.18375
2.717918701171875
9.096640625000001
0.61390625
1.763125
1.409375
31.49857681274414
51.745
32.0585546875
51.37796875
39.61669921875
33.21021484375
16.06875
2.7196875))

(define median* '(
;2.59
2.69
8.52
0.39
1.86
1.38
33.68
1.0
24.02
44.06
41.03
24.83
1.56
2.82))

(define W 0.4)
(parameterize ([plot-width 750]
               [plot-height 450]
               [point-size 12]
               [plot-font-size 12]
               ;[ticks-default-number (length proj)]
               [plot-x-ticks no-ticks] ;(linear-ticks #:number (length proj*) #:base 10 #:divisors '(1) )]
              )
 (let ([mean-mean (mean mean*)]
       [xmax (+ 1 (length proj*))])
  (plot-file
    (cons
        (rectangles (list (vector (ivl 0 xmax) (ivl mean-mean mean-mean)))
                    #:alpha 0.4
                    #:color 0)
      ;; TODO rectangle
    (for/list ([m (in-list mean*)]
               ;[md (in-list median*)]
               [p (in-list proj*)]
               [i (in-naturals 1)])
      (printf "~a = ~a\n" i p)
      (rectangles (list (vector (ivl (- i W) i) (ivl 0 m)))
        #:color i
      )))
    "mean.png" 'png
    #:x-min 0
    #:x-max xmax
    #:y-min 0
    #:y-max 60
    #:y-label ""
    #:x-label ""
    #:title ""
  ))
 (let ([mean-med (mean median*)]
       [xmax (+ 1 (length proj*))])
  (plot-file
    (cons
        (rectangles (list (vector (ivl 0 xmax) (ivl mean-med mean-med)))
                    #:alpha 0.4
                    #:color 0)
      ;; TODO rectangle
    (for/list (
               [m (in-list median*)]
               [p (in-list proj*)]
               [i (in-naturals 1)])
      (rectangles (list (vector (ivl (- i W) i) (ivl 0 m)))
        #:color i
      )))
    "median.png" 'png
    #:x-min 0
    #:x-max xmax
    #:y-min 0
    #:y-max 60
    #:y-label ""
    #:x-label ""
    #:title ""
  ))
)
