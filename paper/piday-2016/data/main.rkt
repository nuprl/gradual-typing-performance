#lang racket/base

(require
  glob
  gtp-summarize/render-lnm
  gtp-summarize/lnm-parameters
  pict
  racket/class
  racket/string
  (only-in racket/list range)
)

(define (pict->png p path)
  (send (pict->bitmap p) save-file path 'png))

(define (main tag version)
  ;; Sort & make figures of with 6 plots each or whatever
  (parameterize ([*AXIS-LABELS?* #f]
                 [*L* '(0)]
                 [*L-LABELS?* #f]
                 [*LEGEND?* #f]
                 [*LINE-LABELS?* #t]
                 [*LOG-TRANSFORM?* #t]
                 [*M* #f]
                 [*MAX-OVERHEAD* 5]
                 [*N* #f]
                 [*NUM-SAMPLES* 60] ;; 200
                 [*LEGEND-ANCHOR* 'bottom-right]
                 [*PLOT-FONT-SCALE* 0.02]
                 [*PLOT-HEIGHT* 500]
                 [*PLOT-WIDTH* 800]
                 [*SINGLE-PLOT?* #f]
                 [*X-TICKS* (range 0 11)]
                 ;[*X-NUM-TICKS* 6]
                 [*Y-MINOR-TICKS* '(25 75)]
                 [*Y-NUM-TICKS* 3]
                 [*Y-STYLE* '%]
                 [*CACHE-TAG* #f])
    (define tag+ (if (string=? tag "zordoz") "zordoz.6.[23]" tag))
    (define rktd (car (glob (format "~a/~a-*.rktd" version tag+))))
    (pict->png
      (render-lnm (vector rktd))
      (format "~a-lnm.png" tag))))

(define (safe? str)
  (not (string-contains? str "/")))

(module+ main
  (define argv (current-command-line-arguments))
  (unless (and (= 2 (vector-length argv)) (safe? (vector-ref argv 0)))
    (raise-user-error 'main "Usage: main.rkt STRING VERSION"))
  (main (vector-ref argv 0) (vector-ref argv 1)))
