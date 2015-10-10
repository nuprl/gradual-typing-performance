#lang racket/base

(require
  "spreadsheet.rkt"
  "summary.rkt"
  "lnm-plot.rkt"
)

(module+ test
  (require
    "summary.rkt"
    "lnm-plot.rkt"
    pict
    racket/class
    glob
  )
  (define L-list '(0 1 2))
  (define DATAS
    (list
     ;; "../data/echo.rktd"
     ;; "../data/synth.rktd"
     ;; "../data/gregor-05-11.rktd"
     ;; "../data/gregor-05-24.rktd"
     ;; ;; BAD "../data/kcfa-06-01.rktd"
     ;; "../data/mbta-04-20.rktd"
     ;; "../data/morsecode-06-19.rktd"
     ;; "../data/morsecode.rktd"
     ;; "../data/sieve-04-06.rktd"
     ;; "../data/snake-04-10.rktd"
     ;; "../data/suffixtree-06-10.rktd"
     ;; ;; BAD "../data/tetris.rktd"
     ;; "../data/zordoz-04-09.rktd"
  ))
  (for ([rktd (in-list DATAS)])
    (printf "Parsing '~a'\n" rktd)
    (define data (from-rktd rktd))
    (define name (get-project-name data))
    (printf "Making plots for '~a'\n" name)
    (define picts (lnm-plot data #:L L-list
                                  #:plot-width 400
                                  #:plot-height 300))
    (printf "Saving plots for '~a'\n" name)
    (for/list ([pic (in-list picts)]
               [i (in-list L-list)])
      (define fname (format "output/~a~a.png" name i))
      (send (pict->bitmap pic) save-file fname 'png)))

)
