#lang typed/racket/base

(require
  require-typed-check
  "summary-adapted.rkt"
)
(require/typed/check "spreadsheet.rkt"
  [rktd->spreadsheet (-> Path-String #:output Path-String #:format Symbol Void)]
)
(require/typed/check "lnm-plot.rkt"
 [lnm-plot (-> Summary
               #:L (Listof Index)
               #:N Index
               #:M Index
               #:max-overhead Index
               #:cutoff-proportion Float
               #:num-samples Positive-Integer
               #:plot-height Positive-Integer
               #:plot-width Positive-Integer
               (Listof Any))]
)
;; Just testing

(: l-list (Listof Index))
(define l-list '(0 1 2))
(define NUM_SAMPLES 60)

(: main (-> String Void))
(define (main filename)
  ;; Parse data from input file (also creates module graph)
  (define summary (from-rktd filename))
  (define name (get-project-name summary))
  ;; Create L-N/M pictures
  (define picts (lnm-plot summary #:L l-list
                                  #:N 3
                                  #:M 10
                                  #:max-overhead 20
                                  #:cutoff-proportion 0.6
                                  #:num-samples NUM_SAMPLES
                                  #:plot-height 300
                                  #:plot-width 400))
  ;; Make a spreadsheet, just to test that too
  (rktd->spreadsheet filename #:output "./test-case-output.out" #:format 'tab)
  (void)
)

;; (time (main "../base/data/echo.rktd")) ;; 93ms
;; (time (main "../base/data/sieve.rktd")) ;; 90ms
;; (time (main "../base/data/gregor.rktd")) ;; 13203ms
(time (main "../base/data/suffixtree.rktd")) ;; 143ms
