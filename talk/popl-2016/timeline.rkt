#lang racket/base

(require pict
         ppict
         racket/list)

(provide timeline-pict)

(define *timeline-pict
  (inset 
   (cc-superimpose
    (apply hc-append 30
           (build-list
            11
            (λ (n)
              (vc-append 5
                         (blank 1 12)
                         (colorize (linewidth 2 (vline 1 30))
                                   "dark gray")
                         (text (number->string (+ 2006 n))
                               "Roboto Condensed" 15)))))
   (colorize (linewidth 3 (hline 700 1)) "dark gray"))
   3))

(define timeline-pict
  (ppict-do
   (vc-append (blank 1 100) *timeline-pict)
   #:go (coord 0.83 0.00 'lt)
   (colorize (text "Safe TypeScript" "Roboto" 15) "firebrick")
   #:go (coord 0.83 0.15 'lt)
   (colorize (text "StrongScript" "Roboto" 15) "indigo")
   #:go (coord 0.745 0.35 'lt)
   (colorize (text "Gradualtalk" "Roboto" 15) "forest green")
   #:go (coord 0.745 0.50 'lt)
   (colorize (text "Reticulated Python" "Roboto" 15) "chocolate")
   #:go (coord 0.163 0.50 'lt)
   (colorize (text "Typed Racket" "Roboto" 15)
             "darkorange")))