#lang typed/racket/base

(provide defparam Y-Style)
;; Calls to (defparam id ...) expand to a (provide id)

(require
  (for-syntax racket/base syntax/parse)
  plot/typed/utils
)

;; -----------------------------------------------------------------------------
;; --- plumbing

;; Should look `*LIKE-THIS*`, with asterisks on either end and capital letters or
;;  dashes in-between.
(define-for-syntax (well-styled-param? id)
  (regexp-match? #rx"^\\*[A-Z]([A-Z\\-])*\\??\\*$" (symbol->string id)))

(define-syntax (defparam stx)
  (syntax-parse stx
   [(_ name:id type default-value)
    (define name-sym (syntax-e #'name))
    (unless (well-styled-param? name-sym)
      (error 'lnm-plot (format "Parameter id '~a' doesn't match preferred style '*~a*', please refactor" name-sym name-sym)))
    (syntax/loc stx
      (begin
        (define name : (Parameterof type) (make-parameter default-value))
        (provide name)))]))

;;; (module+ test
;;;   (begin-for-syntax
;;;     (require rackunit)
;;; 
;;;     (check-true (well-styled-param? '*YES*))
;;;     (check-true (well-styled-param? '*YES-AND-YES*)) ;;;     (check-true (well-styled-param? '*YOLO*))
;;;     (check-true (well-styled-param? '*SOMETHING-NOT-STARTING-WITH-Y*))
;;;     (check-true (well-styled-param? '*A*))
;;;     (check-true (well-styled-param? '*Z-*))
;;; 
;;;     (check-false (well-styled-param? 'NO*))
;;;     (check-false (well-styled-param? '*NO))
;;;     (check-false (well-styled-param? '**))
;;;     (check-false (well-styled-param? '*-*))
;;;     (check-false (well-styled-param? '*))
;;;     (check-false (well-styled-param? 'X))
;;;     (check-false (well-styled-param? '*NO_UNDERSCORES*))
;;; ))

;; -----------------------------------------------------------------------------
;; --- parameters

;; These are all defined as:
;; (defparam *NAME* T VAL)
;; - *NAME* is the parameter name
;; - T is the parameter type
;; - VAL is the default value

;; --- Lines to plot
(defparam *N* (U #f Natural) #f)
(defparam *M* (U #f Natural) #f)
(defparam *L* (U Natural (Listof Natural) (Listof (List Natural Plot-Pen-Style))) 0)
(defparam *CUTOFF-PROPORTION* (U #f Real) #f) ;; [0,1]

;; --- Plot bounds / sizes / colors
(defparam *MAX-OVERHEAD* Natural 20)
(defparam *NUM-SAMPLES*  Positive-Integer (assert (* (*MAX-OVERHEAD*) 3) positive?))

(defparam *PLOT-FONT-FACE* String "bold")
(defparam *PLOT-FONT-SIZE* Positive-Integer 20)

(defparam *TABLE-FONT-FACE*   String "Liberation Serif")
(defparam *TABLE-FONT-SIZE* Positive-Index 10)

(defparam *TITLE-FONT-FACE*   String "Liberation Serif")
(defparam *TITLE-FONT-SIZE*   Positive-Integer (+ 2 (*TABLE-FONT-SIZE*)))

(defparam *PLOT-WIDTH*  Exact-Positive-Integer 360)
(defparam *PLOT-HEIGHT* Exact-Positive-Integer 300)

;; --- Other styles
(defparam *LNM-COLOR*    Plot-Color 'navy)
(defparam *N-COLOR*      Plot-Color 'forestgreen)
(defparam *M-COLOR*      Plot-Color 'goldenrod)
(defparam *CUTOFF-COLOR* Plot-Color 'orangered)

(defparam *LNM-STYLE*    Plot-Pen-Style 'solid)
(defparam *N-STYLE*      Plot-Pen-Style 'solid)
(defparam *M-STYLE*      Plot-Pen-Style 'solid)
(defparam *CUTOFF-STYLE* Plot-Pen-Style 'short-dash)

(define thin (* 0.8 (line-width)))
(define thick (* 1.25 (line-width)))
(defparam *LNM-WIDTH*    Nonnegative-Real thick)
(defparam *N-WIDTH*      Nonnegative-Real thin)
(defparam *M-WIDTH*      Nonnegative-Real thin)
(defparam *CUTOFF-WIDTH* Nonnegative-Real thin)

(defparam *TICK-SIZE* Natural 4) ;; Dude IDK
(defparam *X-NUM-TICKS* Natural 5)
(defparam *Y-NUM-TICKS* Natural 6)
(define-type Y-Style (U 'count '%))
(defparam *Y-STYLE* Y-Style 'count)

;; --- Boolean flags
(defparam *AXIS-LABELS?*   Boolean #t) ;; If #t, label all plot axes
(defparam *HISTOGRAM?*     Boolean #f) ;; If #t, make a histogram instead of a line
(defparam *L-LABELS?*      Boolean #f) ;; If #t, label each plot column with its L-value
(defparam *LEGEND?*        Boolean #f) ;; If #t, make a legend for all plots
(defparam *LINE-LABELS?*   Boolean #t) ;; If #t, label all plot lines
(defparam *LOG-TRANSFORM?* Boolean #f)
(defparam *MAKE-TABLE?*    Boolean #f) ;; If #t, make a table of Summary statistics
(defparam *PDF?*           Boolean #f) ;; If #t, plot the Probabilistic Distribution Function
(defparam *SINGLE-PLOT?*   Boolean #t) ;; If #t, make 1 plot for each L
(defparam *TITLE?*         Boolean #t) ;; If #t, print a plot title

(defparam *SHOW-PATHS?* Boolean #f)
;; If #t, make a path picture

(defparam *AGGREGATE* (U #f #t 'mean) #f)
;; TODO, something about combining figures

(defparam *OUTPUT* Path-String "./output.png") ;; Where to save output pict
(defparam *CACHE-PREFIX* String "./compiled/lnm-cache-")
