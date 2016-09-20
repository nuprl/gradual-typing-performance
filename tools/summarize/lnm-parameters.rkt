#lang typed/racket/base

;; Calls to (defparam id ...) expand to a (provide id)
(provide
  defparam
)

(require
  (for-syntax racket/base syntax/parse)
  plot/typed/utils
)

;; -----------------------------------------------------------------------------
;; --- plumbing

(define-syntax-rule (deftype id t)
  (begin (define-type id t) (provide id)))

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
(defparam *N* (U #f Real) #f)
(defparam *M* (U #f Real) #f)
(defparam *L* (U Natural (Listof Natural) (Listof (List Natural Plot-Pen-Style))) 0)
(defparam *CUTOFF-PROPORTION* (U #f Real) #f) ;; [0,1]

;; --- Plot bounds / sizes / colors
(defparam *MAX-OVERHEAD* Natural 20)
(defparam *NUM-SAMPLES*  Positive-Integer (assert (* (*MAX-OVERHEAD*) 3) positive?))

(defparam *PLOT-FONT-FACE* String "bold")
(defparam *PLOT-FONT-SCALE* Nonnegative-Real 0.03)

(defparam *TABLE-FONT-FACE*   String "Liberation Serif")
(defparam *TABLE-FONT-SIZE* Positive-Index 10)

(defparam *TITLE-FONT-FACE*   String "Liberation Serif")
(defparam *TITLE-FONT-SIZE*   Positive-Index (assert (+ 2 (*TABLE-FONT-SIZE*)) index?))

(defparam *PLOT-WIDTH*  Exact-Positive-Integer 600)
(defparam *PLOT-HEIGHT* Exact-Positive-Integer 300)

;; --- Other styles
(defparam *LNM-COLOR*    Plot-Color 'navy)
(defparam *N-COLOR*      Plot-Color 'orangered)
(defparam *M-COLOR*      Plot-Color 'dimgray)
(defparam *CUTOFF-COLOR* Plot-Color 'orangered)

(defparam *LNM-STYLE*    Plot-Pen-Style 'solid)
(defparam *N-STYLE*      Plot-Pen-Style 'solid)
(defparam *M-STYLE*      Plot-Pen-Style 'solid)
(defparam *CUTOFF-STYLE* Plot-Pen-Style 'short-dash)

(define thin (* 0.5 (line-width)))
(define thick (line-width))
(defparam *LNM-WIDTH*    Nonnegative-Real thin)
(defparam *N-WIDTH*      Nonnegative-Real thin)
(defparam *M-WIDTH*      Nonnegative-Real thin)
(defparam *CUTOFF-WIDTH* Nonnegative-Real thin)

(defparam *TICK-SIZE* Natural 4) ;; Dude IDK
(defparam *X-MINOR-TICKS* (U #f (Listof Real)) #f)
(defparam *X-NUM-TICKS* Natural 5)
(defparam *X-TICK-LINES?* Boolean #t)
(defparam *X-TICKS* (U #f (Listof Real)) '(1 2.5 3.5 4.25 5))
(defparam *Y-MINOR-TICKS* (U #f (Listof Real)) '(.25 .75))
(defparam *Y-NUM-TICKS* Natural 3)
(defparam *Y-TICK-LINES?* Boolean #t)
(defparam *Y-TICKS* (U #f (Listof Real)) #f)
(deftype Y-Style (U 'count '% 'X))
(defparam *Y-STYLE* Y-Style '%)
(defparam *Y-MAX* (U #f Real) #f)

;; --- Boolean flags
(defparam *AXIS-LABELS?*   Boolean #t) ;; If #t, label all plot axes
(defparam *HISTOGRAM?*     Boolean #f) ;; If #t, make a histogram instead of a line
(defparam *L-LABELS?*      Boolean #f) ;; If #t, label each plot column with its L-value
(defparam *LEGEND?*        Boolean #f) ;; If #t, make a legend for all plots
(defparam *LINE-LABELS?*   Boolean #t) ;; If #t, label all plot lines
(defparam *LOG-TRANSFORM?* Boolean #f)
(defparam *PDF?*           Boolean #f) ;; If #t, plot the Probabilistic Distribution Function
(defparam *SINGLE-PLOT?*   Boolean #t) ;; If #t, make 1 plot for each L
(defparam *GROUP-BY-TITLE?* Boolean #t)

(defparam *SHOW-PATHS?* Boolean #f)
;; If #t, make a path picture

;; ---
(defparam *OUTPUT* Path-String "./output.png") ;; Where to save output pict
(defparam *CACHE-PREFIX* String "./compiled/cache-lnm-")
(defparam *CACHE-TAG* (U #f String) #f)

(defparam *ERROR-BAR?* Boolean #f)
(defparam *ERROR-BAR-WIDTH* Nonnegative-Real (cast (/ (error-bar-width) 2) Nonnegative-Real))
(defparam *ERROR-BAR-LINE-WIDTH* Nonnegative-Real (cast (* 0.7 (error-bar-line-width)) Nonnegative-Real))

(defparam *DISCRETE?* Boolean #f)
(defparam *POINT-SIZE* Positive-Index 6)
(defparam *POINT-ALPHA* Nonnegative-Real 0.6)

(defparam *RECTANGLE-BORDER-WIDTH* Nonnegative-Real 1)
(defparam *RECTANGLE-WIDTH* Nonnegative-Real 2)
(defparam *RECTANGLE-SKIP* Nonnegative-Real 12)

(defparam *LEGEND-ANCHOR* (U 'top-right 'bottom-right) 'top-right)

(defparam *TOO-MANY-BYTES* Natural (expt 10 9))
;; Files larger than this should not be loaded

(deftype BarType (U 'overhead 'ratio 'runtime))

(defparam *TRACE-NUM-COLORS* Index 3)

(defparam *CONFIDENCE-LEVEL* Index 95)

(defparam *COLOR-OFFSET* Index 1)

(defparam *NUM-SIMPLE-RANDOM-SAMPLES* Index 2)
