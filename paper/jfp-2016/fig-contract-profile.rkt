#lang racket/base

(provide fig-contract-profile)

(require scribble/base)

;; =============================================================================

(define (fig-contract-profile)
  (elem "TODO"))

;(require racket/format)
;
;@(define (T->any) @racket[(-> T any/c)])
;@(define (any->T) @racket[(-> any/c T)])
;@(define (any->bool) @racket[(-> any/c boolean?)])
;
;@(define-syntax-rule 
;   (row x y z w ...)
;   @list[ @hspace[4]
;	  (tt (~a 'x)) (math (~a y)) (math (format "(~a)" (number->string z))) (math (~a 'w)) ... 
;	  @hspace[4]])
;
;@tabular[
; #:sep @hspace[2]
; #:row-properties '(bottom-border ())
; #:column-properties '(left left right)
;
;@list[
; @list[@hspace[4]
;	"Project"    "%C" "(S.E.)" "adaptor" "higher-order" "library" @T->any[]  @any->T[] @any->bool[] 
;       @hspace[4]]
; @row[ sieve          92    2.33         0           46       0         0        54           31]
; @row[ morse-code     29    6.80         0            0       0         0       100            0]
; @row[ mbta           39    3.65         0            0      65         0        65            0]
; @row[ zordoz         95    0.10         0           55      45         0        99           43]
; @row[ suffixtree     94    0.18        98           <1       0         2        94           18]
; @row[ lnm            81    0.73         0            9      99        91         0            0]
; @row[ kcfa           91    0.26       100            0       0         0        54           31]
; @row[ snake          98    0.21        93            0       0         1        99           49]
; @row[ tetris         96    0.35        89            0       0        11        89           44]
; @row[ synth          83    1.22        51           90       0        29        20            0]
; @row[ gregor         83    4.01        78            0       3         7        85           31]
; @row[ quad           80    0.96        <1            1       0         3        <1           <1]
;]
;]
;
;}
;
;
