#lang racket/base

(require
  "tree-game.rkt"
  racket/port
  math/statistics
  plot/no-gui)

(define L 3)
(define H 4)
(define S 1)
(define NUM-TURNS 9) ;; time/space exponential in NUM-TURNS

(require contract-profile)
(contract-profile (time (main "3" NUM-TURNS)))

;(define time*
;  (parameterize ([current-output-port (open-output-nowhere)])
;    (for/list ([n (in-range L H S)])
;      (define-values (_out ms real gc) (time-apply main (list (number->string n))))
;      ms)))
;
;(displayln time*)
;(displayln (mean time*))
;(plot-file
;  (lines (for/list ([i (in-range L H S)] [t (in-list time*)]) (list i t)))
;  "output.png" 'png
;)
