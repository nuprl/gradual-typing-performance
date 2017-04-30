#lang racket/base

;; Print a .rktd data file in a .csv format for import to R
;;
;; Usage:
;;   racket dataframe.rkt FILE.rktd
;; Prints result to FILE.csv

;; TODO integrate with summarize script

(require
  racket/string
  racket/list
  racket/file
  gtp-summarize/bitstring
)

;; =============================================================================

(define (path->filename p)
  (define str (if (path? p) (path->string p) p))
  (car (string-split (last (string-split str "/")) ".")))

(module+ main
  (require racket/cmdline)
  (command-line
   #:args (rktd)
   (define v (file->value rktd))
   (define num-cfgs (vector-length v))
   (define num-mods (log2 num-cfgs))
   (define (nat->bits i)
     (natural->bitstring i #:pad num-mods))
   (with-output-to-file (format "~a.csv" (path->filename rktd))
     (lambda ()
       (displayln (string-join (cons "time" (for/list ((i (in-range num-mods))) (format "t~a" i)))  ","))
       (for ((row (in-vector v))
             (i (in-naturals)))
         (define cfg (string->list (nat->bits i)))
         (for ((data (in-list row)))
           (displayln
             (string-join (cons (number->string data)
                                (for/list ((c (in-list cfg))) (string c))) ","))))))))
