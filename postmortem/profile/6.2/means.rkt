#lang racket/base

(require glob math/statistics racket/format)

(define NAME* (map symbol->string '(
;gregor
;kcfa
;lnm
;mbta
;morse-code
;quad
sieve
;snake
;suffixtree
;synth
;tetris
;zordoz
)))

(define rPCT (regexp "Running time is ([0-9]*\\.?[0-9]*)% contracts"))
(define (rnd n)
  (~r n #:precision '(= 2)))

(module+ main
  (printf "PROJECT\t\tMEAN\tSTD\n")
  (for ([n (in-list NAME*)])
    (define xs
      (for/list ([f (in-glob (format "./~a-*.txt" n))])
        (with-input-from-file f
          (lambda () (string->number (cadr (regexp-match rPCT (read-line))))))))
    (define m (mean xs))
    (define std (stddev/mean m xs))
    (printf "~a\t\t~a\t~a\n" n (rnd m) (rnd std))))
