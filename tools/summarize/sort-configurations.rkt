#lang racket/base

;; Sort all configurations in a dataset by their running time, from worst to best.
;;
;; Usage: racket sort-configurations.rkt FILE.rktd

(require
  gtp-summarize/summary
  (only-in racket/format ~r)
)

;; =============================================================================

(define (my-round n)
  (~r n #:precision '(= 2)))

(module+ main
  (require racket/cmdline)
  (define *output-dir* (make-parameter "."))
  (command-line
   #:program "sort-configurations"
   #:once-each
   [("-o" "--output") o-p "Directory to save results in (default is current directory)" (*output-dir* o-p)]
   #:args ARG*
  (for ([pn (in-list ARG*)])
    (define S (from-rktd pn))
    (define baseline (untyped-mean S))
    (define var+mean+std* (for/list ([v (all-configurations S)])
                        (list v (configuration->mean-runtime S v) (configuration->stddev S v))))
    (define name (get-project-name S))
    (define out-file (format "~a/~a-worst.rktd" (*output-dir*) name))
    (with-output-to-file out-file #:exists 'replace
      (lambda ()
        (printf ";; ~a\n;; ~a\n;;   Config.   Avg.Runtime (ms)    Stddev    Overhead (X worse than untyped)\n" name (make-string (string-length name) #\=))
        (displayln "(")
        (for ([v+r (in-list (sort var+mean+std* > #:key cadr))])
          (printf "(~a   ~a   ~a   ~a)\n" (car v+r) (my-round (cadr v+r)) (my-round (caddr v+r)) (my-round (/ (cadr v+r) baseline))))
        (displayln ")")))
    (printf "Saved results for '~a' to file '~a'\n" pn out-file))))
