#lang racket/base

(provide
  collect-rktd
)

(require
  glob
  racket/sequence
  benchmark-run/benchmark-data
  benchmark-run/parameters
  benchmark-run/utilities
  (only-in racket/list last append*)
  (only-in racket/string string-split)
)

;; =============================================================================

(define (filename->tag str)
  (car (string-split str "-")))

(define (collect-rktd dir #:delete? [delete? #t])
  (define tag (last (string-split dir "/")))
  (collect-rktd/filename (glob (format "~a-v~a-*.tmp.rktd" tag (*RACKET-VERSION*)))
                         #:delete? delete?))

(define (collect-rktd/filename fname* #:delete? [delete? #t])
  (define out (output-file (filename->tag (car fname*))))
  (with-output-to-file out #:exists 'replace
    (lambda ()
      (define port* (map (lambda (p) (open-input-file p)) fname*))
      (sequence-for-each
        (lambda v*
          (cond
           [(data-line? (car v*))
            (writeln (append* (map (compose1 cdr read-string) v*)))]
           [else
            (displayln (car v*))]))
        (apply in-parallel (map (lambda (p) (in-lines p)) port*)))))
  (when delete?
    (for-each delete-file fname*)))

(define (check-tags rktd*)
  (for/fold ([acc #f])
            ([rktd (in-list rktd*)])
    (define tag (filename->tag rktd))
    (when (and acc (not (string=? acc tag)))
      (raise-user-error 'gtp-collect "Datasets are for different benchmarks in: '~a'" rktd*))
    tag))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "gtp-collect"
   #:args rktd*
   (cond
    [(null? rktd*)
     (printf "Usage: gtp-collect RKTD* ...\n")]
    [(and (null? (cdr rktd*))
          (not (rktd-file? (car rktd*))))
     (collect-rktd (car rktd*))]
    [else
     (check-tags rktd*)
     (collect-rktd/filename rktd*)])))



