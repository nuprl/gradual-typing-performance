#lang racket/base

;; Create colored versions of a few module graphs
;; Should be run from the root of the `gradual-typing-performance` folder

(require
  glob
  (only-in racket/system system)
  (only-in racket/list last)
  (only-in racket/string string-split)
)

(define all-folders '(
  "echo"
  "sieve"
  "morse-code"
  "mbta"
  "zordoz"
  "suffixtree"
  "lnm"
  "kcfa"
  "snake"
  "tetris"
  "funkytown"
  "gregor"
  "quad"
))

(define (strip-path str)
  (last (string-split str "/")))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "list-filenames"
    #:args DIR*
    (begin
      (define dir* (if (eq? '() DIR*) all-folders DIR*))
      (for ([d (in-list dir*)])
        (printf "# ~a\n" d)
        (for ([fname (in-glob (format "~a/untyped/*.rkt" d))]
              [n (in-naturals)])
          (printf "  ~a. ~a\n" n (strip-path fname)))
        (newline)))))

