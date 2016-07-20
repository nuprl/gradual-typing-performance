#lang racket/base

(provide
  ;; (define-type Version (U 6.2 6.3 6.4 6.5))
  ;; (define-type Project String)

  collect-benchmark
  ;; (->* [Version Project] [#:output-file (U #f Path-String)] Void)
  ;; Print all data for one benchmark `B` to a file
  ;; By default, output goes to `B.rktd`

  collect-all-benchmarks
  ;; (->* [Version] [#:output-directory (U #f Path-String)] Void)
  ;; Print all data for all benchmarks to a given directory.
  ;; Filenames are automatic -- one for each benchmark.

)

(require
  glob
  racket/sequence
  (only-in racket/path file-name-from-path)
  (only-in racket/port with-output-to-string)
  (only-in racket/system system)
  (only-in racket/list last append*)
  (only-in racket/string string-split string-trim string-prefix?)
)

;; =============================================================================

(define DEFAULT_DATA_DIR "./data")

(define (infer-output-file bm)
  (path-replace-extension
    (build-path (current-directory) bm)
    ".rktd"))

;(: get-git-root (-> String))
(define (get-git-root)
  (define ok? (box #t))
  (define outs
    (with-output-to-string
      (lambda ()
        (set-box! ok? (system "git rev-parse --show-toplevel")))))
  (if (unbox ok?)
    (string-trim outs)
    (raise-user-error 'modulegraph "Must be in `gradual-typing-performance` repo to use script")))

(define (infer-project-dir name)
  (define p-dir (build-path (get-git-root) "benchmarks" name))
  (if (directory-exists? p-dir)
    p-dir
    #f))

(define rx-DATA #rx"::: cpu time: ([0-9])+ ")

(define (STERN-WARNING str . arg*)
  (fprintf (current-error-port) "WARNING: ")
  (apply fprintf (current-error-port) str arg*)
  (fprintf (current-error-port) "\n"))

(define (collect-benchmark rkt-version bm #:output-file [of #f])
  (define output-file (or of (infer-output-file bm)))
  (with-output-to-file output-file #:exists 'replace
    (lambda ()
      (printf "#(\n")
      (define project-dir
        (or (infer-project-dir bm)
            (raise-user-error 'collect "Failed to find project directory for '~a', cannot summarize data" bm)))
      (define config-glob
        (format "~a/benchmark/config*/output-~a.txt" project-dir rkt-version))
      (for ((file (in-glob config-glob)))
        (with-input-from-file file
          (lambda ()
            (printf "(")
            (for ((ln (in-lines)))
              (define m (regexp-match rx-DATA ln))
              (if m
                (displayln (cadr m))
                (STERN-WARNING "malformed data: ~a" ln)))
            (printf ")\n"))))
      (printf ")\n"))))

(define (collect-all-benchmarks rkt-version #:output-directory [od #f])
  (define output-directory (or od DEFAULT_DATA_DIR))
  (define bm-glob
    (format "~a/benchmarks/*" (get-git-root)))
  (for ((bm-path (in-glob bm-glob))
        #:when (directory-exists? bm-path))
    (define bm (path->string (file-name-from-path bm-path)))
    (define of (build-path output-directory (string-append bm ".rktd")))
    (collect-benchmark rkt-version bm #:output-file of)))

;; =============================================================================

(module+ main
  (require racket/cmdline glob)
  (define *benchmark* (make-parameter #f))
  (define *output-file* (make-parameter #f))
  (define *output-directory* (make-parameter #f))
  (command-line
   #:program "gtp-collect"
   #:once-each
   [("-b" "--benchmark")
    bm
    "Collect for a single benchmark"
    (*benchmark* bm)]
   [("-f" "--output-file")
     of
     "Choose output file (needs -b)"
     (*output-file* of)]
   [("-d" "--output-directory")
     od
     "Choose output directory (incompatible with -b)"
     (*output-directory* od)]
   #:args (VERSION)
   (define-values (bm of od)
     (values (*benchmark*) (*output-file*) (*output-directory*)))
   (cond
    [(and bm od)
     (raise-user-error 'gtp-collect "Incompatible flags -b and -d (will not output data for a single file to a directory)")]
    [(and (not bm) of)
     (raise-user-error 'gtp-collect "Cannot use flag -f without flag -b (more than one data file is output)")]
    [bm
     (collect-benchmark VERSION bm #:output-file of)]
    [else
     (collect-all-benchmarks VERSION #:output-directory od)])))

;; =============================================================================

(module+ test
  (require rackunit rackunit-abbrevs)

  (check-apply* infer-output-file
   ["yolo"
    ==> (build-path (current-directory) "yolo.rktd")])

  (check-apply* infer-project-dir
   ["sieve"
    ==> (build-path (get-git-root) "benchmarks/sieve")]
   ["silly"
    ==> #f])

  (parameterize ([current-error-port (open-output-string)])
    (STERN-WARNING "hola ~a" 'world)
    (check-equal? (get-output-string (current-error-port))
                  "WARNING: hola world\n"))

)
