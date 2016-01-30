#lang racket/base

;; Benchmark driver
;;
;; Usage:
;;   run.rkt BENCHMARK
;; This will:
;; - Create directories `BENCHMARK/benchmark` and `BENCHMARK/benchmark/base`
;; - Spawn jobs for each of the 2**n configurations
;;   Each job will create a new directory & save results to a file
;; - Aggregate the results from all sub-jobs

(require benchmark-util/data-lattice
         "stats-helpers.rkt"
         (only-in glob in-glob)
         (only-in racket/file file->value)
         (only-in racket/format ~a ~r)
         math/statistics
         mzlib/os
         pict
         pkg/lib
         racket/class
         racket/cmdline
         racket/date
         racket/draw
         (only-in racket/format ~a ~r)
         (only-in racket/file file->value)
         racket/future
         racket/match
         racket/list
         racket/port
         racket/string
         racket/system
         racket/vector
         unstable/sequence)

;; Flag to set compilation-only mode. If #f it will compile and
;; run, otherwise only compiles.
(define only-compile? (make-parameter #f))

;; Number of iterations to run the benchmark
;; - If `num-iterations` is given, run EXACTLY that
;; - By default, run `min-iterations` then check for non-normality.
;;   If non-normal, run more--until `max-iterations`
(define num-iterations (make-parameter #f))
(define min-iterations (make-parameter 10))
(define max-iterations (make-parameter 30))

;; The number of jobs to spawn for the configurations. When jobs is
;; greater than 1, the configuration space is split evenly and allocated
;; to the various jobs.
(define num-jobs (make-parameter "1"))

;; Paths to write results/diagrams
(define output-path (make-parameter #f))
(define lattice-path (make-parameter #f))
(define entry-point-param (make-parameter #f))
(define exclusive-config (make-parameter #f))
(define min-max-config (make-parameter #f))
(define *racket-bin* (make-parameter "")) ;; Path-String

;; Get paths for all configuration directories
;; Path Path -> Listof Path
(define (mk-configurations basepath entry-point)
  (define num-modules
    (for/sum ([fname (in-list (directory-list (build-path basepath "untyped")))]
              #:when (regexp-match? "\\.rkt$" (path->string fname))) 1))
  (for/list ([i (in-range (expt 2 num-modules))])
    (define bits
      (if (zero? i)
        (make-string num-modules #\0)
        (~r i #:base 2 #:min-width num-modules #:pad-string "0")))
    (define var-str (format "configuration~a" bits))
    (build-path basepath "benchmark" var-str entry-point)))

;; Run the configurations for each configuration directory
;; Optional argument gives the exact configuration to run.
;; Default is to run all configurations
;; (Listof Path) Path Nat Nat [(U (Listof String) #f)] -> Void
(define (run-benchmarks basepath entry-point jobs
                        #:config [cfg #f]
                        #:min/max [min/max #f])
  (define benchmark-dir (build-path basepath "benchmark"))
  (unless (directory-exists? benchmark-dir)
    (raise-user-error 'run (format "Directory '~a' does not exist, please run `setup.rkt ~a` and try again." benchmark-dir basepath)))
  (define configurations
    (cond
      [cfg
      (list (build-path benchmark-dir (string-append "configuration" cfg) entry-point))]
      [min/max
       (match-define (list min max) min/max)
       (define all-vars (mk-configurations basepath entry-point))
       (define (in-range? var)
         (match-define (list _ bits) (regexp-match "configuration([10]*)/" var))
         (define n (string->number bits 2))
         (and (>= n (string->number min))
              (<= n (string->number max))))
       (filter in-range? all-vars)]
      [else
       (mk-configurations basepath entry-point)]))
  ;; allocate a slice of the configurations per job
  (define slice-size (ceiling (/ (length configurations) jobs)))
  (define threads
    (for/list ([var-slice (in-slice slice-size (in-values-sequence (in-indexed (in-list configurations))))]
               [job# (in-range 1 (add1 jobs))])
      ;; Spawn a control thread for each job, which will in turn spawn an OS process.
      ;; Each job gets assigned to the CPU that is the same as the job# using
      ;; the `taskset` command.
      (thread
       (λ ()
         (for ([var-in-slice var-slice])
           (match-define (list var var-idx) var-in-slice)
           (define-values (new-cwd file _2) (split-path var))
           (define times null)
           (parameterize ([current-directory new-cwd])
             ;; first compile the configuration
             (unless (system (format "taskset -c ~a ~araco make -v ~a"
                                     job# (*racket-bin*) (path->string file)))
               (error (format "Compilation failed for '~a/~a', shutting down"
                              (current-directory) (path->string file))))

             ;; run an extra run of the configuration to throw away in order to
             ;; avoid OS caching issues
             (unless (only-compile?)
               (printf "job#~a, throwaway build/run to avoid OS caching~n" job#)
               (match-define (list in out _ err control)
                 (process (format "taskset -c ~a ~aracket ~a"
                                  job# (*racket-bin*) (path->string file))))
               ;; make sure to block on this run
               (control 'wait)
               (close-input-port in)
               (close-input-port err)
               (close-output-port out))

             ;; run the iterations that will count for the data
             (define exact-iters (num-iterations))
             (unless (only-compile?)
               (for ([i (in-range 0
                                  (or exact-iters (+ 1 (max-iterations)))
                                  1)]
                     ;; Stop early if user did NOT give an exact iterations
                     ;;  and Anderson-Darling does not reject null normality hypothesis
                     #:break (and (not exact-iters)
                                  (= i (min-iterations))
                                  (anderson-darling? times)))
                 (printf "job#~a, iteration #~a of ~a started~n" job# i var)
                 (define command `(time (dynamic-require ,(path->string file) #f)))
                 (match-define (list in out pid err control)
                   (process (format "taskset -c ~a ~aracket -e '~s'" job# (*racket-bin*) command)
                            #:set-pwd? #t))
                 ;; if this match fails, something went wrong since we put time in above
                 (define time-info
                   (for/or ([line (in-list (port->lines in))])
                     (regexp-match #rx"cpu time: (.*) real time: (.*) gc time: (.*)" line)))
                 (match time-info
                   [(list full (app string->number cpu)
                               (app string->number real)
                               (app string->number gc))
                    (printf "job#~a, iteration#~a - cpu: ~a real: ~a gc: ~a~n"
                            job# i cpu real gc)
                    (set! times (cons real times))]
                   [#f (void)])
                 ;; print anything we get on stderr so we can detect errors
                 (for-each displayln (port->lines err))
                 ;; block on the run, just in case
                 ;; (the blocking of `port->lines` should be enough)
                 (control 'wait)
                 ;; we're reponsible for closing these
                 (close-input-port in)
                 (close-input-port err)
                 (close-output-port out))
               (write-results (reverse times)))))))))
  ;; synchronize on all jobs
  (for ([thd (in-list threads)]) (thread-wait thd))
  (void))

(define (write-results times [dir (current-directory)])
  (with-output-to-file (build-path dir (output-path)) #:exists 'append
    (lambda ()
      (write times))))

;; Get the most recent commit hash for the chosen Racket install
(define (racket-checksum)
  (define rkt-dir
    (if (zero? (string-length (*racket-bin*)))
      ;; Hacks
      (string-append
        (with-output-to-string
          (lambda () (system "which racket")))
        "/..")
      (*racket-bin*)))
  (define str
    (parameterize ([current-directory rkt-dir])
      (with-output-to-string
        (lambda ()
          (system "git rev-parse HEAD")))))
  (~a str #:max-width 8))

;; Use the current `raco` to get the most-recent commit hash for typed-racket
(define (typed-racket-checksum)
  (define tbl (installed-pkg-table #:scope 'installation))
  (if tbl
    (let ([pkg (hash-ref tbl "typed-racket")])
      (if pkg
        (let ([chk (pkg-info-checksum pkg)])
          (~a chk #:max-width 8))
        (printf "Failed to find package 'typed-racket' in 'installation pkg-table\n")))
    (printf "Failed to get 'installed-pkg-table'\n")))

(module+ main
  (define basepath
    (command-line #:program "benchmark-runner"
                  #:once-any
                  [("-x" "--exclusive")    x-p
                                           "Run the given configuration and no others"
                                           (exclusive-config x-p)]
                  [("-m" "--min-max") min max
                                      "Run the configurations between min and max inclusive"
                                      (min-max-config (list min max))]
                  #:once-each
                  [("-c" "--only-compile") "Only compile and don't run"
                                           (only-compile? #t)]
                  [("-o" "--output") o-p
                                     "A path to write data to"
                                     (output-path o-p)]
                  [("-l" "--lattice") l-p
                                     "A path to write the lattice diagram to"
                                     (lattice-path l-p)]
                  [("-e" "--entry-point") e-p
                                          "The main file to execute. (Defaults to 'main.rkt'.)"
                                          (entry-point-param e-p)]
                  [("-j" "--jobs") j
                                   "The number of processes to spawn"
                                   (num-jobs j)]
                  [("-r" "--racket") r-p
                                     "Directory containing the preferred racket & raco executables"
                                     (*racket-bin* (string-append r-p "/"))]

                  #:multi
                  [("-i" "--iterations") n-i
                                         "The number of iterations to run"
                                         (let ([n (string->number n-i)])
                                           (unless n (raise-user-error 'run (format "Expected natural number, got '~a'" n-i)))
                                           (num-iterations n))]
                  #:args (basepath)
                  basepath))
  ;; Validate given entry-point, or fall back to default
  (define entry-point
    (cond [(and (entry-point-param)
                (path-string? (entry-point-param))
                (regexp-match? #rx"\\.rkt$" (entry-point-param)))
           (entry-point-param)]
          [(entry-point-param) ;; Optional argument given, but is not a .rkt file
           (raise-user-error (format "expected a Racket file, given ~a" (entry-point-param)))]
          [else ;; Default
           "main.rkt"]))
  ;; Assert that the parsed entry point exists in the project
  (unless (and (file-exists? (build-path basepath "untyped" entry-point))
               (file-exists? (build-path basepath "typed" entry-point)))
    (raise-user-error (format "entry point '~a' not found in project '~a', cannot run" entry-point basepath)))
  (unless (path-string? basepath)
    (raise-user-error (format "expected a path, given ~a" basepath)))
  (unless (and (path-string? entry-point)
               (regexp-match? #rx"\\.rkt$" entry-point))
    (raise-user-error (format "expected a Racket file, given ~a" entry-point)))
  (define jobs (string->number (num-jobs)))
  (unless (number? jobs)
    (raise-user-error (format "expected a number, given ~a" (num-jobs))))

  ;; Set a default output path based on the "basepath" if one is not provided
  (unless (output-path)
    (date-display-format 'iso-8601)
    (output-path (string-append (last (string-split basepath "/"))
                                "-"
                                (date->string (current-date) #t)
                                ".rktd")))

  ;; Need at least 2 CPUs since our controller thread is pinned to core 0 and workers
  ;; to cores 1 and above.
  (when (< (processor-count) 2)
    (raise-user-error
      (string-append "Detected less than 2 CPUs. Please run this "
                     "script on a machine/VM with at least 2 CPUs.")))
  (when (< (processor-count) (add1 jobs))
    (raise-user-error
      (string-append "Too many jobs specified. Need at least as many "
                     "processors as #jobs+1")))

  ;; Set the CPU affinity for this script to CPU0. Jobs spawned by this script run
  ;; using CPU1 and above.
  (system (format "taskset -pc 0 ~a" (getpid)))

  (run-benchmarks basepath entry-point jobs
                  #:config (exclusive-config)
                  #:min/max (min-max-config))

  (when (and (not (only-compile?)) (output-path))
    (with-output-to-file (output-path)
      (λ ()
        ;; first write a comment that encodes the commandline args / version
        (printf ";; ~a~n" (current-command-line-arguments))
        (printf ";; ~a~n" (version))
        (printf ";; base @ ~a~n" (racket-checksum))
        (printf ";; typed-racket @ ~a~n" (typed-racket-checksum))
        (displayln "#(")
        (for ([result-file (in-glob (format "~a/benchmark/configuration*/~a"
                                     basepath
                                     (output-path)))])
          (with-input-from-file result-file
            (lambda () (for ([ln (in-lines)]) (displayln ln)))))
        (displayln ")"))
      #:mode 'text
      #:exists 'replace))

  ;; TODO: update lattice to account for empty-result startup time
  (when (lattice-path)
    (define averaged-results
      (vector-map (λ (times) (cons (mean times) (stddev times))) (file->value (output-path))))
    (send ;; default size is too small to see, so apply a scaling factor
          (pict->bitmap (scale (make-performance-lattice averaged-results) 3))
          save-file
          (lattice-path)
          'png)))
