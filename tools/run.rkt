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

(provide
  run-benchmark
  ;; Same as calling from the command line, just pass argv vector
)

(require benchmark-util/data-lattice
         ffi/unsafe/atomic
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

(require (prefix-in mg: gtp-summarize/modulegraph)
         (prefix-in p:  gtp-summarize/predict))

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
(define predict? (make-parameter #f))
(define *racket-bin* (make-parameter "")) ;; Path-String
(define *AFFINITY?* (make-parameter #t)) ;; Boolean
(define *ERROR-MESSAGES* (make-parameter '())) ;; (Listof String)

(define-syntax-rule (compile-error path)
  (let ([message (format "Compilation failed in '~a/~a'" (current-directory) path)])
    (*ERROR-MESSAGES* (cons message (*ERROR-MESSAGES*)))
    (error 'run:compile message)))

(define-syntax-rule (runtime-error var var-idx)
  (let ([message (format "Error running configuration ~a in '~a'" var-idx var)])
    (*ERROR-MESSAGES* (cons message (*ERROR-MESSAGES*)))
    (error 'run:runtime message)))

;; Get paths for all configuration directories
;; ModuleGraph #:such-that (Nat -> Bool) -> Listof Path
(define (mk-configurations mg #:such-that [include? (λ (x) #t)])
  (define num-modules (length (mg:module-names mg)))
  (for/list ([i     (in-range (expt 2 num-modules))]
             #:when (include? i))
    (define bits
      (if (zero? i)
          (make-string num-modules #\0)
          (~r i #:base 2 #:min-width num-modules #:pad-string "0")))
    (format "configuration~a" bits)))

(define (mk-prediction-configurations mg)
  (append
   (apply append
    (for*/list ([requirer (in-list (mg:module-names mg))]
                [requiree (in-list (mg:requires mg requirer))])
      (list (format "edge-u~a-t~a" requirer requiree)
            (format "edge-t~a-u~a" requirer requiree))))
   (for/list ([m (in-list (mg:module-names mg))])
     (format "optimize-~a" m))))

;; Run the configurations for each supplied configuration directory
(define (run-benchmarks basepath
                        dir-names
                        entry-point
                        jobs)
  (define benchmark-dir (build-path basepath "benchmark"))
  (unless (directory-exists? benchmark-dir)
    (raise-user-error 'run (format "Directory '~a' does not exist, please run `setup.rkt ~a` and try again." benchmark-dir basepath)))
  (define configurations
    (for/list ([dir (in-list dir-names)])
      (build-path benchmark-dir dir entry-point)))
  (run-benchmark-dirs configurations jobs))

(define (run-benchmark-dirs configurations jobs)
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
             (define compile-ok?
               (system (format "~a~araco make -v ~a"
                         (if (*AFFINITY?*) (format "taskset -c ~a " job#) "")
                         (*racket-bin*)
                         (path->string file))))

             (unless compile-ok?
               (compile-error (path->string file)))

             ;; run an extra run of the configuration to throw away in order to
             ;; avoid OS caching issues
             (unless (only-compile?)
               (printf "job#~a, throwaway build/run to avoid OS caching~n" job#)
               (match-define (list in out _ err control)
                 (process (format "~a~aracket ~a"
                            (if (*AFFINITY?*) (format "taskset -c ~a " job#) "")
                            (*racket-bin*)
                            (path->string file))))
               ;; make sure to block on this run
               (control 'wait)
               (close-input-port in)
               (close-input-port err)
               (close-output-port out))

             ;; run the iterations that will count for the data
             (define exact-iters (num-iterations))
             (unless (and (only-compile?) compile-ok?)
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
                   (call-as-atomic
                    (λ ()
                     (process (format "~a~aracket -e '~s'"
                                      (if (*AFFINITY?*) (format "taskset -c ~a " job#) "")
                                      (*racket-bin*)
                                      command)
                              #:set-pwd? #t))))
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
                   [#f (runtime-error var var-idx)])
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
  #t)

(define (write-results times [dir (current-directory)])
  (with-output-to-file (build-path dir (output-path)) #:exists 'replace
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
  (define success? (box #f))
  (define str
    (parameterize ([current-directory rkt-dir])
      (with-output-to-string
        (lambda ()
          (when (directory-exists? "./git")
            (and (system "git rev-parse HEAD")
                 (set-box! success? #t)))))))
  ;; If we parsed the git version, print it. Otherwise, notify.
  (if (unbox success?)
      (~a str #:max-width 8)
      "<unknown-commit>"))

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

(define (print-data-preamble)
  ;; first write a comment that encodes the commandline args / version
  (printf ";; ~a~n" (current-command-line-arguments))
  (printf ";; ~a~n" (version))
  (printf ";; base @ ~a~n" (racket-checksum))
  (printf ";; typed-racket @ ~a~n" (typed-racket-checksum)))

(define (run-benchmark vec)
  (define basepath
    (command-line #:program "benchmark-runner"
                  #:argv vec
                  #:once-any
                  [("-x" "--exclusive")    x-p
                   "Run the given configuration and no others"
                   (exclusive-config x-p)]
                  [("-m" "--min-max") min max
                   "Run the configurations between min and max inclusive"
                   (min-max-config (list min max))]
                  [("-p" "--predict") "Run the prediction configurations instead of normal configurations"
                   (predict? #t)]
                  #:once-each
                  [("-n" "--no-affinity")
                   "Do NOT set task affinity (runs all jobs on current core)"
                   (*AFFINITY?* #f)]
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
  (unless (and (directory-exists? basepath)
               (file-exists? (build-path basepath "untyped" entry-point))
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
  (when (*AFFINITY?*)
    (system (format "taskset -pc 0 ~a" (getpid))))
  (define-values (_a bp _b) (split-path basepath))
  (define mg (mg:project-name->modulegraph (path->string bp)))
  ;; produce the dir-names to run
  ;; and a thunk to collect all the data afterwards
  (match-define (list dir-names post-process)
    (cond [(predict?)
           (define configs (p:prediction-configs mg))
           (define dirs (map p:prediction-config->name configs))
           (define (collect)
             (define data (for/hash ([config (in-list configs)])
                (define data
                  (with-input-from-file
                    (build-path basepath "benchmark" (p:prediction-config->name config) (output-path))
                    (λ () (read))))
                (values config data)))
             (with-output-to-file (output-path)
               (λ ()
                 (print-data-preamble)
                 (write data))))
           (list dirs collect)]
          [else
           (define dirs
             (cond [(exclusive-config)
                    (list (string-append "configuration" (exclusive-config)))]
                   [(min-max-config)
                    (match-define (list min max) (map string->number (min-max-config)))
                    (mk-configurations mg
                                       #:such-that (λ (n)
                                                     (and (>= n min)
                                                          (<= n max))))]
                   [else (mk-configurations mg)]))
           (define (collect)
             (with-output-to-file (output-path)
                 (λ ()
                   (print-data-preamble)
                   (displayln "#(")
                   (for ([result-file (in-glob (format "~a/benchmark/configuration*/~a"
                                                       basepath
                                                       (output-path)))])
                     (with-input-from-file result-file
                       (lambda () (for ([ln (in-lines)]) (displayln ln)))))
                   
                   (displayln ")"))
                 #:mode 'text
                 #:exists 'replace))
           (list dirs collect)]))

  (run-benchmarks basepath dir-names entry-point jobs)
  (when (and (not (only-compile?)) (output-path))
    (post-process))
  
  

  ;; TODO: update lattice to account for empty-result startup time
  (when (lattice-path)
    (define averaged-results
      (vector-map (λ (times) (cons (mean times) (stddev times))) (file->value (output-path))))
    (send ;; default size is too small to see, so apply a scaling factor
          (pict->bitmap (scale (make-performance-lattice averaged-results) 3))
          save-file
          (lattice-path)
          'png))
  (void))

;; =============================================================================

(module+ main
  (run-benchmark (current-command-line-arguments)))
