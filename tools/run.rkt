#lang racket/base

;; Benchmark driver

(require "data-lattice.rkt"
         math/statistics
         mzlib/os
         pict
         racket/class
         racket/cmdline
         racket/date
         racket/draw
         racket/match
         racket/port
         racket/system
         racket/vector
         unstable/sequence)

;; Flag to set compilation-only mode. If #f it will compile and
;; run, otherwise only compiles.
(define only-compile? (make-parameter #f))

;; The number of times to run the benchmark as a string (to
;; be parsed later)
(define num-iterations (make-parameter "1"))

;; The number of jobs to spawn for the variations. When jobs is
;; greater than 1, the variation space is split evenly and allocated
;; to the various jobs.
(define num-jobs (make-parameter "1"))

;; Paths to write results/diagrams
(define output-path (make-parameter #f))
(define lattice-path (make-parameter #f))
(define entry-point-param (make-parameter #f))
(define exclusive-config (make-parameter #f))
(define min-max-config (make-parameter #f))

;; Get paths for all variation directories
;; Path Path -> Listof Path
(define (mk-variations basepath entry-point)
  (for/list ([file (in-list (directory-list (build-path basepath "benchmark")))]
             #:when (not (equal? "base" (path->string file))))
    (build-path basepath "benchmark" file entry-point)))

;; Run the variations for each variation directory
;; Optional argument gives the exact variation to run.
;; Default is to run all variations (either run all, or exactly one)
;; (Listof Path) Path Nat Nat [(U (Listof String) #f)] -> Results
(define (run-benchmarks basepath entry-point iters jobs
                        #:config [cfg #f]
                        #:min/max [min/max #f])
  (define variations
    (cond [cfg
           (list (build-path basepath "benchmark" (string-append "variation" cfg) entry-point))]
          [min/max
           (match-define (list min max) min/max)
           (define all-vars (mk-variations basepath entry-point))
           (define (in-range? var)
             (match-define (list _ bits) (regexp-match "variation([10]*)/" var))
             (define n (string->number bits 2))
             (and (>= n (string->number min))
                  (<= n (string->number max))))
           (filter in-range? all-vars)]
          [else (mk-variations basepath entry-point)]))
  (define results (make-vector (length variations)))
  ;; allocate a slice of the variations per job
  (define slice-size (ceiling (/ (length variations) jobs)))
  (define threads
    (for/list ([var-slice (in-slice slice-size (in-values-sequence (in-indexed (in-list variations))))]
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
             ;; first compile the variation
             (unless (system (format "taskset -c ~a raco make -v ~a"
                                     job# (path->string file)))
               (error (format "Compilation failed for '~a/~a', shutting down"
                              (current-directory) (path->string file))))

             ;; run an extra run of the variation to throw away in order to
             ;; avoid OS caching issues
             (unless (only-compile?)
               (printf "job#~a, throwaway build/run to avoid OS caching~n" job#)
               (match-define (list in out _ err control)
                 (process (format "taskset -c ~a racket ~a"
                                  job# (path->string file))))
               ;; make sure to block on this run
               (control 'wait)
               (close-input-port in)
               (close-input-port err)
               (close-output-port out))

             ;; run the iterations that will count for the data
             (unless (only-compile?)
               (for ([i (in-range iters)])
                 (printf "job#~a, iteration #~a of ~a started~n" job# i var)
                 (define command `(time (dynamic-require ,(path->string file) #f)))
                 (match-define (list in out pid err control)
                   (process (format "taskset -c ~a racket -e '~s'" job# command)
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
                 (close-output-port out))))

           ;; the order of runs doesn't really matter, but keep them in the order
           ;; they were run anyway just in case
           (vector-set! results var-idx (reverse times)))))))
  ;; synchronize on all jobs
  (for ([thd (in-list threads)]) (thread-wait thd))
  results)

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
                  #:multi
                  [("-i" "--iterations") n-i
                                         "The number of iterations to run"
                                         (num-iterations n-i)]
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
  (define iters (string->number (num-iterations)))
  (unless (number? iters)
    (raise-user-error (format "expected a number, given ~a" (num-iterations))))
  (define jobs (string->number (num-jobs)))
  (unless (number? jobs)
    (raise-user-error (format "expected a number, given ~a" (num-jobs))))

  ;; Set a default output path based on the "basepath" if one is not provided
  (unless (output-path)
    (date-display-format 'iso-8601)
    (output-path (string-append basepath
                                "-"
                                (date->string (current-date) #t)
                                ".rktd")))

  ;; Set the CPU affinity for this script to CPU0. Jobs spawned by this script run
  ;; using CPU1 and above.
  (system (format "taskset -pc 0 ~a" (getpid)))

  (define results (run-benchmarks basepath entry-point iters jobs
                                  #:config (exclusive-config)
                                  #:min/max (min-max-config)))

  (when (output-path)
    (with-output-to-file (output-path)
      (λ () (write results))
      #:mode 'text
      #:exists 'replace))

  ;; TODO: update lattice to account for empty-result startup time
  (when (lattice-path)
    (define averaged-results
      (vector-map (λ (times) (cons (mean times) (stddev times))) results))
    (send ;; default size is too small to see, so apply a scaling factor
          (pict->bitmap (scale (make-performance-lattice averaged-results) 3))
          save-file
          (lattice-path)
          'png)))
