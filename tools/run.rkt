#lang racket/base
(date-display-format 'iso-8601)

;; TODO 
;; - add option to change governor, default to something reasonable

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

(require benchmark-run/unixtime
         "stats-helpers.rkt"
         (only-in glob in-glob)
         (only-in racket/file file->value)
         (only-in racket/format ~a ~r)
         math/statistics
         mzlib/os
         glob
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

;; # warmup iterations to run. Throw these numbers away
(define *NUM-WARMUP* (make-parameter 1))

;; Processes that finish quicker than *MIN-MILLISECONDS*
;;  are very likely to be affected by OS effects
(define *MIN-MILLISECONDS* (make-parameter (* 1.3 1000)))

;; Apply transformation to the list of configurations before running
;; TODO should be a sequence
(define *PERMUTE* (make-parameter values))

;; The number of jobs to spawn for the configurations. When jobs is
;; greater than 1, the configuration space is split evenly and allocated
;; to the various jobs.
(define num-jobs (make-parameter "1"))

;; Paths to write results/diagrams
(define output-path (make-parameter #f))
(define entry-point-param (make-parameter #f))
(define exclusive-config (make-parameter #f))
(define min-max-config (make-parameter #f))
(define *racket-bin* (make-parameter "")) ;; Path-String
(define *AFFINITY?* (make-parameter #t)) ;; Boolean
(define *ERROR-MESSAGES* (make-parameter '())) ;; (Listof String)
(define *DATA-TMPFILE* (make-parameter #f))
(define *TIME-TMPFILE* (make-parameter "time.tmp"))
(define *TEMPERATURE-FREQUENCY* (make-parameter 1))

(define-syntax-rule (compile-error path)
  (let ([message (format "Compilation failed in '~a/~a'" (current-directory) path)])
    (*ERROR-MESSAGES* (cons message (*ERROR-MESSAGES*)))
    (error 'run:compile message)))

(define-syntax-rule (runtime-error var var-idx)
  (let ([message (format "Error running configuration ~a in '~a'" var-idx var)])
    (*ERROR-MESSAGES* (cons message (*ERROR-MESSAGES*)))
    (error 'run:runtime message)))

(define (timestamp)
  (date->string (current-date) #t))

(define (time? t)
  (or (unixtime? t) (real? t)))

(define (time*->min t*)
  (if (unixtime? (car t*))
    (unixtime*->min t*)
    (apply min t*)))

(define (time->real t)
  (if (unixtime? t)
    (unixtime-real t)
    t))

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

;; Start a thread to monitor CPU temperature
;; Delay before returning
(define (make-temperature-monitor)
  (define t-file (string-append (output-path) ".heat"))
  (and (check-system-command "sensors")
       (printf "### Monitoring temperature at '~a'\n" t-file)
       (let ([p (process
                  (string-append
                    (format "echo '(~a ' > " (timestamp)) t-file "; "
                    "sensors -u >> " t-file "; "
                    "echo ')' >> " t-file "; "
                    "while true; do "
                      (format "sleep ~a; " (*TEMPERATURE-FREQUENCY*))
                      (format "echo '(~a ' >> " (timestamp)) t-file "; "
                      "sensors -u >> " t-file "; "
                      "echo ') ' >> " t-file "; "
                    "done"))])
         (sleep 2) ;; For temperature readings to stabilize
         p)))

(define (system-command-exists? cmd-str)
  (if (system (format "hash ~a &> /dev/null" cmd-str)) #t #f))

(define (check-system-command cmd-str)
  (unless (system-command-exists? cmd-str)
    (printf "WARNING: `sensors` command not found, cannot monitor temperature\n")
    #f))

(define (system-command-fallback . cmd-str*)
  (or (for/or ([cmd (in-list cmd-str*)]
               #:when (system-command-exists? cmd))
        cmd)
      (raise-user-error 'run "sys command fallback failed ~a" cmd-str*)))

(define (kill-temperature-monitor TM)
  (when TM
    (match-define (list out in pid err control) TM)
    (control 'kill)
    (control 'wait)
    (for-each displayln (port->lines err))
    (close-output-port in)
    (close-input-port out)
    (close-input-port err))
  (void))

(define ((make-run-command/racket job#) stub #:iters [iters 1] #:stat? [stat? #f])
  (define cmd (string-append
                (if (*AFFINITY?*) (format "taskset -c ~a " job#) "")
                " " stub))
  (for/list ([i (in-range iters)])
    (match-define (list out in pid err control) (process cmd #:set-pwd? #t))
    (control 'wait)
    (for-each displayln (port->lines err))
    (define t
      (let ([time-info ;; 2016-05-10: can also use for/first
             (for/last ([line (in-list (port->lines out))])
              (regexp-match #rx"cpu time: (.*) real time: (.*) gc time: (.*)" line))])
        (match time-info
         [(list full
             (app string->number cpu)
             (app string->number real)
             (app string->number gc))
          (printf "job#~a, iteration#~a - cpu: ~a real: ~a gc: ~a~n" job# i cpu real gc)
          real]
         [#f
          (when (regexp-match? "racket " stub)
            (runtime-error 'run stub))])))
    (close-input-port out)
    (close-input-port err)
    (close-output-port in)
    t))

;; (-> Natural (-> String #:iters Natural #:stat? Boolean (Listof unixtime)))
(define ((make-run-command/gnutime job#) stub #:iters [iters 1] #:stat? [stat? #f])
  (define time-cmd (system-command-fallback "gtime" "/usr/bin/time"))
  (define time-tmpfile (*TIME-TMPFILE*))
  (when (file-exists? time-tmpfile)
    (delete-file time-tmpfile))
  (define cmd
    (string-append
      (format " for i in `seq 1 ~a`; do " iters)
      (if (*AFFINITY?*) (format "taskset -c ~a " job#) "")
      time-cmd
      " -o "
      time-tmpfile
      " --append "
      " -f "
      TIME-FMT
      " "
      stub
      "; done;"))
  (printf "#### exec `~a` in `~a`\n" stub (current-directory))
  (match-define (list out in pid err control) (process cmd #:set-pwd? #t))
  (control 'wait)
  (for-each displayln (port->lines err))
  (define ut*
    (with-input-from-file time-tmpfile (lambda ()
      (for/fold ([acc '()])
                ([ln (in-lines)])
        (cond
         [(string->unixtime ln)
          => (lambda (ut)
            (if (zero? (unixtime-exit ut))
              (cons ut acc)
              (raise-user-error 'run-command
                "Process terminated with non-zero exit code '~a'. Full command was '~a'"
                (unixtime-exit ut) cmd)))]
         [(string->number ln)
          => (lambda (n) (cons n acc))]
         [else acc])))))
  (close-input-port out)
  (close-input-port err)
  (close-output-port in)
  ut*)

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
       ((*PERMUTE*)
        (mk-configurations basepath entry-point))]))
  (define hot-proc (make-temperature-monitor))
  ;; allocate a slice of the configurations per job
  (define slice-size (ceiling (/ (length configurations) jobs)))
  (define thread*
    (for/list ([var-slice (in-slice slice-size (in-values-sequence (in-indexed (in-list configurations))))]
               [job# (in-range 1 (add1 jobs))])
      ;; Spawn a control thread for each job, which will in turn spawn an OS process.
      ;; Each job gets assigned to the CPU that is the same as the job# using
      ;; the `taskset` command.
      (thread
       (λ ()
         (define run-command (make-run-command/racket job#))
         (for ([var-in-slice var-slice])
           (match-define (list var var-idx) var-in-slice)
           (define-values (new-cwd file _2) (split-path var))
           (define file-str (path->string file))
           (parameterize ([current-directory new-cwd])
             ;; first compile the configuration
             (void (run-command (format "~araco make -v ~a" (*racket-bin*) file-str)))
             ;; run the iterations that will count for the data
             (define exact-iters (num-iterations))
             (unless (or (only-compile?)
                         (file-exists? (*DATA-TMPFILE*)))
               (define ut*
                 (let* (;[rkt-command (format "~aracket ~a" (*racket-bin*) file-str)]
                        [rkt-command (format "~aracket -e '~s'"
                                       (*racket-bin*)
                                       `(time (dynamic-require ,file-str #f)))]
                        ;; -- throwaway builds (use to get baseline time)
                        [time0 (time*->min (run-command #:iters (*NUM-WARMUP*) rkt-command))]
                        [use-stat? (< time0 (*MIN-MILLISECONDS*))]
                        [run  (lambda (i)
                                (run-command rkt-command #:iters i #:stat? use-stat?))]
                        ;; -- real thing
                        [ut0* (run (or exact-iters (min-iterations)))]
                        [ut1* (if (or exact-iters
                                      (anderson-darling? (map time->real ut0*)))
                                    '()
                                    (run (- (+ 1 (max-iterations) (min-iterations)))))])
                   (append ut0* ut1*)))
               (write-results (reverse ut*)))))))))
  ;; synchronize on all jobs
  (for ([thd (in-list thread*)]) (thread-wait thd))
  (kill-temperature-monitor hot-proc)
  #t)

(define (write-results times [dir (current-directory)])
  (with-output-to-file (build-path dir (*DATA-TMPFILE*)) #:exists 'append
    (lambda ()
      (display "(") (writeln (timestamp))
      (for-each writeln times)
      (displayln ")"))))

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

;; Read a string, return a permutation function on lists
;; (-> String (All (A) (-> (Listof A) (Listof A))))
(define (read-permutation str)
  (cond
   [(string=? str "reverse")
    reverse]
   [(string=? str "shuffle")
    shuffle]
   [(string->number str)
    => rotate]
   [else
    values]))

;; Rotate a list
(define ((rotate i) x*)
  (let-values (((a b) (split-at x* (modulo i (length x*)))))
    (append b a)))

;; Read a .rktd file which SHOULD contain 1 list of runtimes.
;; Raise an error if there's more than 1 list or the data is malformed.
(define (result-file->time* fname)
  (with-handlers ([exn:fail? (lambda (e) (raise-user-error 'run "Error collecting data from file '~a', please inspect & try again.\n~a" fname (exn-message e)))])
    (define v (file->value fname))
    (unless (and (list? v) (not (null? (cdr v)))
                 (for/and ([x (in-list (cdr v))])
                   (time? x)))
      (raise-user-error 'run "Malformed tmp data ~a" v))
    (map time->real (cdr v))))

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
                  #:once-each
                  [("-w" "--warmup")
                   w
                   "Set number of warmup iterations"
                   (*NUM-WARMUP* (string->number w))]
                  [("-p" "--permute")
                   p
                   "Change order of configurations"
                   (*PERMUTE* (read-permutation p))]
                  [("-n" "--no-affinity")
                   "Do NOT set task affinity (runs all jobs on current core)"
                   (*AFFINITY?* #f)]
                  [("-c" "--only-compile") "Only compile and don't run"
                                           (only-compile? #t)]
                  [("-o" "--output") o-p
                                     "A path to write data to"
                                     (output-path o-p)]
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
  ;; Also set *DATA-TMPFILE*
  (cond
   [(output-path)
    (*DATA-TMPFILE* (output-path))]
   [else
    (date-display-format 'iso-8601)
    (define tag (last (string-split basepath "/")))
    (output-path (string-append tag "-" (timestamp) ".rktd"))
    (*DATA-TMPFILE* (string-append tag ".rktd"))])

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
        (printf ";; ~a~n" (timestamp))
        (displayln "#(")
        (for ([result-file (in-glob (format "~a/benchmark/configuration*/~a"
                                     basepath
                                     (*DATA-TMPFILE*)))])
          (writeln (file->value result-file)))
        (displayln ")"))
      #:mode 'text
      #:exists 'replace))
  (printf "### saved results to '~a'\n" (output-path))
  (void))

;; =============================================================================

(module+ main
  (run-benchmark (current-command-line-arguments)))
