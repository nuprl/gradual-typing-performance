#lang racket/base

;; TODO
;; - manage "ALL" benchmarks

(require
  glob
  benchmark-run/run
  benchmark-run/setup
  benchmark-run/benchmark-data
  benchmark-run/collect
  benchmark-run/parameters
  benchmark-run/utilities
  (only-in version/utils valid-version?)
  (only-in racket/string string-split)
)

;; =============================================================================

(define *RACKET-VERSIONS* (make-parameter (list "6.5")))

(define (racket-bin)
  (define RKTV (*RACKET-VERSION*))
  (define prefix
    (if (absolute-path? RKTV)
      RKTV
      (build-path (find-system-path 'home-dir) "code" "racket" RKTV)))
  (let ([try-path-1 (build-path prefix "bin")])
    (if (has-racket-binaries? try-path-1)
      (string-append (path->string try-path-1) "/")
      (let ([try-path-2 (build-path prefix "racket" "bin")])
        (if (has-racket-binaries? try-path-2)
          (string-append (path->string try-path-2) "/")
          (raise-user-error 'racket-bin "Could not locate Racket v~a binaries in '~a'" (*RACKET-VERSION*) prefix))))))

;; Call `run.rkt` with the right parameters
(define (run fname benchmark)
  (setup-benchmark benchmark)
  (run-benchmark (vector
                         "-o" fname
                         "-i" (number->string 1)
                         "-p" "shuffle"
                         benchmark)))

(define (run-round dir)
  (unless (refresh-benchmark dir)
    (setup-benchmark dir))
  (define fname (output-file dir #:tmp? #t))
  (run fname dir))

(define (run-all-rounds bm-dir* outer-iters)
  (for ([i (in-range outer-iters)])
    (info "Starting round ~a\n" i)
    (for ([version (in-list (*RACKET-VERSIONS*))])
      (info "Running version ~a" version)
      (parameterize* ([*RACKET-VERSION* version]
                      [*RACKET-BIN* (racket-bin)])
        (for-each run-round bm-dir*))))
  (info "Collecting results\n")
  (for ([version (in-list (*RACKET-VERSIONS*))])
    (parameterize ([*RACKET-VERSION* version])
      (for-each collect-rktd bm-dir*))))

(define (run-smoke-round dir)
  (setup-benchmark dir)
  (run-benchmark (vector "--smoke-test" dir)))

(define (run-smoke-test bm-dir*)
  (for ([version (in-list (*RACKET-VERSIONS*))])
    (info "Testing version ~a" version)
    (parameterize* ([*RACKET-VERSION* version]
                    [*RACKET-BIN* (racket-bin)])
      (for-each run-smoke-round bm-dir*))))

;; =============================================================================

;; "(6.2)" -> ("6.2")
(define (parse-versions str)
  (define L (string-length str))
  (and (< 2 L)
       (eq? #\( (string-ref str 0))
       (eq? #\) (string-ref str (- L 1)))
       (string-split (substring str 1 (- L 1)))))

(module+ main
  (require racket/cmdline)
  (define smoke-test? (box #f))
  (command-line
   #:program "gtp-run"
   #:once-any
   [("-j" "--jobs") j "Number of parallel jobs" (*NUM-JOBS* (string->number j))]
   [("-n" "--no-affinity")
    "Do NOT set task affinity (runs all jobs on current core)"
    (*AFFINITY?* #f)]
   #:once-each
   [("--smoke-test") "Spot-check the benchmark" (set-box! smoke-test? #true)]
   [("-i" "--iters") i "Number of iterations to run." (*NUM-ITERATIONS* (string->number i))]
   [("-v" "--version") v* "Racket versions to run e.g. '6.3' or '(6.3 6.5)'"
     (if (valid-version? v*)
       (*RACKET-VERSIONS* (list v*))
       (let ([v+* (parse-versions v*)])
         (if (and (list? v+*)
                  (andmap valid-version? v+*))
           (*RACKET-VERSIONS* v+*)
           (raise-argument-error 'gtp-barrier "Racket version, or list of versions" v*))))]
   #:args BENCHMARK-DIR*
   (begin
     (when (null? BENCHMARK-DIR*)
       (raise-user-error 'gtp-run "Usage: raco gtp-run [OPTION] ... <BENCHMARK-NAME> ..."))
     (info "Validating benchmark directories")
     (for-each assert-benchmark? BENCHMARK-DIR*)
     (if (unbox smoke-test?)
       (run-smoke-test BENCHMARK-DIR*)
       (run-all-rounds BENCHMARK-DIR* (*NUM-ITERATIONS*))))))

;; =============================================================================

(module+ test
  (require rackunit rackunit-abbrevs)

  (check-apply* parse-versions
   [""
    => #f]
   ["()"
    => #f]
   ["(6.2 6.3)"
    => '("6.2" "6.3")])

)
