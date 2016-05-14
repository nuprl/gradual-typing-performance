#lang racket/base

;; TODO
;; - is shuffle a good idea?
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
  (define try-path (build-path (find-system-path 'home-dir) "code" "racket" (*RACKET-VERSION*) "bin"))
  (if (has-racket-binaries? try-path)
    (string-append (path->string try-path) "/")
    (raise-user-error 'racket-bin "Could not locate Racket v~a binaries at '~a'" (*RACKET-VERSION*) try-path)))

;; Call `run.rkt` with the right parameters
(define (run fname benchmark)
  (setup-benchmark benchmark)
  (run-benchmark (vector
                         "-o" fname
                         "-i" (number->string 1)
                         "-p" "shuffle" ;; Good idea?
                         benchmark)))

(define (run-round dir)
  (unless (refresh-benchmark dir)
    (setup-benchmark dir))
  (define fname (output-file dir #:tmp? #t))
  (run fname dir))

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
  (command-line
   #:program "gtp-barrier"
   #:once-each
   [("-j" "--jobs") j "Number of parallel jobs" (*NUM-JOBS* (string->number j))]
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
     (define outer-iters (*NUM-ITERATIONS*))
     (info "Validating benchmark directories")
     (for-each assert-benchmark? BENCHMARK-DIR*)
     (for ([i (in-range outer-iters)])
       (info "Starting round ~a\n" i)
       (for ([version (in-list (*RACKET-VERSIONS*))])
         (info "Running version ~a" version)
         (parameterize* ([*RACKET-VERSION* version]
                         [*RACKET-BIN* (racket-bin)])
           (for-each run-round BENCHMARK-DIR*))))
     (info "Collecting results\n")
     (for ([version (in-list (*RACKET-VERSIONS*))])
       (parameterize ([*RACKET-VERSION* version])
         (for-each collect-rktd BENCHMARK-DIR*))))))

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
