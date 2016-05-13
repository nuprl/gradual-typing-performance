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
)

;; =============================================================================

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
  (define fname (output-file dir))
  (run fname dir))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "gtp-barrier"
   #:once-each
   [("-j" "--jobs") j "Number of parallel jobs" (*NUM-JOBS* (string->number j))]
   [("-i" "--iters") i "Number of iterations to run." (*NUM-ITERATIONS* (string->number i))]
   #:args BENCHMARK-DIR*
   (parameterize ([*RACKET-BIN* (racket-bin)]
                  [*UID* (symbol->string (gensym 'uid))])
     (define outer-iters (*NUM-ITERATIONS*))
     (info "Validating benchmark directories")
     (for-each assert-benchmark? BENCHMARK-DIR*)
     (for ([i (in-range outer-iters)])
       (info "Starting round ~a\n" i)
       (for-each run-round BENCHMARK-DIR*))
     (info "Collecting results\n")
     (for-each collect-rktd BENCHMARK-DIR*))))

