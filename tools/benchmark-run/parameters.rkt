#lang racket/base

(provide (all-defined-out))

(define *NUM-JOBS* (make-parameter 2)) ;; a safe default

;; Number of iterations to run the benchmark
;; - If `num-iterations` is given, run EXACTLY that
;; - By default, run `min-iterations` then check for non-normality.
;;   If non-normal, run more--until `max-iterations`
(define *MIN-ITERATIONS* (make-parameter 10))
(define *MAX-ITERATIONS* (make-parameter 30))
(define *NUM-ITERATIONS* (make-parameter 8))

(define *RACKET-VERSION* (make-parameter "6.5"))
(define *UID* (make-parameter "default"))
;; # warmup iterations to run. Throw these numbers away
(define *NUM-WARMUP* (make-parameter 1))

;; Flag to set compilation-only mode. If #f it will compile and
;; run, otherwise only compiles.
(define *ONLY-COMPILE?* (make-parameter #f))

;; Processes that finish quicker than *MIN-MILLISECONDS*
;;  are very likely to be affected by OS effects
(define *MIN-MILLISECONDS* (make-parameter (* 1.3 1000)))

;; Apply transformation to the list of configurations before running
;; TODO should be a sequence
(define *PERMUTE* (make-parameter values))

(define *AFFINITY?* (make-parameter #t)) ;; Boolean
(define *ERROR-MESSAGES* (make-parameter '())) ;; (Listof String)

(define *DATA-TMPFILE* (make-parameter #f))
(define *TIME-TMPFILE* (make-parameter "time.tmp"))
(define *TEMPERATURE-FREQUENCY* (make-parameter 1))
(define *TEMPERATURE-MONITOR?* (make-parameter #f))

;; Paths to write results/diagrams
(define *OUTPUT-PATH* (make-parameter #f))

(define *ENTRY-POINT-PARAM* (make-parameter #f))
(define *EXCLUSIVE-CONFIG* (make-parameter #f))
(define *MIN-MAX-CONFIG* (make-parameter #f))

;; --- begin WARNING: do not set this parameter
(define *RACKET-BIN* (make-parameter #f))
;; --- end WARNING
