#lang racket/base

;; Test example benchmarks
;; - Correct benchmarks should run without error
;; - Compile-error benchmarks should generate an error
;; - Runtime-error benchmarks should generate an error

(module+ test

  (require
    rackunit
    (only-in racket/string string-split)
    (only-in racket/port call-with-output-string open-output-nowhere)
    gtp-summarize/modulegraph
    "setup-benchmark.rkt"
    "run.rkt")

  (define (pass/fail should-pass?)
    (if should-pass?
      "PASS"
      "FAIL"))

  (define (check-benchmark bm #:max-output-lines [max-lines #f]
                              #:output-rx* [rx* #f]
                              #:should-fail? [should-fail? #f])
    (define project-dir (path->string (infer-project-dir bm)))
    (create-benchmark-dirs project-dir)
    (define should-pass? (not should-fail?))
    (define str
      (parameterize ([error-print-context-length 0])
        (call-with-output-string
          (lambda (p)
            (parameterize ([current-output-port (open-output-nowhere)]
                           [current-error-port p])
              (run-benchmark (vector "-n" "-i" "1" project-dir)))))))
    (check-equal? (string=? "" str) should-pass?
      (format "Expected benchmark '~a' to ~a, but it ~a-ed instead."
        bm (pass/fail should-pass?) (pass/fail (not should-pass?))))
    (when max-lines
      (let ([L (length (string-split str "\n"))])
        (check-true (<= L max-lines)
          (format "Expected '~a' to generate at most ~a lines of output, got ~a lines."
            bm max-lines L))))
    (when rx*
      (for/list ([rx (in-list (if (list? rx*) rx* (list rx*)))])
        (check-regexp-match rx str)))
    (void))

  (check-benchmark "test-success"
    #:max-output-lines 2)

  (check-benchmark "test-compile-error"
    #:should-fail? #t
    #:output-rx* '(#rx"Type Checker" #rx"run:compile" #rx"configuration01")
    #:max-output-lines 30)

  (check-benchmark "test-runtime-error"
    #:should-fail? #t
    #:output-rx* '(#rx"run:runtime" #rx"configuration01")
    #:max-output-lines 20)

)
