#lang racket/base

;; Benchmark driver

(require math/statistics
         racket/cmdline
         racket/match
         racket/system)

;; Flag to set compilation-only mode. If #f it will compile and
;; run, otherwise only compiles.
(define only-compile? (make-parameter #f))

;; The number of times to run the benchmark as a string (to
;; be parsed later)
(define num-iterations (make-parameter "1"))

;; A path to write data to
(define output-path (make-parameter #f))

(module+ main
  (match-define (list basepath entry-point)
    (command-line #:program "benchmark-runner"
                  #:once-each
                  [("-c" "--only-compile") "Only compile and don't run"
                                           (only-compile? #t)]
                  [("-o" "--output") o-p
                                     "A path to write data to"
                                     (output-path o-p)]
                  #:multi
                  [("-i" "--iterations") n-i
                                         "The number of iterations to run"
                                         (num-iterations n-i)]
                  #:args (basepath entry-point)
                  (list basepath entry-point)))
  (unless (path-string? basepath)
    (raise-user-error (format "expected a path, given ~a" basepath)))
  (unless (and (path-string? entry-point)
               (regexp-match? #rx"\\.rkt$" entry-point))
    (raise-user-error (format "expected a Racket file, given ~a" entry-point)))
  (define iters (string->number (num-iterations)))
  (unless (number? iters)
    (raise-user-error (format "expected a number, given ~a" (num-iterations))))

  (define variations
    (for/list ([file (in-list (directory-list (build-path basepath "benchmark")))]
               #:when (not (equal? "base" (path->string file))))
      (build-path basepath "benchmark" file entry-point)))

  (define results (make-vector (length variations)))

  (for ([(var var-idx) (in-indexed (in-list variations))])
    (define-values (new-cwd file _2) (split-path var))
    (define times null)
    (for ([i (in-range iters)])
      (printf "iteration #~a of ~a~n" i var)
      ;; FIXME: use benchmark library
      (parameterize ([current-directory new-cwd])
        ;; FIXME: compile first in separate pass?
        (system (string-append "raco make -v " (path->string file)))
        (unless (only-compile?)
          (define-values (_ cpu real gc)
            (time-apply
             (λ () (system (string-append "racket " (path->string file))
                           #:set-pwd? #t))
             null))
          (printf "cpu: ~a real: ~a gc: ~a~n" cpu real gc)
          (set! times (cons real times)))))
    (vector-set! results var-idx (cons (mean times) (stddev times))))

  (when (output-path)
    (with-output-to-file (output-path)
      (λ () (write results))
      #:mode 'text
      #:exists 'replace)))
