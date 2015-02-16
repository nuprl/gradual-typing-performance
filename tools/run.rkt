#lang racket/base

;; Benchmark driver

(require racket/cmdline
         racket/match
         racket/system)

;; Flag to set compilation-only mode. If #f it will compile and
;; run, otherwise only compiles.
(define only-compile? (make-parameter #f))

;; The number of times to run the benchmark as a string (to
;; be parsed later)
(define num-iterations (make-parameter "1"))

(module+ main
  (match-define (list basepath entry-point)
    (command-line #:program "benchmark-runner"
                  #:once-each
                  [("-c" "--only-compile") "Only compile and don't run"
                                           (only-compile? #t)]
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

  (for ([var (in-list variations)])
    (define-values (new-cwd file _2) (split-path var))
    (for ([i (in-range iters)])
      (printf "iteration #~a of ~a~n" i var)
      ;; FIXME: use benchmark library
      (parameterize ([current-directory new-cwd])
        ;; FIXME: compile first in separate pass?
        (system (string-append "raco make -v " (path->string file)))
        (unless (only-compile?)
          (time (system (string-append "racket " (path->string file))
                        #:set-pwd? #t)))))))
