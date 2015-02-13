#lang racket/base

;; Benchmark driver

(require racket/cmdline
         racket/match
         racket/system)

(module+ main
  (match-define (list basepath entry-point num-iterations)
    (command-line #:program "benchmark-runner"
                  #:args (basepath entry-point num-iterations)
                  (list basepath entry-point num-iterations)))
  (unless (path-string? basepath)
    (raise-user-error (format "expected a path, given ~a" basepath)))
  (unless (and (path-string? entry-point)
               (regexp-match? #rx"\\.rkt$" entry-point))
    (raise-user-error (format "expected a Racket file, given ~a" entry-point)))
  (define iters (string->number num-iterations))
  (unless (number? iters)
    (raise-user-error (format "expected a number, given ~a" num-iterations)))

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
        (time (system (string-append "racket " (path->string file))
                      #:set-pwd? #t))))))
