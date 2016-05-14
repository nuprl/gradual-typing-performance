#lang racket/base
(date-display-format 'iso-8601)

(provide
  data-line?
  timestamp
  has-racket-binaries?
  output-file
  read-string
  data-line?
  info
  rkt-file?
  rktd-file?
)

(require
  (only-in racket/list last)
  (only-in racket/string string-split string-contains? string-suffix?)
  (only-in racket/port with-input-from-string)
  racket/date
  benchmark-run/parameters
)

;; =============================================================================

(define (data-line? str)
  (if (string? str)
    (and (< 0 (string-length str))
         (eq? #\( (string-ref str 0)))
    #f))

(define ((ends-with? suffix) p)
  (define str (if (path? p) (path->string p) p))
  (string-suffix? str suffix))

(define rkt-file? (ends-with? ".rkt"))

(define rktd-file? (ends-with? ".rktd"))

(define (timestamp)
  (date->string (current-date) #t))

(define (output-file dirname #:tmp? [tmp? #f])
  (define tag (last (string-split dirname "/")))
  (string-append tag "-v" (*RACKET-VERSION*) "-" (timestamp) (if tmp? ".tmp.rktd" ".rktd")))

(define (read-string x)
  (with-input-from-string x read))

(define-syntax-rule (info msg arg* ...)
  (begin (display "INFO: ") (printf msg arg* ...) (newline)))

(define (has-racket-binaries? dir)
  (and (directory-exists? dir)
       (file-exists? (build-path dir "racket"))
       (file-exists? (build-path dir "raco"))))

;; =============================================================================

(module+ test
  (require rackunit rackunit-abbrevs)

  (check-apply* data-line?
   [""
    => #f]
   ["a"
    => #f]
   [";;"
    => #f]
   [" ("
    => #f]
   ["("
    => #t]
   ["(1 2 3)"
    => #t])

  (check-apply* read-string
   ["1"
    => 1]
   ["(1 2 3)"
    => '(1 2 3)])

  (let* ([test "TEST"]
         [d1 (output-file test)]
         [_x (sleep 1)]
         [d2 (output-file test)])
    (check-true (not (equal? d1 d2)))
    (check-true (string-contains? d1 test))
    (check-true (string-contains? d2 test)))

)
