#lang racket/base

;; Restart the benchmarks from yesterday.
;; 

;; - clean up old mutex.lock & unfinished data files
;; - start a few nodes
;; - remember last-used parameters

(require
  (only-in racket/file file->value)
  (only-in racket/port open-output-nowhere)
  (only-in racket/string string-prefix?)
  (only-in racket/system system)
)

;; =============================================================================

;; Location for reporting errors
(define ERR 'again)

(define *CACHE* (make-parameter "./.again.rktd"))

;; Cache data keys
(define VERSION 'racket-version)
(define APPEND  'current-iteration)

(define NUM-NODE 10)

(define (version? n)
  (and (member n '(6.2 6.3 6.4 6.5)) #t))

(define (read-cache)
  (define H (file->value (*CACHE*)))
  (or (and (not (eof-object? H))
           (hash? H)
           (hash-eq? H)
           (hash-has-key? H VERSION)
           (hash-has-key? H APPEND)
           H)
      (begin (WARNING "Malformed data '~a' in cachefile '~a'" H (*CACHE*))
             #f)))

(define (WARNING str . arg*)
  (display "WARNING: ")
  (apply format str arg*)
  (newline))

(define (get-cache-from-user)
  (hash-set* (make-immutable-hasheq)
    VERSION (string->number/ui "a Racket version" version?)
    APPEND  (string->number/ui "The number of iterations we have finished already" exact-nonnegative-integer?)))

(define (string->number/ui description ok?)
  (define (prompt) (printf "Enter ~a:\n" description))
  (define (read-from-user) (string->number (begin (prompt) (read-line))))
  (let loop ([v (read-from-user)])
    (or (and (number? v)
             (ok? v)
             v)
        (loop (read-from-user)))))

(define (run-benchmarks H)
  (define R (hash-ref H VERSION))
  (define A (hash-ref H APPEND))
  (define cmd (format "sh run-benchmark.sh ~a ~a" R A))
  (for/and ((_i (in-range NUM-NODE)))
    (system cmd)))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  (command-line
   #:args ()
   (begin
    (define H (or (read-cache) (get-cache-from-user)))
    (or (run-benchmarks H)
        (raise-user-error ERR "Unknown error executing `run-benchmark.sh`"))
    (void)))
)

;; =============================================================================

(module+ test
  (require rackunit rackunit-abbrevs)

  (check-apply* version?
   [6.4
    ==> #t]
   [(string->number "6.5")
    ==> #t]
   [6.1
    ==> #f]
   ["asdf"
    ==> #f])

  (define (random-file)
    (define tmp (find-system-path 'temp-dir))
    (define (genfile) (build-path tmp (symbol->string (gensym 'testfile))))
    (let loop ([fname (genfile)])
      (if (file-exists? fname)
        (loop (genfile))
        fname)))

  (parameterize ([*CACHE* (random-file)])
    (define (test-bad-data data)
      (parameterize ([current-output-port (open-output-string)])
        (with-output-to-file (*CACHE*) #:exists 'replace
          (lambda () (writeln data)))
        (check-equal? (read-cache) #f)
        (check-equal? (string-prefix? (get-output-string (current-output-port)) "WARNING:") #t)))
    (test-bad-data "")
    (test-bad-data 42)
    (test-bad-data "[")
    (define H (make-immutable-hasheq `((,VERSION . 6.3) (,APPEND . 0))))
    (with-output-to-file (*CACHE*) #:exists 'replace
      (lambda () (displayln H)))
    (check-equal? (read-cache) H))

  (let-values ([(inp outp) (make-pipe)])
    (define H (make-immutable-hasheq `((,VERSION . 6.2) (,APPEND . 12))))
    (parameterize ([current-input-port inp]
                   [current-output-port (open-output-nowhere)])
      (writeln (hash-ref H VERSION) outp)
      (writeln (hash-ref H APPEND) outp)
      (check-equal? (get-cache-from-user) H)))

)
