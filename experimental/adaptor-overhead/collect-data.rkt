#lang racket/base

(require
  racket/pretty
  racket/match
  racket/path
  racket/file
  (only-in racket/port port->lines)
  racket/system
)

;; =============================================================================

(define *OUTFILE* (make-parameter "out.rktd"))
(define *ITERS* (make-parameter 20))
(define *RKT-VERSION* (make-parameter "6.5"))

(define test-dirs '("typed" "no-adapt"))

(define (benchmark-file p)
  (define version (*RKT-VERSION*))
  (parameterize ([current-directory (path-only p)])
    (delete-directory/files "compiled" #:must-exist? #f)
    (system (format "raco~a make main.rkt" version))
    (define cmd (format "racket~a main.rkt" version))
    ;; --
    (for/list ([i (in-range (*ITERS*))])
      (match-define (list out in pid err control) (process cmd #:set-pwd? #t))
      (control 'wait)
      (for-each displayln (port->lines err))
      (define t
       (let ([time-info ;; 2016-05-10: can also use for/first
             (for/last ([line (in-list (port->lines out))])
              (regexp-match #rx"cpu time: (.*) real time: (.*) gc time: (.*)" line))])
        (match time-info
         [(list full
             (app string->number cpu)
             (app string->number real)
             (app string->number gc))
         ;;(printf "job#~a, iteration#~a - cpu: ~a real: ~a gc: ~a~n" job# i cpu real gc)
         cpu]
         [#f
          (error 'run (current-directory))])))
      (close-input-port out)
      (close-input-port err)
      (close-output-port in)
      t)))

(module+ main
  (require racket/cmdline)
  (command-line
   #:once-each
   [("-o" "--output")
    o
    "Specify output file"
    (*OUTFILE* o)]
   #:args ()
   (define dataset
     (for/list ([bm (in-list (directory-list "."))]
                #:when (directory-exists? bm))
       (printf "[collect] running ~a\n" bm)
       (cons (string->symbol (path->string bm))
             (for/list ([d (in-list test-dirs)]
                        #:when (or (directory-exists? (build-path bm d))
                                   (begin
                                     (printf "Skipping '~a', directory '~a' does not exist" bm d)
                                     #f)))
               (cons (string->symbol d)
                     (benchmark-file (build-path bm d "main.rkt")))))))
   (with-output-to-file (*OUTFILE*) #:exists 'replace
     (lambda () (pretty-print dataset)))
   (void)))
