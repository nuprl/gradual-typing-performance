#lang racket/base

;; Check the progress of a running benchmark.
;;
;; Usage:
;;   racket status.rkt <PROJECT-NAME>
;;   racket status.rkt <LOGFILE-NAME>
;;
;; Prints the number of configurations that have at least started to run.
;;
;; Preconditions:
;; - <PROJECT-NAME> is the name of a benchmark folder,
;;   and '<PROJECT-NAME>.log' is currently being used as a logfile
;; - <LOGFILE-NAME> begins with the name of a benchmark folder + a hyphen, and ends with '.log'
;;   Example: 'quad-12-06.log'

(require
  glob
  (only-in racket/format
    ~r)
  (only-in racket/system
    system)
  (only-in racket/port
    with-output-to-string)
  (only-in gtp-summarize/summary
    path->project-name)
  (only-in racket/string
    string-trim
    string-suffix?)
)

;; -----------------------------------------------------------------------------

(define ERRLOC 'status)

(define-syntax-rule (raise-project-error p)
  (raise-user-error ERRLOC (format "Could not find project sources '~a'" p)))

(define-syntax-rule (raise-logfile-error p)
  (raise-user-error ERRLOC (format "Invalid/missing logfile '~a'" p)))

(define (infer-path p-or-log)
  (cond
   [(string-suffix? p-or-log ".log")
    ;; then: It's a logfile
    (unless (file-exists? p-or-log)
      (raise-logfile-error p-or-log))
    (define project (path->project-name p-or-log))
    (unless (directory-exists? project)
      (raise-project-error project))
    (values p-or-log project)]
   [else
    ;; else: It's a project name
    (unless (directory-exists? p-or-log)
      (raise-project-error p-or-log))
    (define log (string-append p-or-log ".log"))
    (unless (file-exists? log)
      (raise-logfile-error log))
    (values log p-or-log)]))

(define (count-num-started logfile)
  (define s
    (with-output-to-string
      (lambda ()
        (system (format "cat ~a | grep \"\\\"main\\.rkt\\\"\" | wc -l" logfile)))))
  (string->number (string-trim s)))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "status"
    #:args (project-name-or-logfile)
    (define-values (logfile project) (infer-path project-name-or-logfile))
    (define num-configs (expt 2 (length (glob (string-append project "/typed/*.rkt")))))
    (define num-started (count-num-started logfile))
    (unless num-started
      (raise-user-error ERRLOC (format "Error parsing logfile '~a'" logfile)))
    (printf "~a: started ~a of ~a configurations (~a% complete)\n"
      project
      num-started
      num-configs
      (~r (/ num-started num-configs) #:precision 2))))
