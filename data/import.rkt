#lang racket/base

;; Usage:
;;   racket import.rkt <DIR>
;; - Search <DIR> for new .rktd files
;; - Find a "matching" old .rktd file
;; - Append the new data to the old file & note the change
;; - Or just copy in the new data

;; -----------------------------------------------------------------------------

(provide
)

(require
  ;file/glob
  glob

  racket/path
  racket/string
  (only-in gtp-summarize/summary
    path->version)
  (only-in gtp-summarize/modulegraph
    get-git-root
    infer-project-dir
    path->project-name)
  (only-in racket/file
    file->value)
  (only-in racket/port
    open-output-nowhere)
)

;; =============================================================================

(define *ROOT* (make-parameter #f))

;; -----------------------------------------------------------------------------

;; import-dir : (-> Directory Void)
;; Import data from a directory to the GTP repo
(define (import-dir dir)
  ;for ([fname (in-glob (format "~a/**/*.rktd"))])
  (for ([fname (in-directory dir)]
        #:when (rktd? fname))
    (import-file fname)))

;; import-file : (-> File Void)
(define (import-file fname)
  (define-values (benchmark-name version) (parse-filename fname))
  (define existing (find-data-file benchmark-name version))
  (if existing
    (append-data existing fname)
    (new-data fname version)))

(define (append-data old-file new-file)
  (log-import-info "appending dataset '~a' to existing file ~a" new-file old-file)
  (define tmp-file (build-path (find-system-path 'temp-dir) "gtp-import.tmp.rktd"))
  (define old-prefix (parse-prefix old-file))
  (define new-prefix (parse-prefix new-file))
  (define new-tag (gentag old-prefix))
  (with-output-to-file tmp-file #:exists 'replace
    (λ ()
      (printf ";; ~a~n" new-tag)
      (for-each displayln new-prefix)
      (for-each displayln old-prefix)
      (define v-old (file->value old-file))
      (define v-new (file->value new-file))
      (unless (= (vector-length v-old) (vector-length v-new))
        (raise-user-error 'append-data "files ~a and ~a have unequal number of rows, cannot append" old-file new-file))
      (displayln "#(")
      (for ([old-data (in-vector v-old)]
            [new-data (in-vector v-new)])
        (display "(")
        (for ([o (in-list old-data)])
          (display " ")
          (display o))
        (printf " #| ~a |#" new-tag)
        (for ([n (in-list new-data)])
          (display " ")
          (display n))
        (displayln ")")
        (void))
      (displayln ")")
      (void)))
  (copy-file tmp-file old-file #t))

(define (new-data fname version)
  (log-import-info "new ~a dataset, '~a'" version fname)
  (copy-file fname (build-path (*ROOT*) "data" version (file-name-from-path fname))))

;; -----------------------------------------------------------------------------

(define (log-import-info msg . arg*)
  (printf "[GTP-IMPORT] ")
  (apply printf msg arg*)
  (newline))

(define (log-import-debug msg)
  (displayln msg))

;; tag? : (-> String Boolean)
(define (tag? str)
  (regexp-match? #rx"^;; ([a-z0-9]*)$" str))

;; gentag : (-> (Listof String) String)
;; Return a "unique" tag for a data series
;; EXAMPLE
;; if a data file starts with a prefix like:
;;   ;; #(-j 2 -i 5 -v 6.4 benchmarks/gregor benchmarks/fsmoo benchmarks/sieve)
;;   ;; 6.4
;;   ;; binaries in /home/ben/code/racket/6.4/bin/
;;   ;; base @ <unknown-commit>
;;   ;; typed-racket @ 495da1bd
;;   ;; 2016-11-21T06:57:57
;; then we want a tag <NEWTAG> so we can append a new prefix <NEWPREFIX> to
;; the top of the file
;;   ;; <NEWTAG>
;;   ;; <NEWPREFIX>
;;   ;; #(-j 2 ....)
;; the constraint is that <NEWTAG> doesn't appear as a tag in the old prefix
(define (gentag prefix)
  (define old-tags (filter tag? prefix))
  (format "data~a" (length old-tags)))

;; parse-prefix : (-> Path-String (Listof String))
;; Return the comments from the top of a data file
(define (parse-prefix rktd)
  (with-input-from-file rktd
    (λ ()
      (for/list ([ln (in-lines)]
                 #:break (not (string-prefix? ln ";;")))
        ln))))

;; find-data-file : String String -> Path-String
(define (find-data-file str v)
  (glob-first (string-append (*ROOT*) "/data/" v "/" str "-" "*" ".rktd")))

(define (glob-first str)
  (or (for/first ([x (in-glob str)]) x)
      (raise-user-error 'glob-first "no results for '~a'" str)))

;; parse-filename : (-> Path-String (Values String String))
;; Split the filename of a .rktd file into the benchmark name and Racket version
(define (parse-filename ps)
  (define bm-name (path->project-name ps))
  (define v-name (parse-version ps))
  (if (and bm-name
           v-name
           (infer-project-dir bm-name))
    (values bm-name v-name)
    (raise-user-error 'parse-filename "failed to parse '~a'" ps)))

;; parse-version : (-> Path-String String)
(define (parse-version ps)
  (define m (regexp-match #rx"-v(6\\.[0-9])-" (path-string->string ps)))
  (cond
   [(and m (directory-exists? (cadr m)))
    (cadr m)]
   [else
    (raise-user-error 'parse-version "failed to parse version from '~a'" ps)]))

;; path-string->string : (-> Path-String String)
(define (path-string->string ps)
  (if (string? ps) ps (path->string ps)))

;; rktd? : (-> Path-String Boolean?)
(define (rktd? ps)
  (string-suffix? (path->string (file-name-from-path ps)) ".rktd"))

;; check-gtp-root : (-> Void)
(define (check-gtp-root)
  (define git-root
    (with-handlers ([exn:fail:user? (λ (x) #f)])
      (parameterize ([current-error-port (open-output-nowhere)]) ;; sorry
        (get-git-root))))
  (if (and git-root
           (string=? (path->string (current-directory))
                     (path->string (build-path (string->path git-root) "data/"))))
    git-root
    (raise-user-error 'check-gtp-root "must call from the 'data/' directory of the 'gradual-typing-performance' repo")))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "import"
    #:args DIR*
    (parameterize ([*ROOT* (check-gtp-root)])
      (for-each import-dir DIR*))))

;; -----------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (test-case "check-gtp-root"
    ;; Should fail if current directory is not the gtp/data directory
    (parameterize ([current-directory (find-system-path 'temp-dir)])
      (check-exn #rx"check-gtp-root"
        check-gtp-root)))

)
