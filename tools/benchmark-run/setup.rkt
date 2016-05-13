#lang racket

;; TODO cleanup this file

(provide
  setup-benchmark
  ;; TODO

  refresh-benchmark
  ;; TODO remove data files
)

(require
  glob
  benchmark-run/benchmark-data
  benchmark-run/utilities
)

;; ===================================================================================================

(define (setup-benchmark pwd)
  (assert-benchmark? pwd)
  (define-values (base both typed untyped bdir) (benchmark-paths pwd))
  (define file-names* ;; because typed-dir and untyped-dir contains same file names 
    (filter rkt-file? (directory-list typed)))
  (set-up-benchmark-directory bdir)
  (create-populate-base-directory base bdir)
  (create-populate-configurations-directories* CONFIGURATION bdir both typed untyped file-names*)
  ;; throw them away: 
  (void))

(define (refresh-benchmark dir)
  (define bdir (build-path dir BENCHMARK))
  (and (directory-exists? bdir)
       (for ([rktd-file (in-glob (string-append (path->string bdir) "/*/*.rktd"))])
         (delete-file rktd-file))
       #t))

;; ---------------------------------------------------------------------------------------------------

;; String Path Path Path Path [Listof String] -> [Listof String]
(define (create-populate-configurations-directories* name bdir both typed untyped file-names*)
  (for/list ([combination (in-list (build-combinations* (length file-names*)))])
    (define cdir (build-path bdir (apply string-append CONFIGURATION (map number->string combination))))
    (make-directory cdir)
    (define pop (populate-configuration cdir file-names* combination both typed untyped))
    ;(create-readme cdir pop)
    cdir))

;; ---------------------------------------------------------------------------------------------------
;; Path [Listof [List String Boolean]] -> Void 
(define (create-readme cdir population)
  (with-output-to-file (build-path cdir "README") (lambda () (pretty-print population))))

;; ---------------------------------------------------------------------------------------------------
;; Path [Listof String] Combination Path Path Path -> Void
(define (populate-configuration cdir file-names* combination both typed untyped)
  (when (directory-exists? both) ; both is optional
    (for ([file-name (in-list (directory-list both))]
          #:when (rkt-file? file-name))
      (copy-file (build-path both file-name) (build-path cdir file-name))))
  (for/list ([file-name (in-list file-names*)]
             [src (in-list combination)])
    (copy-file (build-path (if (typed? src) typed untyped) file-name) (build-path cdir file-name))
    (list file-name (typed? src))))

(define typed? (compose not zero?))

;; ---------------------------------------------------------------------------------------------------
;; Path -> Void 
(define (set-up-benchmark-directory bdir)
  (when (directory-exists? bdir)
    (printf "Deleting ~a before setup~n" bdir)
    (delete-directory/files bdir))
  (make-directory bdir))

;; ---------------------------------------------------------------------------------------------------
;; Path Path -> Void 
(define (create-populate-base-directory base bdir)
  (when (directory-exists? base)
    (copy-directory/files base (build-path bdir BASE) #:keep-modify-seconds? #t)))

;; Combination = [Listof {0|1}]
;; Integer -> [Listof Combination]
(define (build-combinations* n)
  [when (< n 1) (error "No combinations of length 0")]
  (let build-combinations ([n n])
    (cond
      [(= n 1) (list (list 0) (list 1))]
      [else (let ([last (build-combinations (sub1 n))])
              (append (map (curry cons 0) last) (map (curry cons 1) last)))])))

;; =============================================================================

;; Usage:
;;   racket setup-benchmark.rkt <directory>
;; will set up benchmarks using the provided directory
(module+ main
  (match (current-command-line-arguments)
    [(vector path)
     (unless (directory-exists? path)
       (raise-user-error 'setup "Invalid directory '~a'" path))
     (setup-benchmark path)]))

;; =============================================================================

(module+ test

  (require rackunit)

  ;; Assert no benchmark/ at start
  ;; Create benchmark/ dir, want same num dirs as 2**N modules
  ;; Run refresh, watch for data files
  (define (check-benchmark pre-bm)
    (define bm (string-append "../../benchmarks/" pre-bm))
    ;; -- make sure directory exists, remove benchmark dir
    (define bbm (string-append bm "/" BENCHMARK))
    (unless (directory-exists? bm)
      (raise-user-error 'check-benchmark "Directory '~a' does not exist" bm))
    (delete-directory/files bbm #:must-exist? #f)
    ;; --
    (setup-benchmark bm)
    (define cfg-dir* (glob (string-append bbm "/config*")))
    (check-equal? (length cfg-dir*)
                  (expt 2 (length (glob (string-append bm "/" UNTYPED "/*.rkt")))))
    ;; -- make random data files
    (for ([dir (in-list cfg-dir*)])
      (with-output-to-file (string-append dir "/test.rktd")
        (lambda () (displayln "test"))))
    ;; -- clear!
    (check-true (refresh-benchmark bm))
    (check-equal? (length (glob (string-append bbm "/config*/*.rktd")))
                  0))

  (check-benchmark "test-success")

)
