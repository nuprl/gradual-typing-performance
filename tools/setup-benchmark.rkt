#lang racket

(provide create-benchmark-dirs)

;; Usage:
;;   racket setup-benchmark.rkt <directory>
;; will set up benchmarks using the provided directory
(module+ main
  (match (current-command-line-arguments)
    [(vector path)
     (create-benchmark-dirs path)]))

;; ===================================================================================================
(require srfi/13)

(define TYPED? "typed?")
(define TYPED "yes")
(define UNTYPED "no")
(define COMMENT (string-append ";; " (make-string 77 #\-)))

(define (create-benchmark-dirs 
         pwd
         #:base [base (build-path pwd "base")]
         #:both [both (build-path pwd "both")]
         #:typed [typed (build-path pwd "typed")]
         #:untyped [untyped (build-path pwd "untyped")]
         #:benchmark-dir [bdir (build-path pwd "benchmark")]
         #:dir-base-name [configuration "configuration"])
  
  ;; -------------------------------------------------------------------------------------------------
  ;; CONTRACT:
  ;; (1) make sure directories base, typed, and untyped exist 
  ;; (2) the typed and untyped contain the same number of files 
  ;; (3) ... and files with the same names
  ;;
  ;; Note: 'both' directory is optional
  
  (define typed-dir (source-paths (directory-list typed)))
  (define untyped-dir (source-paths (directory-list untyped)))
  
  (files-exist? pwd typed untyped)
  (same-number-of-files? typed-dir untyped-dir)
  (same-file-names? typed-dir untyped-dir)
  ;; -------------------------------------------------------------------------------------------------
  ;; WORK: 
  
  (define file-names* ;; because typed-dir and untyped-dir contains same file names 
    (for/list ([path (in-list typed-dir)])
      (path->string path)))
  
  (set-up-benchmark-directory bdir)
  (create-populate-base-directory base bdir)
  (create-populate-configurations-directories* configuration bdir both typed untyped file-names*)
  ;; throw them away: 
  (void))

;; ---------------------------------------------------------------------------------------------------
;; [Listof Path] -> [Listof Path]
(define (source-paths ps)
  (define (is-source-path p)
    (define ext (filename-extension p))
    (or (equal? ext #"rkt")
        (equal? ext #"ss")))
  (filter is-source-path ps))

;; String Path Path Path Path [Listof String] -> [Listof String]
(define (create-populate-configurations-directories* name bdir both typed untyped file-names*)
  (for/list ([combination (in-list (build-combinations* (length file-names*)))])
    (define cdir (build-path bdir (apply string-append "configuration" (map number->string combination))))
    (make-directory cdir)
    (create-readme cdir (populate-configuration cdir file-names* combination both typed untyped))
    cdir))

;; ---------------------------------------------------------------------------------------------------
;; Path [Listof [List String Boolean]] -> Void 
(define (create-readme cdir population)
  (with-output-to-file (build-path cdir "README") (lambda () (pretty-print population))))

;; ---------------------------------------------------------------------------------------------------
;; Path [Listof String] Combination Path Path Path -> Void
(define (populate-configuration cdir file-names* combination both typed untyped)
  (when (directory-exists? both) ; both is optional
    (for ([file-name (in-list (directory-list both))])
      (copy-file (build-path both file-name) (build-path cdir file-name))))
  (for/list ([file-name (in-list file-names*)][src (in-list combination)])
    (copy-file (build-path (if (typed? src) typed untyped) file-name) (build-path cdir file-name))
    #;
    (special-case-for-acquire file-name typed? typed untyped cdir)
    (list file-name (typed? src))))

(define typed? (compose not zero?))

;; ---------------------------------------------------------------------------------------------------
;; String -> String 
(define (filled s)
  (string-pad-right s (- 80 (string-length TYPED?))))

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
    (copy-directory/files base (build-path bdir "base") #:keep-modify-seconds? #t)))

;; ---------------------------------------------------------------------------------------------------
;; [Listof Path] [Listof Path] -> Void
(define (same-file-names? typed-dir untyped-dir)
  (unless (set=? (map path->string typed-dir) (map path->string untyped-dir))
    (error "Typed and Untyped directories do not contain the same file names.")))

;; ---------------------------------------------------------------------------------------------------
;; [Listof X] [Listof Y] -> Void
(define (same-number-of-files? typed-dir untyped-dir)
  (unless (= (length typed-dir) (length untyped-dir))
    (error "Typed and Untyped directories do not have the same number of files")))

;; ---------------------------------------------------------------------------------------------------
;; String *-> Boolean 
(define (files-exist? . dir*)
  (for ([dir (in-list dir*)])
    (unless (directory-exists? dir)
      (raise-user-error 'setup-benchmark "directory `~a' does not exist" dir))))

;; ---------------------------------------------------------------------------------------------------
;; Combination = [Listof {0|1}]

;; Integer -> [Listof Combination]
(define (build-combinations* n)
  [when (< n 1) (error "No combinations of length 0")]
  (let build-combinations ([n n])
    (cond
      [(= n 1) (list (list 0) (list 1))]
      [else (let ([last (build-combinations (sub1 n))])
              (append (map (curry cons 0) last) (map (curry cons 1) last)))])))

;; ---------------------------------------------------------------------------------------------------

(define (special-case-for-acquire file-name typed? typed untyped cdir)
  (when (string=? file-name "tree.rkt")
    ;; special case for Acquire:
    (copy-file
     (build-path "conditional" (if typed? typed untyped) "typed-wrapper.rkt")
     (build-path cdir "typed-wrapper.rkt"))))
