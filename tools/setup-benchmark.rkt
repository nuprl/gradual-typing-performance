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
(require srfi/13
         (prefix-in mg: gtp-summarize/modulegraph))

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
  (define bname (call-with-values (λ () (split-path pwd))
                                  (λ xs (second xs))))
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
  (create-populate-configurations-directories* bname configuration bdir both typed untyped file-names*)
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
(define (create-populate-configurations-directories* bname name bdir both typed untyped file-names*)
  ;; Normal Directories
  (for/list ([combination (in-list (build-combinations* (length file-names*)))])
    (define cdir (build-path bdir (apply string-append "configuration" (map number->string combination))))
    (make-directory cdir)
    (create-readme cdir (populate-configuration cdir file-names* combination both typed untyped))
    cdir)
  (create-populate-prediction-configurations-directories* bname name bdir both typed untyped file-names*)
  )

;; ModuleGraph -> Listof (Pairof String String)
(define (mk-edges mg)
  (apply append
         (for/list ([m-name (in-list (mg:module-names mg))])
           (for/list ([req-name (in-list (mg:requires mg m-name))])
             (list (string-append m-name   ".rkt") (mg:name->index mg m-name)
                   (string-append req-name ".rkt") (mg:name->index mg req-name))))))

(define (create-populate-prediction-configurations-directories* bname name bdir both typed untyped file-names*)
  ;; First, the untyped requiring typed edge directories
  ;; THen, the typed requiring untyped edge directories
  (define mg (mg:from-directory bname))
  (define es (mk-edges mg))
  ;; todo: don't do this yourself
  (for ([e (in-list es)])
    (define ut-dir (build-path bdir (format "edge-u~a-t~a" (second e) (fourth e))))
    (make-directory ut-dir)
    (populate-edge-configuration ut-dir file-names* #f (first e) (third e) both typed untyped)
    ;; todo fix pop-edge-config
    (define tu-dir (build-path bdir (format "edge-t~a-u~a" (second e) (fourth e))))
    (make-directory tu-dir)
    (populate-edge-configuration tu-dir file-names* #t (first e) (third e) both typed untyped))

  ;; Then, the optimization directories
  (for/list ([file-name (in-list file-names*)]
             [i (in-naturals)])
    (define-values (_a basename _c) (split-path file-name))
    (define odir (build-path bdir (format "optimize-~a" i)))
    (make-directory odir)
    (populate-optimize-configuration odir file-name file-names* both typed untyped)))

;; ---------------------------------------------------------------------------------------------------
;; Path [Listof [List String Boolean]] -> Void 
(define (create-readme cdir population)
  (with-output-to-file (build-path cdir "README") (lambda () (pretty-print population))))

(define (populate-both cdir both)
  (when (directory-exists? both) ; both is optional
    (for ([file-name (in-list (directory-list both))])
      (copy-file (build-path both file-name) (build-path cdir file-name)))))
;; ---------------------------------------------------------------------------------------------------
;; Path [Listof String] Combination Path Path Path -> Void
(define (populate-configuration cdir file-names* combination both typed untyped)
  (populate-both cdir both)
  (for ([file-name (in-list file-names*)][src (in-list combination)])
    (copy-file (build-path (if (typed? src) typed untyped) file-name) (build-path cdir file-name))))

(define (populate-edge-configuration cdir file-names* requirer-typed? requirer requiree both typed untyped)
  (populate-both cdir both)
  (for ([file-name (in-list file-names*)])
    (cond [(or (and requirer-typed?       (equal? file-name requirer))
               (and (not requirer-typed?) (equal? file-name requiree)))
           (copy-no-optimize cdir typed file-name)]
          [else
           (copy-file (build-path untyped file-name)
                      (build-path cdir file-name))]))
  (write-overrides-file cdir (cons requirer requiree)))

(define (populate-optimize-configuration cdir opt-file-name file-names* both typed untyped)
  (populate-both cdir both)
  (for ([file-name file-names*])
    (copy-file (build-path (if (equal? file-name opt-file-name)
                               typed
                               untyped)
                           file-name)
               (build-path cdir file-name)))
  (write-overrides-file cdir))

(define (write-overrides-file cdir . overrides)
  (with-output-to-file (build-path cdir "override-all-except.rktd")
    (λ () (write overrides))))

(define (copy-no-optimize cdir typed file-name)
  (call-with-output-file (build-path cdir file-name)
    (λ (outp)
      (call-with-input-file (build-path typed file-name)
        (λ (inp)
          (define lang-line (read-line inp))
          (write-string lang-line outp)
          (write-string " #:no-optimize\n" outp)
          (copy-port inp outp))))))

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
