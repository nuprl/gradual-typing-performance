#lang racket

(provide create-benchmark-dirs)

;; Usage:
;;   racket setup-benchmark.rkt
;;   racket setup-benchmark.rkt <directory>
;; will set up benchmarks using the current directory or the
;; provided one
(module+ main
  (match (current-command-line-arguments)
    [(vector path) (create-benchmark-dirs path)]
    [_             (create-benchmark-dirs (current-directory))]))

;; ===================================================================================================
(require srfi/13)

(define (create-benchmark-dirs 
         pwd
         #:base [base (build-path pwd "base")]
         #:typed [typed (build-path pwd "typed")]
         #:untyped [untyped (build-path pwd "untyped")]
         #:benchmark-dir [bdir (build-path pwd "benchmark")]
         #:dir-base-name [variation "variation"])
  
  ;; -------------------------------------------------------------------------------------------------
  ;; CONTRACT:
  ;; (1) make sure directories base, typed, and untyped exist 
  ;; (2) the typed and untyped contain the same number of files 
  ;; (3) ... and files with the same names
  
  (define typed-dir (directory-list typed))
  (define untyped-dir (directory-list untyped))
  
  (files-exist? pwd base typed untyped)
  (same-number-of-files? typed-dir untyped-dir)
  (same-file-names? typed-dir untyped-dir)
  ;; -------------------------------------------------------------------------------------------------
  ;; WORK: 
  
  (set-up-benchmark-directory bdir)
  (create-populate-base-directory base bdir)
  (define generated-variations
    (create-populate-variations-directories* variation bdir typed untyped typed-dir))
  ;; throw them away: 
  (void))

;; String Path Path Path [Listof String] -> [Listof String]
(define (create-populate-variations-directories* name bdir typed untyped typed-dir)
  (define file-names
    (for/list ([path (in-list typed-dir)] #:when (regexp-match? #rx"\\.rkt$|\\.ss$" path))
      (path->string path)))
  
  (for/list ([c (in-list (build-combinations (length file-names)))])
    (define cdir (build-path bdir (format "~a~a" name (combination->number c))))
    (make-directory cdir)
    ;; populate cdir & generate a README file in S-expression form that specifies the origin of files
    (with-output-to-file (build-path cdir "README")
      (lambda ()
        (displayln COMMENT)
        (printf ";; ~a ~a\n" (filled "file") TYPED?)
        (displayln COMMENT)
        (displayln "(")
        (for ([file-name (in-list file-names)][typed? (in-list c)])
          (printf "(~a ~a)\n" (filled file-name) (if typed? "yes" "no"))
          (copy-file (build-path (if typed? typed untyped) file-name) (build-path cdir file-name))
          #;
          (when (string=? file-name "tree.rkt")
            ;; special case for Acquire:
            (copy-file
             (build-path "conditional" (if typed? typed untyped) "typed-wrapper.rkt")
             (build-path cdir "typed-wrapper.rkt"))))
        (displayln ")")))
    cdir))

(define TYPED? "typed?")
(define COMMENT (string-append ";; " (make-string 77 #\-)))

;; String -> String 
(define (filled s)
  (string-pad-right s (- 80 (string-length TYPED?))))

;; Path -> Void 
(define (set-up-benchmark-directory bdir)
  (when (directory-exists? bdir)
    (printf "Deleting ~a before setup~n" bdir)
    (delete-directory/files bdir))
  (make-directory bdir))

;; Path Path -> Void 
(define (create-populate-base-directory base bdir)
  (copy-directory/files base (build-path bdir "base") #:keep-modify-seconds? #t))

;; [Listof Path] [Listof Path] -> Void
(define (same-file-names? typed-dir untyped-dir)
  (unless (set=? (map path->string typed-dir) (map path->string untyped-dir))
    (error "Typed and Untyped directories do not contain the same file names.")))

;; [Listof X] [Listof Y] -> Void
(define (same-number-of-files? typed-dir untyped-dir)
  (unless (= (length typed-dir) (length untyped-dir))
    (error "Typed and Untyped directories do not have the same number of files")))

;; String *-> Boolean 
(define (files-exist? . dir*)
  (for ([dir (in-list dir*)])
    (unless (directory-exists? dir)
      (raise-user-error 'setup-benchmark "directory `~a' does not exist" dir))))

;; a combination is a (listof boolean) 
;; Integer -> [Listof Combination]
(define (build-combinations n)
  [when (< n 1) (error "No combinations of length 0")]
  (let build-combinations ([n n])
    (cond
      [(= n 1) (list (list #f) (list #t))]
      [else (let ([last (build-combinations (sub1 n))])
              (append (map (curry cons #f) last)
                      (map (curry cons #t) last)))])))

;; Combination -> Integer 
(define (combination->number combination)
  (let run ([combo (reverse combination)])
    (cond
      [(empty? combo) 0]
      [else (define result (run (rest combo)))
            (if (first combo) (+ 1 (* 2 result)) (* 2 result))])))


