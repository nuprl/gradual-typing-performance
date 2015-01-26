#lang racket

(provide create-benchmark-dirs)
    
;; Usage:
;;   racket setup-benchmark.rkt
;;   racket setup-benchmark.rkt <directory>
;; will set up benchmarks using the current directory or the
;; provided one
(module+ main
  (match (current-command-line-arguments)
    [(vector path)
     (create-benchmark-dirs path)]
    [_ (create-benchmark-dirs (current-directory))]))

(define (create-benchmark-dirs pwd
                               #:base [*base "base"]
                               #:typed [*typed "typed"]
                               #:untyped [*untyped "untyped"]
                               #:dir-base-name [name "variation"]
                               #:benchmark-dir [*bdir "benchmark"])
  (define-values (base typed untyped bdir)
    (values (build-path pwd *base)
            (build-path pwd *typed)
            (build-path pwd *untyped)
            (build-path pwd *bdir)))

  (for ([dir (in-list (list base typed untyped))])
    (unless (directory-exists? dir)
      (raise-user-error "directory ~a does not exist" dir)))

  (define typed-dir (directory-list typed))
  (define untyped-dir (directory-list untyped))
  
  (unless (= (length typed-dir) (length untyped-dir))
    (error "Typed and Untyped directories do not have the same number of files"))

  (unless (set=? (map path->string typed-dir)
                 (map path->string untyped-dir))
    (error "Typed and Untyped directories do not contain the same file names."))

  (define file-names (for/list ([path (in-list typed-dir)]
                                #:when (regexp-match? #rx"\\.rkt$|\\.ss$" path))
                       (path->string path)))
  (define num-files (length file-names))

  (define combinations (build-combinations num-files))
  ;; make benchmark directory
  (make-directory bdir)
  (list file-names 
        (for/list ([combo (in-list combinations)])
          ; build target directory
          (define var-dir-num (number->string (combination->number combo)))
          (define cdir (build-path bdir (string-append name var-dir-num)))
          (copy-directory/files base
                                cdir
                                #:keep-modify-seconds? #t)
          (for ([item (in-list (zip combo file-names))])
            (copy-file
             (build-path (if (first item) typed untyped) (second item))
             (build-path cdir (second item)))
            ;; special case for acquire
            #;(when (string=? (second item) "tree.rkt")
              (copy-file
               (build-path "conditional" (if (first item) typed untyped) "typed-wrapper.rkt")
               (build-path cdir "typed-wrapper.rkt"))))
          cdir)))  

;; zip
(define (zip l1 l2) (map list l1 l2))

;; a combination is a (listof boolean) 
;; build-combinations : integer -> (listof combination)
(define (build-combinations n)
  (cond
    [(< n 1) (error "No combinations of length 0")]
    [(= n 1) (list (list #f) (list #t))]
    [else (let ([last (build-combinations (sub1 n))])
            (append (map (curry cons #f) last)
                    (map (curry cons #t) last)))]))

(define (combination->number combination)
  (define rcombo (reverse combination))
  (define (run combo)
    (cond
      [(empty? combo) 0]
      [else (let ([result (run (rest combo))])
              (if (first combo)
                  (+ 1 (* 2 result))
                  (* 2 result)))]))
  (run rcombo))
  
                               
