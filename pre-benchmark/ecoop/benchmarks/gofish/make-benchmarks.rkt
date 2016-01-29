#lang racket
;(require benchmark)
;(require compiler/compile-file)
(provide create-benchmark-dirs)

#;(define (benchmark-typed-untyped typed-files 
                                 benchmark-dirs
                                 #:arg [arg "1"] ;temp
                                 #:num-trials [num-trials 2])
  (printf "Typed Files:\n~a\n" typed-files)
  ;; run benchmark
  (run-benchmarks benchmark-dirs
                  (list (list arg)) ; fix later
                  (lambda (dir arg)
                    (define main (dynamic-require (string-append dir "/" "tree-game.rkt") 'main))
                    (time (main arg)))
                  #:num-trials num-trials))
    


(define (create-benchmark-dirs #:base [base "Base"]
                               #:typed [typed "Typed"]
                               #:untyped [untyped  "Untyped"]
                               #:dir-base-name [name "Variation"]
                               #:benchmark-dir [bdir "Benchmark-Folders"])
                               
  (unless (andmap directory-exists? (list base typed untyped))
    (error "Invalid Directory."))
  
  (define typed-dir (directory-list typed))
  (define untyped-dir (directory-list untyped))
  
  
  (unless (= (length typed-dir) (length untyped-dir))
    (error "Typed and Untyped directories do not have the same number of files"))
  
  (unless (set=? (map path->string typed-dir)
                 (map path->string untyped-dir))
    (error "Typed and Untyped directories do not contain the same file names."))
  
  (define file-names (map path->string typed-dir))
  (define num-files (length file-names))
  
  (define combinations (build-combinations num-files))
  ;; make benchmark directory
  (make-directory (string-append "./" bdir))
  (list file-names 
        (for/list ([combo (in-list combinations)])
          ; build target directory
          (define var-dir-num (number->string (combination->number combo)))
          (define cdir (string-append "./" bdir "/" name var-dir-num))
          (copy-directory/files (string-append "./" base)
                                cdir
                                #:keep-modify-seconds? #t)
          (for ([item (in-list (zip combo file-names))])
            (copy-file
             (string-append "./" (if (first item) typed untyped) "/" (second item))
             (string-append cdir "/" (second item)))
            (when (string=? (second item) "tree.rkt")
              (copy-file
               (string-append "./Conditional/" (if (first item) typed untyped) "/typed-wrapper.rkt")
               (string-append cdir "/typed-wrapper.rkt"))))
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
  
                               