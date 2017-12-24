#lang racket/base

;; Define & describe benchmark programs

(provide
  ;; -- all `define-benchmark`
  (struct-out benchmark)

  ALL-BENCHMARKS
  ;; (Listof Benchmark)

  benchmark-rktd
  ;; (-> Benchmark Version Path-String)

  benchmark-modulegraph
  ;; (-> Benchmark ModuleGraph)

  benchmark->num-configurations
  benchmark->num-modules
  ;; (-> Benchmark Natural)

  benchmark->num-iterations
  ;; (-> Benchmark Version Natural)

  benchmark->module-names
  ;; (-> Benchmark (Listof String))

  benchmark<?
  ;; (-> Benchmark Benchmark Boolean)

  (rename-out [format:bm bm])
  ;; (-> Benchmark Elem)
  ;; For formatting

  (all-from-out "jfp-parameters.rkt")

  NUM-POPL
)

(require
  (only-in racket/path
    find-relative-path)
  (only-in racket/list
    last)
  (only-in racket/file
    file->value)
  racket/serialize
  gtp-summarize/summary
  gtp-summarize/modulegraph
  (for-syntax racket/base syntax/parse)
  with-cache
  (only-in scribble/base tt
    hyperlink)
  (only-in "common.rkt"
    library)
  "jfp-parameters.rkt"
  "util.rkt"
)

;; =============================================================================

(define NUM-POPL
  (length '(sieve morsecode mbta zordoz suffixtree lnm kcfa snake tetris synth gregor quadMB)))

(struct benchmark (
  name         ;; Symbol, canonical name of benchmark, like `fsm`
  author       ;; Elem, original author of benchmark
  num-adaptor  ;; Natural, number of adaptor modules in the benchmark
  origin       ;; Elem, quick description of where the benchmark came from (educational, synthetic, lib)
  purpose      ;; String, short description of what the benchmark does
  lib*         ;; (U #f (Listof Elem)), external libraries the benchmark uses
  adjlist      ;; (Listof (Listof String)), module graph structure for the benchmark
  rktd*        ;; (Listof (Pairof Version Path-String)), map from Racket versions to datasets (.rktd files)
) #:prefab )

(define *ALL-BENCHMARKS* (box '()))

;; (->* (valid-benchmark? valid-version?) (#:tag string?) path-string?)
(define (data-path bm v [tag "*"])
  (define str (symbol->string bm))
  (define bm-str (if (eq? bm 'zordoz) (string-append str "." v) str))
  (define fname (format "~a-~a.rktd" bm-str tag))
  (define full-path (simplify-path (glob-first (path->string (build-path PAPER-DIRECTORY ".." ".." "data" v fname)))))
  (path->string (find-relative-path PAPER-DIRECTORY full-path)))

;; Resolve a list of unique modulegraphs to a single one.
(define (choose-modulegraph M* #:src name*)
  (cond
   [(null? M*)
    (raise-user-error 'choose-modulegraph "Failed to infer modulegraphs for '~a'." name*)]
   [(null? (cdr M*))
    (car M*)]
   [else
    (WARNING "got different modulegraphs for '~a'. Ignoring all but the last." name*)
    (last M*)]))

(define (name->modulegraph name)
  (if (eq? name 'zordoz)
    (project-name->modulegraph 'zordoz.6.3)
    (project-name->modulegraph name)))

(define (benchmark->num-iterations b v)
  (for/fold ([min-iters #f])
            ([x* (in-vector (file->value (benchmark-rktd b v)))])
    (define l (length x*))
    (if (or (not min-iters) (< l min-iters))
      l
      min-iters)))

(define (benchmark->num-configurations b)
  (expt 2 (benchmark->num-modules b)))

(define (benchmark->num-modules b)
  (length (benchmark-adjlist b)))

(define-syntax-rule (define-oo-benchmark stx* ...)
  (begin
    (*NUM-OO-BENCHMARKS* (+ 1 (*NUM-OO-BENCHMARKS*)))
    (define-benchmark stx* ...)))

(define-syntax (define-benchmark stx)
  (syntax-parse stx
   [(_ name:id parent:id)
    #'(begin
        (define name
          (make-benchmark 'name
                          (benchmark-author parent)
                          (benchmark-num-adaptor parent)
                          (benchmark-origin parent)
                          (benchmark-purpose parent)
                          (benchmark-lib* parent)))
        (register-benchmark! name)
        (provide name))]
   [(_ name:id #:author author
               #:num-adaptor num-adaptor:nat
               #:origin origin
               #:purpose purpose:str
               (~optional (~seq #:external-libraries maybe-lib*)))
    #:with lib* (or (attribute maybe-lib*) #'#f)
    #'(begin
        (define name
          (make-benchmark 'name author num-adaptor origin purpose lib*))
        (register-benchmark! name)
        (provide name))]))

(define (make-benchmark name author num-adaptor origin purpose lib*)
  (define cache-rktd (format "cache-bm-~a.rktd" name))
  (with-cache (build-path "cache" cache-rktd)
    #:read deserialize
    #:write serialize
    #:fasl? #false
    #:keys #f
    (lambda ()
      (define M
        (modulegraph-adjlist (name->modulegraph name)))
      (define rktd*
        (for/list ([v (in-list (*RKT-VERSIONS*))])
          (cons v (data-path name v))))
      (benchmark name author num-adaptor origin purpose lib* M rktd*))))

(define (register-benchmark! name)
  (set-box! *ALL-BENCHMARKS* (cons name (unbox *ALL-BENCHMARKS*)))
  (*NUM-BENCHMARKS* (+ 1 (*NUM-BENCHMARKS*)))
  (*TOTAL-NUM-CONFIGURATIONS* (+ (expt 2 (benchmark->num-modules name))
                                 (*TOTAL-NUM-CONFIGURATIONS*))))

(define-benchmark sieve
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin "Synthetic"
  #:purpose "Generate prime numbers"
)
(define-benchmark morsecode
  #:author "John Clements and Neil Van Dyke"
  #:num-adaptor 0
  #:origin (hyperlink "https://github.com/jbclements/morse-code-trainer/tree/master/morse-code-trainer" "Library")
  #:purpose "Morse code trainer"
)
(define-benchmark mbta
  #:author "Matthias Felleisen"
  #:num-adaptor 0
  #:origin "Educational"
  #:purpose "Interactive map"
  #:external-libraries (list (hyperlink "http://github.com/stchang/graph" (library "graph")))
)
(define-benchmark zordoz
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin (hyperlink "http://github.com/bennn/zordoz" "Tool")
  #:purpose "Explore Racket bytecode"
  #:external-libraries (list (hyperlink "http://docs.racket-lang.org/raco/decompile.html#%28mod-path._compiler%2Fdecompile%29" (library "compiler-lib")))
)
(define-benchmark suffixtree
  #:author "Danny Yoo"
  #:num-adaptor 1
  #:origin (hyperlink "https://github.com/dyoo/suffixtree" "Library")
  #:purpose "Ukkonen's suffix tree algorithm"
)
(define-benchmark lnm
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin "Synthetic"
  #:purpose "Graphing"
  #:external-libraries (list (hyperlink "https://docs.racket-lang.org/plot/" (library "plot"))
                             ", "
                             (hyperlink "https://docs.racket-lang.org/math/stats.html" (library "math/statistics")))
)
(define-benchmark kcfa
  #:author "Matt Might"
  #:num-adaptor 4
  #:origin (hyperlink "http://matt.might.net/articles/implementation-of-kcfa-and-0cfa/" "Educational")
  #:purpose "Explanation of k-CFA"
)
(define-benchmark zombie
  #:author "David Van Horn"
  #:num-adaptor 1
  #:origin (hyperlink "https://github.com/philnguyen/soft-contract" "Research")
  #:purpose "Game"
)
(define-benchmark snake
  #:author "David Van Horn"
  #:num-adaptor 1
  #:origin (hyperlink "https://github.com/philnguyen/soft-contract" "Research")
  #:purpose "Game"
)
(define-benchmark tetris
  #:author "David Van Horn"
  #:num-adaptor 1
  #:origin (hyperlink "https://github.com/philnguyen/soft-contract" "Research")
  #:purpose "Game"
)
(define-benchmark synth
  #:author "Vincent St. Amour \\& Neil Toronto"
  #:num-adaptor 1
  #:origin (hyperlink "http://github.com/stamourv/synth" "Application")
  #:purpose "Music synthesis DSL"
)
(define-benchmark gregor
  #:author "Jon Zeppieri"
  #:num-adaptor 2
  #:origin (hyperlink "https://docs.racket-lang.org/gregor/index.html" "Library")
  #:purpose "Date and time library"
  #:external-libraries
    (list (hyperlink "https://docs.racket-lang.org/cldr-core/index.html" (library "cldr"))
          ", "
          (hyperlink "https://docs.racket-lang.org/tzinfo/index.html" (library "tzinfo")))
)
(define-oo-benchmark dungeon
  #:author "Vincent St. Amour"
  #:num-adaptor 0
  #:origin "Application"
  #:purpose "Maze generator"
)
(define-oo-benchmark take5
  #:author "Matthias Felleisen"
  #:num-adaptor 1
  #:origin "Educational"
  #:purpose "Game"
)
(define-oo-benchmark forth
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin (hyperlink "http://docs.racket-lang.org/forth/index.html" "Library")
  #:purpose "Forth interpreter"
)
(define-oo-benchmark acquire
  #:author "Matthias Felleisen"
  #:num-adaptor 3
  #:origin (hyperlink "https://github.com/mfelleisen/Acquire" "Educational")
  #:purpose "Game"
)
(define-benchmark fsm
  #:author "Linh Chi Nguyen" ; and matthias
  #:num-adaptor 1
  #:origin (hyperlink "https://github.com/mfelleisen/sample-fsm" "Economics Research")
  #:purpose "Economy Simulator"
)
(define-oo-benchmark fsmoo fsm)
(define-benchmark quadBG
  #:author "Matthew Butterick"
  #:num-adaptor 2
  #:origin (hyperlink "https://github.com/mbutterick/quad" "Library")
  #:purpose "Typesetting"
  #:external-libraries (list (hyperlink "https://github.com/mbutterick/csp" (library "csp")))
)
(define-benchmark quadMB quadBG)

(define quad (gensym 'quad))
(provide quad)

;; -----------------------------------------------------------------------------
;; --- Assertions & Predicates

;; 2016-08-10 : could check against benchmark/ directory
;;; Like, zordoz.6.2 and zordoz.6.3 instead of zordoz
;;; (-> (Listof Symbol) Void)
;(define (check-missing-benchmarks name*)
;  (let loop ([expect* (sort BENCHMARK-NAMES name<?)]
;             [given*  (sort name* symbol<?)])
;    (cond
;     [(null? expect*)
;      (unknown-benchmark-error given*)]
;     [(null? given*)
;      (missing-benchmark-error (cdr expect*))]
;     [else
;      (define expect (car expect*))
;      (define given (car given*))
;      (cond
;       [(name<? expect given)
;        (loop (cdr expect*) given*)]
;       [(in-name? given expect)
;        (if (and (null? (cdr given*))
;                 (null? (cdr expect*)))
;            (void)
;            (loop expect* (cdr given*)))]
;       [(name<? expect given)
;        (missing-benchmark-error expect)]
;       [else
;        (unknown-benchmark-error given)])])))

(define (benchmark<? b1 b2)
  (define m1 (benchmark->num-modules b1))
  (define m2 (benchmark->num-modules b2))
  (or (< m1 m2)
      (and (= m1 m2)
           (symbol<? (benchmark-name b1) (benchmark-name b2)))))

(define (benchmark-rktd bm v)
  (assoc/fail v (benchmark-rktd* bm)))

(define (assoc/fail str pair*)
  (or
    (for/first ([p (in-list pair*)]
                #:when (equal? str (car p)))
      (cdr p))
    (raise-user-error 'assoc/fail "No match for '~s' in ~s" str pair*)))

(define missing-benchmark-error
  (let ([msg "Missing descriptions for benchmark(s) '~a'"])
    (lambda (name*)
      (if (*DRAFT?*)
        (WARNING msg name*)
        (raise-user-error 'benchmark msg name*)))))

(define unknown-benchmark-error
  (let ([msg "Got descriptions for unknown benchmarks '~a'. Register them at the top of 'typed-racket.rkt'"])
    (lambda  (name*)
      (if (*DRAFT?*)
        (WARNING msg name*)
        (raise-user-error 'benchmark msg name*)))))

(define (benchmark-modulegraph bm)
  (define pn
    (if (eq? (benchmark-name bm) 'zordoz)
      "zordoz.6.3"
      (symbol->string (benchmark-name bm))))
  (modulegraph pn
               (benchmark-adjlist bm)
               (build-path (get-git-root) "benchmarks" pn)))

(define (benchmark->module-names bm)
  (map car (benchmark-adjlist bm)))

(define (format:bm benchmark)
  (if (eq? benchmark quad)
    (tt "quad")
    (tt (symbol->string (benchmark-name benchmark)))))

(define ALL-BENCHMARKS (sort (unbox *ALL-BENCHMARKS*) benchmark<?))

;; =============================================================================

(module+ test
  (require rackunit rackunit-abbrevs racket/string)

  (test-case "data-path"
    (define (check-data-path b v)
      (let ([p (data-path b v)])
        (check-true (string-contains? p (format "data/~a/~a" v b)))))

    (check-data-path 'sieve "6.2")
    (check-data-path 'quadBG "6.4"))

  (test-case "choose-modulegraph"
    (check-exn #rx"choose-modulegraph"
      (lambda () (choose-modulegraph '() #:src '())))
    (let ([m* '(M1)])
      (check-equal?
        (choose-modulegraph m* #:src m*)
        'M1))
    (let* ([m* '(M1 M2)]
           [p (open-output-string)]
           [r (parameterize ([current-output-port p])
                (choose-modulegraph m* #:src m*))]
           [msg (begin0 (get-output-string p) (close-output-port p))])
      (check-equal? r 'M2)
      (check-true (string-contains? msg "WARNING"))))

  ;(test-case "name*->modulegraph"
  ;  (let ([zM (name*->modulegraph '(zordoz))])
  ;    (check-equal? (modulegraph-project-name zM) "zordoz.6.3")
  ;    (check-equal? (modulegraph->num-modules zM) 5))
  ;  (let* ([p (open-output-string)]
  ;         [fM (parameterize ([current-output-port p])
  ;               (name*->modulegraph '(fsm fsmoo)))]
  ;         [msg (begin0 (get-output-string p) (close-output-port p))])
  ;    (check-equal? (modulegraph-project-name fM) "fsm")
  ;    (check-equal? (modulegraph->num-modules fM) 4)
  ;    (check-true (string-contains? msg "WARNING"))))

  (test-case "benchmark->num-modules"
    (check-apply* benchmark->num-modules
     [sieve   => 2]
     [forth   => 4]
     [dungeon => 5]
     [kcfa    => 7]
     [snake   => 8]
     [take5   => 8]
     [acquire => 9]
     [tetris  => 9]
     [gregor  => 13]
     [quadBG  => 14]))

  (test-case "num-benchmarks"
    (check-equal? (*NUM-OO-BENCHMARKS*) 5)
    (check-equal? (*NUM-BENCHMARKS*) 20)
    (check-equal? (length ALL-BENCHMARKS) 20)
    (check-equal? (*TOTAL-NUM-CONFIGURATIONS*) 43940))

  (test-case "define-benchmark"
    (check-apply* benchmark-name
     [sieve => 'sieve]
     [morsecode => 'morsecode]
     [quadMB => 'quadMB]
     [zordoz => 'zordoz]
     [acquire => 'acquire])

    (check-apply* benchmark-author
     [sieve => "Ben Greenman"]
     [morsecode => "John Clements and Neil Van Dyke"]
     [kcfa => "Matt Might"]
     [dungeon => "Vincent St. Amour"]
    )

    (check-apply* benchmark-num-adaptor
     [sieve => 0]
     [suffixtree => 1]
     [synth => 1]
     [kcfa => 4])

    (check-apply* benchmark-origin
     [sieve => "Synthetic"]
     [mbta => "Educational"]
    )

    (check-apply* benchmark-purpose
     [take5 => "Card game"]
     [synth => "Music synthesis DSL"]
    )
  )

  (test-case "benchmark-lib*"

    (define (lib*-length bm)
      (let ([lib* (benchmark-lib* bm)])
        (and lib* (length lib*))))

    (check-apply* lib*-length
     [sieve => #f]
     [mbta => 1]
     [zordoz => 1]
     [suffixtree => #f]
     [lnm => 3]
     [zombie => #f]
     [gregor => 3]
     [quadBG => 1])
  )

  (test-case "benchmark-adjlist"
    (check-equal?
      (length (benchmark-adjlist synth))
      10)

    (check-equal?
      (length (benchmark-adjlist suffixtree))
      6)
  )

  (test-case "benchmark-rktd*"
    (let ([rktd* (benchmark-rktd* zordoz)])
      (check-equal? (length rktd*) (length (*RKT-VERSIONS*))))
  )

  (test-case "num-configurations"
    (check-apply* benchmark->num-configurations
     [suffixtree => 64]
     [kcfa => 128]
     [sieve => 4]
     [synth => 1024]))

  (test-case "num-modules"
    (check-apply* benchmark->num-modules
     [suffixtree => 6]
     [kcfa => 7]
     [sieve => 2]
     [synth => 10]))

  (test-case "num-iterations"
    (check-apply* benchmark->num-iterations
     [suffixtree "6.2" => 10]))

  (test-case "benchmark<?"
    (let* ([b* (list mbta sieve quadMB)]
           [b+* (sort b* benchmark<?)])
      (check-equal?
        (map benchmark-name b+*)
        '(sieve mbta quadMB)))
    (let* ([b* (list zombie forth fsm take5)]
           [b+* (sort b* benchmark<?)])
      (check-equal?
        (map benchmark-name b+*)
        '(forth fsm zombie take5))))

  (test-case "benchmark->module-names"
    (check-equal?
      (benchmark->module-names sieve)
      '("main" "streams"))
    (check-equal?
      (benchmark->module-names kcfa)
      '("ai" "benv" "denotable" "main" "structs" "time" "ui")))

)
