#lang racket/base

;; Supporting code for `typed-racket.scrbl`
;; - Render & organize benchmarks
;; - Make L-N/M figures

(provide
  EXAMPLE-BENCHMARK ;; Symbol
  EXAMPLE-OVERHEAD  ;; Natural
  ;; For discussion / prose

  MAX-OVERHEAD
  ;; Natural
  ;; To generate L-N/M figures

  NUM-NEW-OO
  NUM-BENCHMARKS
  ;; Natural
  ;; Not really a constant -- depends on `benchmark-name*`

  ;; ---------------------------------------------------------------------------

  bits
  ;; (-> String Any)
  ;; Use to format bitstrings

  bm
  ;; (-> String Any)
  ;; Use to format benchmark names.
  ;; Asserts that its argument is a correctly-spelled benchmark name.

  data-lattice
  ;; (-> Benchmark-Name Version-String Any)
  ;;   where Benchmark-Name = (U String Symbol)
  ;;     and Version-String = String
  ;; Finds the data file corresponding to the benchmark
  ;;  at the specific version and renders a data lattice.

  (rename-out [make-benchmark benchmark])
  ;; (->* [] [#:name String
  ;;          #:author String
  ;;          #:num-adaptor Natural
  ;;          #:origin String
  ;;          #:purpose String
  ;;          #:external-libraries (Listof String)]
  ;;     #:rest (Listof String)
  ;;     Benchmark)

  benchmark-descriptions
  ;; (-> Benchmark * Any)
  ;; Render a list of Benchmark structures.
  ;; Use the `benchmark` constructor to make a `Benchmark`

  lnm-descriptions
  ;; (-> Lnm * Any)

  (rename-out [make-lnm lnm])
  ;; (->* [Symbol] [] #:rest (Listof String) Lnm)

  benchmark-characteristics
  ;; (-> Any)
  ;; 

  lnm-plots
  ;; (-> String * Any)

  lnm-summary
  ;; (-> String * Any)
  ;; Create a summary table for all versions of Racket
)

(require
 glob
 gtp-summarize/render-lnm
 gtp-summarize/modulegraph
 racket/match
 (only-in racket/file file->value)
 (only-in "common.rkt" exact parag)
 scribble/core
 scribble/base
 version/utils
)

;; -----------------------------------------------------------------------------
;; -- Organizing the benchmarks

;; TODO precompute modulegraphs?

(define benchmark-name* '(
  acquire
  forth
  (fsm fsm fsmoo)
  gregor
  kcfa
  lnm
  mbta
  morsecode
  (quad quadBG quadMB)
  sieve
  snake
  suffixtree
  synth
  tetris
  zombie
  (zordoz zordoz.6.2 zordoz.6.3)
))
(define NUM-BENCHMARKS
  (for/sum ([name (in-list benchmark-name*)])
    (if (list? name)
      (length (cdr name))
      1)))
(define NUM-NEW-OO
  (length '(acquire forth fsmoo)))

(define MAX-OVERHEAD 20)
(define NUM-SAMPLES 120)
(define EXAMPLE-BENCHMARK 'kcfa)
(define EXAMPLE-OVERHEAD 10)

;; -----------------------------------------------------------------------------

(define bits tt)

(define (bm name)
  (define name-sym (string->symbol name))
  (unless (for/or ([b (in-list benchmark-name*)])
            (if (list? b)
              (memq name-sym b)
              (eq? name-sym b)))
    (unknown-benchmark-error name))
  (tt name))

(define (data-lattice bm-raw v #:tag [tag "*"])
  (unless (valid-version? v)
    (raise-user-error 'data-path "Invalid version string '~a'" v))
  (define bm (if (string? bm) (string->symbol bm-raw) bm-raw))
  (unless (memq bm benchmark-name*)
    (unknown-benchmark-error bm))
  (raise-user-error 'NOT-IMPLEMENTED)
  ;@(let* ([vec (file->value (data-path 'fsm "6.2"))]
  ;        [vec* (vector-map (λ (p) (cons (mean p) (stddev p))) vec)])
  ;   (make-performance-lattice vec*))
  ;;; --- do search
  ;(define path-str (format "~a/data/~a/~a*~a*.rktd" (get-git-root) v bm tag))
  ;(match (glob path-str)
  ; [(cons p '())
  ;  p]
  ; ['()
  ;  (raise-user-error 'data-path "No matches for '~a'" path-str)]
  ; [p*
  ;  (raise-user-error 'data-path "Path '~a' returned multiple results. Try again with a #:tag parameter to filter the search: (data-path BENCHMARK VERSION #:tag STR).\n    All results: ~a" path-str p*)]))
)

;; -----------------------------------------------------------------------------

(struct benchmark (name author num-adaptor origin purpose lib* description))
(define (make-benchmark #:name name
                        #:author author
                        #:num-adaptor num-adaptor
                        #:origin origin
                        #:purpose purpose
                        #:external-libraries [lib* #f]
                        description)
  (benchmark name author num-adaptor origin purpose lib* description))

;; (-> benchmark String)
(define (render-benchmark b)
  (match-define
    (benchmark name author num-adaptor origin purpose lib* description)
    b)
  ;; TODO render lib*, hard because it's an optional list
  (format "\\benchmark{~a}{~a}{~a}{~a}{~a}" name author origin purpose description))
  ;(paragraph plain
  ; (list
  ;  (elem #:style 'bold (symbol->string name))
  ;  (element 'newline "")
  ;  (list "Author : " author)
  ;  (element 'newline "")
  ;  (list "Origin : " origin)
  ;  (element 'newline "")
  ;  (list "Purpose : " purpose)
  ;  (element 'newline "")
  ;  (append
  ;   (if lib*
  ;     (list "External Libraries: " lib*
  ;           (element 'newline ""))
  ;     '())
  ;   (list description)))))

;; (-> Symbol Natural)
(define (benchmark-num-modules name)
  ;; TODO implement
  0)

(define (benchmark<? name1 name2)
  (< (benchmark-num-modules name1)
     (benchmark-num-modules name2)))

(define (missing-benchmark-error name*)
  (raise-user-error 'benchmark
                    "Missing descriptions for benchmark(s) '~a'" name*))

(define (unknown-benchmark-error name*)
  (raise-user-error 'benchmark
    "Got descriptions for unknown benchmarks '~a'. Register them at the top of 'typed-racket.rkt'" name*))


;; If exact?, use the specific names and not the 'umbrella' benchmark names.
;; (-> Boolean (Listof Symbol))
(define (flatten-benchmark-name* exact?)
  (for/fold ([acc '()])
            ([name (in-list benchmark-name*)])
    (if (list? name)
      (if exact?
        (append (cdr name) acc)
        (cons (car name) acc))
      (cons name acc))))

;; Like, zordoz.6.2 and zordoz.6.3 instead of zordoz
;; (-> (Listof Symbol) Void)
(define (check-missing-benchmarks name* #:exact? [exact? #f])
  (let loop ([expect* (sort (flatten-benchmark-name* exact?) symbol<?)]
             [given*  (sort name* symbol<?)])
    (cond
     [(null? expect*)
      (if (null? given*)
        (void)
        (unknown-benchmark-error given*))]
     [(null? given*)
      (missing-benchmark-error expect*)]
     [else
      (define expect (car expect*))
      (define given (car given*))
      (cond
       [(eq? expect given)
        (loop (cdr expect*) (cdr given*))]
       [(symbol<? expect given)
        (missing-benchmark-error expect)]
       [else
        (unknown-benchmark-error given)])])))

;; (-> Benchmark * Any)
(define (benchmark-descriptions . b*)
  ;(check-missing-benchmarks (map benchmark-name b*))
  (define b+*
    (for/fold ([acc ""])
              ([b (in-list (sort b* benchmark<? #:key benchmark-name))])
      (string-append acc "\n" (render-benchmark b))))
  (exact b+*))

(define (benchmark-characteristics)
  (elem "TODO"))

  ;(define MG (project-name->modulegraph MG))
  ;(define loc (modulegraph->untyped-loc MG))
  ;(define ann-loc (modulegraph->ann-loc MG))
  ;(define other-loc (modulegraph->other-loc MG))
  ;(define num-modules (modulegraph->num-modules MG))

;; -----------------------------------------------------------------------------

(struct lnm (name description))
(define (make-lnm name . descr*)
  (lnm name (apply elem descr*)))

;; (-> Lnm * Any)
(define (lnm-descriptions . l*)
  (check-missing-benchmarks (map lnm-name l*) #:exact? #t)
  (map render-lnm-description (sort l* benchmark<? #:key lnm-name)))

(define (render-lnm-description l)
  (elem
    (parag (symbol->string (lnm-name l)))
    (elem (lnm-description l))))

(define (lnm-plots . version*)
  ;; Map over benchmark names,
  ;; Sort & make figures of with 6 plots each or whatever
  (elem "TODO"))
;@figure*["fig:lnm1"
;  @list{@step["L" "N" "M"] results for the first six benchmarks}
;  @(let* ([data `(("sieve"        ,SIEVE-DATA)
;                  ("morse-code"   ,MORSECODE-DATA)
;                  ("mbta"         ,MBTA-DATA)
;                  ("zordoz"       ,ZORDOZ-DATA)
;                  ("suffixtree"   ,SUFFIXTREE-DATA)
;                  ("lnm"          ,LNM-DATA)
;                  )])
;     (data->pict data #:tag "1"))
;]
;
;@figure*["fig:lnm2"
;  @list{@step["L" "N" "M"] results for the remaining benchmarks}
;  @(let* ([data `(("kcfa"       ,KCFA-DATA)
;                  ("snake"      ,SNAKE-DATA)
;                  ("tetris"     ,TETRIS-DATA)
;                  ("synth"      ,SYNTH-DATA)
;                  ("gregor"     ,GREGOR-DATA)
;                  ("quad"       ,QUAD-DATA))])
;     (data->pict data #:tag "2"))
;]

(define (lnm-summary . version*)
  (elem "TODO"))

