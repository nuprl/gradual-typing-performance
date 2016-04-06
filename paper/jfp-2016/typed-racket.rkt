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
 (only-in "common.rkt" etal cite exact parag)
 scribble/core
 scribble/base
 version/utils
)

;; -----------------------------------------------------------------------------
;; -- Organizing the benchmarks

(define *MODULE-GRAPH-DIR* "./module-graphs")
;; Place to store module graphs

(define *STRICT?* (make-parameter #f))
;; When #t, raise exceptions instead of warnings

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

(define benchmark-data* (box #f))
;; List of benchmarks used in this pdf.
;; Is validated against `benchmark-name*` before set;
;;  later used to generate summary table and L-NM plots

;; -----------------------------------------------------------------------------

(define bits tt)

(define (assert-benchmark name-sym)
  (unless (for/or ([b (in-list benchmark-name*)])
            (if (list? b)
              (memq name-sym b)
              (eq? name-sym b)))
    (unknown-benchmark-error name-sym)))

(define (bm name)
  (assert-benchmark (string->symbol name))
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
  (assert-benchmark name)
  (benchmark (symbol->string name) author num-adaptor origin purpose lib* description))

(define missing-benchmark-error
  (let ([msg "Missing descriptions for benchmark(s) '~a'"])
    (lambda (name*)
      (if (*STRICT?*)
        (raise-user-error 'benchmark msg name*)
        (begin (printf "WARNING: ")
               (printf msg name*))))))

(define unknown-benchmark-error
  (let ([msg "Got descriptions for unknown benchmarks '~a'. Register them at the top of 'typed-racket.rkt'"])
    (lambda  (name*)
      (if (*STRICT?*)
        (raise-user-error 'benchmark msg name*)
        (begin (printf "WARNING: ")
               (printf msg name*))))))

;; (-> benchmark String)
(define (render-benchmark b)
  (match-define
    (benchmark name author num-adaptor origin purpose lib* description)
    b)
  ;; TODO render lib*, hard because it's an optional list
  (elem "\\benchmark{" name "}{" author "}{" origin "}{" purpose "}{" description "}"))

;; (-> Symbol Natural)
(define (benchmark-num-modules name)
  ;; TODO implement
  0)

(define (benchmark<? name1 name2)
  (< (benchmark-num-modules name1)
     (benchmark-num-modules name2)))

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
;; (-> (Listof String) Void)
(define (check-missing-benchmarks name* #:exact? [exact? #f])
  (let loop ([expect* (sort (flatten-benchmark-name* exact?) symbol<?)]
             [given*  (sort (map string->symbol name*) symbol<?)])
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
  (check-missing-benchmarks (map benchmark-name b*))
  (set-box! benchmark-data* b*)
  (apply exact (map render-benchmark (sort b* benchmark<? #:key benchmark-name))))

;; Format a table of benchmark characteristics.
;; Assumes that `benchmark-data*` has been populated (i.e. we called `benchmark-descriptions`)
;; (-> Content)
;; TODO
;; - use b name, not modulegraph name
;; - compile 6.2
;; - group adaptor with modules
;; - give typed % increase
;; - actual modulegraphs
;; - tt titles
;; - wtf mbta
(define (benchmark-characteristics)
  (tabular
   #:sep (hspace 2)
   #:row-properties '(bottom-border ())
   #:column-properties '(left right)
   (cons
     (list "Benchmark" "#M" "#A" "Untyped LOC" "Type Ann. LOC" "Module Graph")
     (for*/list ([b (in-list (unbox benchmark-data*))]
                 [M (in-list (benchmark-modulegraph* b))])
       (define uloc (modulegraph->untyped-loc M))
       (define tloc (modulegraph->typed-loc M))
       (list
        (benchmark-name b)
        (number->string (modulegraph->num-modules M))
        (number->string (benchmark-num-adaptor b))
        (number->string uloc)
        (number->string (- tloc uloc))
        "f" #;(benchmark-tex M))))))

(define (benchmark-modulegraph* b)
  (define name (benchmark-name b))
  (define name-sym (string->symbol name))
  (map project-name->modulegraph
    (or (for/or ([s (in-list benchmark-name*)])
          (cond
           [(and (symbol? s) (eq? name-sym s))
            (list name)]
           [(and (list? s) (eq? name-sym (car s)))
            (if (eq? name-sym 'zordoz)
              (list "zordoz.6.3")
              (map symbol->string (cdr s)))]
           [else
            #f]))
        (raise-user-error 'benchmark-modulgraph "Unknown benchmark name '~a'." name))))

(define (benchmark-tex M)
  (define mgd (*MODULE-GRAPH-DIR*))
  (unless (directory-exists? mgd)
    (make-directory mgd))
  (define pn (modulegraph-project-name M))
  (define mgf (string-append mgd "/" pn ".tex"))
  (unless (file-exists? mgf)
    (printf "WARNING: could not find modulegraph for project '~a', creating graph now.\n" pn)
    (call-with-output-file mgf
      (lambda (p) (modulegraph->tex M p))))
  (exact "\\input{" mgf "}"))

;; -----------------------------------------------------------------------------

(struct lnm (name description))
(define (make-lnm name . descr*)
  ;; TODO assert?
  (lnm (symbol->string name) (apply elem descr*)))

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

