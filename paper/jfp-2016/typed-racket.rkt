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

  add-commas
  ;; (-> Number String)

  bits
  ;; (-> String Any)
  ;; Use to format bitstrings

  bm
  ;; (-> String Any)
  ;; Use to format benchmark names.
  ;; Asserts that its argument is a correctly-spelled benchmark name.

  count-all-configurations
  ;; (-> Natural)

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
 benchmark-util/data-lattice
 glob
 gtp-summarize/render-lnm
 gtp-summarize/modulegraph
 racket/match
 (only-in racket/file file->value)
 (only-in "common.rkt" etal cite exact parag)
 (only-in racket/list last)
 scribble/core
 scribble/base
 version/utils
 ;;
 racket/contract
)
(require (only-in racket/serialize
  serialize deserialize
))

;; -----------------------------------------------------------------------------
;; -- Organizing the benchmarks

(define *CACHE-BENCHMARKS-TABLE?* (make-parameter #t))
;; TODO fix this

(define *COMPILED* (make-parameter "./compiled"))
;; Where Racket stores compiled files

(define *BENCHMARK-DESCRIPTIONS-CACHE* (make-parameter "cache-benchmark-descriptions.rktd"))
;; Place to store cached benchmarks table

(define *MODULE-GRAPH-DIR* (make-parameter "./module-graphs"))
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

(define (valid-benchmark? bm)
  (for/or ([b (in-list benchmark-name*)])
    (if (list? b)
      (memq bm b)
      (eq? bm b))))

(define (assert-benchmark name-sym)
  (unless (valid-benchmark? name-sym)
    (unknown-benchmark-error name-sym)))

(define (bm name)
  (assert-benchmark (string->symbol name))
  (tt name))

(define (ensure-dir d)
  (unless (directory-exists? d)
    (make-directory d)))

(define (glob/first str)
  (match (glob str)
   [(cons r '())
    r]
   ['()
    (raise-user-error 'glob/first "No results for glob '~a'" str)]
   [r*
    (printf "WARNING: ambiguous results for glob '~a'. Returning the first.\n" str)
    (car r*)]))

(define (add-commas n)
  (define str (number->string n))
  (define L (string-length str))
  (apply string-append
    (let loop ([i L]
               [acc '()])
      (let ([i-3 (- i 3)])
        (cond
         [(<= i-3 0)
          (cons (substring str 0 i) acc)]
         [else
          (loop i-3 (cons "," (cons (substring str i-3 i) acc)))])))))

;; -----------------------------------------------------------------------------

(define (lattice-filename bm v tag)
  (string-append (*COMPILED*) "/cache-lattice-" (symbol->string bm) "-" v "-" tag ".rktd"))

(define (data-lattice bm v #:tag [tag "*"] #:cache? [cache? #t])
  (or (and cache? (uncache-lattice bm v tag))
      (let* ([fname (lattice-filename bm v tag)]
             [_void (printf "INFO: Building new performance lattice at '~a'\n" fname)]
             [p (file->performance-lattice (data-path bm v #:tag tag))])
        (with-output-to-file fname #:exists 'replace
          (lambda () (writeln (serialize p))))
        p)))

(define/contract (data-path bm v #:tag [tag "*"])
  (->* (valid-benchmark? valid-version?) (#:tag string?) path-string?)
  (define bm-str (symbol->string bm))
  (glob/first (string-append (get-git-root) "/data/" v "/" bm-str "-" tag ".rktd")))

;; -----------------------------------------------------------------------------

(struct benchmark (name author num-adaptor origin purpose lib* description modulegraph))
(define (make-benchmark #:name name
                        #:author author
                        #:num-adaptor num-adaptor
                        #:origin origin
                        #:purpose purpose
                        #:external-libraries [lib* #f]
                        description)
  (assert-benchmark name)
  (define M (symbol->modulegraph name))
  (benchmark name author num-adaptor origin purpose lib* description M))

(define missing-benchmark-error
  (let ([msg "Missing descriptions for benchmark(s) '~a'"])
    (lambda (name*)
      (if (*STRICT?*)
        (raise-user-error 'benchmark msg name*)
        (begin (printf "WARNING: ")
               (printf msg name*)
               (newline))))))

(define unknown-benchmark-error
  (let ([msg "Got descriptions for unknown benchmarks '~a'. Register them at the top of 'typed-racket.rkt'"])
    (lambda  (name*)
      (if (*STRICT?*)
        (raise-user-error 'benchmark msg name*)
        (begin (printf "WARNING: ")
               (printf msg name*)
               (newline))))))

;; (-> benchmark String)
(define (render-benchmark b)
  (match-define
    (benchmark name author num-adaptor origin purpose lib* description M)
    b)
  (elem
    "\\benchmark{"
    (symbol->string name)
    "}{"
    author
    "}{"
    origin
    "}{"
    purpose
    "}{"
    description
    "}{"
    (benchmark-tex M)
    "}{"
    (or (benchmark-lib* b) "N/A")
    "}\n\n"))

(define (benchmark->num-modules b)
  (modulegraph->num-modules (benchmark-modulegraph b)))

(define (count-all-configurations)
  (for/sum ([b (in-list (unbox benchmark-data*))])
    (expt 2 (benchmark->num-modules b))))

(define (benchmark<? b1 b2)
  (< (benchmark->num-modules b1)
     (benchmark->num-modules b2)))

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
  (check-missing-benchmarks (map benchmark-name b*))
  (set-box! benchmark-data* b*)
  (apply exact (map render-benchmark (sort b* benchmark<?))))

(define BENCHMARK-DESCRIPTIONS-TITLE* '(
  "Benchmark"
  "\\twoline{Untyped}{LOC}"
  "\\twoline{Annotation}{LOC}"
  "\\twoline{\\# Modules}{(Adaptors)}"
  "\\# Boundaries"
  "\\# Exports"
))

;; Format a table of benchmark characteristics.
;; Assumes that `benchmark-data*` has been populated (i.e. we called `benchmark-descriptions`)
;; (-> Content)
(define (benchmark-characteristics)
  (exact
    "\\setlength{\\tabcolsep}{0.2em}"
    (format "\\begin{tabular}{l~a}\n" (make-string (sub1 (length BENCHMARK-DESCRIPTIONS-TITLE*)) #\r))
    (apply elem (get-benchmarks-table))
    "\\end{tabular}\n\n"))

(define (get-benchmarks-table)
  (or (and (*CACHE-BENCHMARKS-TABLE?*) (uncache-benchmarks-table))
      (cache-table (new-benchmarks-table))))

(define (benchmark-descriptions-cache)
  (build-path (*COMPILED*) (*BENCHMARK-DESCRIPTIONS-CACHE*)))

(define/contract (uncache-lattice bm v tag)
  (-> valid-benchmark? valid-version? string? any/c)
  (define fname (lattice-filename bm v tag))
  (and (file-exists? fname)
       (printf "INFO: retrieving cached lattice from '~a'\n" fname)
       (deserialize (file->value fname))))

(define (uncache-benchmarks-table)
  (define bdc (benchmark-descriptions-cache))
  (and (file-exists? bdc)
       (let ([tag+data (file->value bdc)])
         (and (equal? (car tag+data) benchmark-name*)
              (printf "INFO: retrieving cached benchmarks table from '~a'\n" bdc)
              (cdr tag+data)))))

(define (cache-table T)
  (ensure-dir (*COMPILED*))
  (define bdc (benchmark-descriptions-cache))
  (printf "INFO: caching new benchmarks table at '~a'\n" bdc)
  (with-output-to-file bdc #:exists 'replace
    (lambda ()
      (writeln (cons benchmark-name* T))))
  T)

(define (new-benchmarks-table)
  (list*
    " \\toprule \n"
    (apply tex-row BENCHMARK-DESCRIPTIONS-TITLE*)
    " \\midrule \n"
    (for/list ([b (in-list (unbox benchmark-data*))])
      (define M (benchmark-modulegraph b))
      (define num-adaptor (benchmark-num-adaptor b))
      (define uloc (modulegraph->untyped-loc M))
      (define tloc (modulegraph->typed-loc M))
      (tex-row
       (format "{\\tt ~a}" (benchmark-name b))
       (format "~a" uloc)
       (format-percent-diff tloc uloc)
       (format-num-modules M #:adaptor num-adaptor)
       (number->string (modulegraph->num-edges M))
       (number->string (modulegraph->num-identifiers M))
      ))))

;; Add '&' for TeX
(define (tex-row . x*)
  (let loop ([x* x*])
    (if (null? (cdr x*))
      (list (car x*) " \\\\\n")
      (list* (car x*) " & " (loop (cdr x*))))))

(define (format-num-modules M #:adaptor [adaptor #f])
  (if adaptor
    (format "~a\\,~~(~a)"
      (modulegraph->num-modules M)
      adaptor)
    (number->string (modulegraph->num-modules M))))

(define (format-percent-diff meas exp)
  (define diff (- meas exp))
  (define pct (round (* 100 (/ diff exp))))
  (format "~a~~~~~a(~a\\%)"
    diff
    (if (< pct 10) "\\hphantom{0}" "")
    pct))

(define (benchmark->name* b)
  (project-name->name* (benchmark-name b)))

(define (project-name->name* name)
  (or (for/first ([s (in-list benchmark-name*)]
                 #:when (or (and (symbol? s) (eq? s name))
                            (and (list? s) (eq? name (car s)))))
        s)
      (unknown-benchmark-error name)))

(define (benchmark->modulegraph b)
  (symbol->modulegraph (benchmark-name b)))

(define (symbol->modulegraph n)
  (define name* (project-name->name* n))
  (cond
   [(not (list? name*))
    ;; Easy! Compile & return the modulegraph.
    (project-name->modulegraph name*)]
   [else
    ;; Hard!
    ;; - Compile all names in `cdr` to modulegraphs. If they fail, ignore them.
    ;; - Filter duplicate modulegraphs from the list.
    ;; - Return only a single modulegraph
    (choose-modulegraph #:src name*
      (for/fold ([acc '()])
                ([name (in-list (cdr name*))])
        ;; Try compiling a modulegraph, don't worry if it fails.
        (define M (with-handlers ([exn:fail? (lambda (e) #f)])
                    (project-name->modulegraph name)))
        (if (and M (not (for/or ([M2 (in-list acc)])
                          (adjlist=? (modulegraph-adjlist M) (modulegraph-adjlist M2)))))
          (cons M acc)
          acc)))]))

;; Resolve a list of unique modulegraphs to a single one.
(define (choose-modulegraph M* #:src name*)
  (cond
   [(null? M*)
    (raise-user-error 'choose-modulegraph "Failed to infer modulegraphs for '~a'." name*)]
   [(null? (cdr M*))
    (car M*)]
   [else
    (printf "WARNING: got different modulegraphs for '~a'. Ignoring all but the last.\n" name*)
    (last M*)]))

(define (benchmark-tex M)
  (define mgd (*MODULE-GRAPH-DIR*))
  (ensure-dir mgd)
  (define pn (modulegraph-project-name M))
  (define mgf (string-append mgd "/" pn ".tex"))
  (unless (file-exists? mgf)
    (printf "WARNING: could not find modulegraph for project '~a', creating graph now.\n" pn)
    (call-with-output-file mgf
      (lambda (p) (modulegraph->tex M p))))
  (elem "\\modulegraph{" mgf "}"))

;; =============================================================================

(struct lnm (name description))
(define (make-lnm name . descr*)
  ;; TODO assert?
  (lnm name (apply elem descr*)))

(define (lnm->benchmark l)
  (define s (lnm-name l))
  (or
   (and
    (list? (unbox benchmark-data*))
    (for/first ([b (in-list (unbox benchmark-data*))]
               #:when (let ([s* (benchmark->name* b)])
                        (if (list? s*) (memq s s*) (eq? s s*))))
      b))
   (raise-user-error 'lnm->benchmark "Failed to get benchmark for LNM '~a'." s)))

(define (lnm<? l1 l2)
  (benchmark<? (lnm->benchmark l1)
               (lnm->benchmark l2)))

;; (-> Lnm * Any)
(define (lnm-descriptions . l*)
  (check-missing-benchmarks (map lnm-name l*) #:exact? #t)
  (map render-lnm-description (sort l* lnm<?)))

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

;; =============================================================================

(module+ test
  (require rackunit rackunit-abbrevs)

  (check-apply* add-commas
   [1
    => "1"]
   [10
    => "10"]
   [100
    => "100"]
   [1000
    => "1,000"]
   [999999
    => "999,999"]
   [12
    => "12"]
   [123456789
    => "123,456,789"]
   [12456789
    => "12,456,789"])
)

