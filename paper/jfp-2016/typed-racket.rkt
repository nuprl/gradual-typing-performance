#lang racket/base

;; TODO
;; - dungeon data

;; Supporting code for `typed-racket.scrbl`
;; - Render & organize benchmarks
;; - Make L-N/M figures

(provide ;; TEMPORARY
  new-untyped-bars
)
(provide
  count-benchmarks
  count-new-oo-benchmarks

  bits
  ;; (-> String Any)
  ;; Use to format bitstrings

  bm
  ;; (-> String Any)
  ;; Use to format benchmark names.
  ;; Asserts that its argument is a correctly-spelled benchmark name.

  count-savings
  ;; (-> (Listof String) (Values Natural Natural))
  ;; Count Anderson-Darling savings.
  ;; i.e. the number of data rows with 10 values rather than 30

  count-all-configurations
  ;; (-> Natural)

  (rename-out [make-benchmark benchmark])
  ;; (->* [] [#:name String
  ;;          #:author String
  ;;          #:num-adaptor Natural
  ;;          #:origin String
  ;;          #:purpose String
  ;;          #:external-libraries (Listof String)]
  ;;     #:rest (Listof String)
  ;;     Benchmark)

  get-lnm-table-data

  render-benchmark-descriptions
  ;; (-> Benchmark * Any)
  ;; Render a list of Benchmark structures.
  ;; Use the `benchmark` constructor to make a `Benchmark`

  render-benchmarks-table
  ;; (-> Any)
  ;; Build a table describing static properties of the benchmarks

  render-data-lattice
  ;; (-> Benchmark-Name Version-String Any)
  ;;   where Benchmark-Name = (U String Symbol)
  ;;     and Version-String = String
  ;; Finds the data file corresponding to the benchmark
  ;;  at the specific version and renders a data lattice.

  render-lnm-table
  ;; (-> Any)

  (rename-out [make-lnm lnm])
  ;; (->* [Symbol] [] #:rest (Listof String) Lnm)

  render-lnm-plot
  ;; (-> (-> (Listof Pict) Elem) Any)

  percent-diff
  (rename-out
    [ext:typed/untyped-ratio typed/untyped-ratio]
    [ext:configuration->overhead configuration->overhead])
)

(require
 benchmark-util/data-lattice
 glob
 gtp-summarize
 racket/match
 (only-in "common.rkt" etal cite exact parag rnd)
 (only-in racket/file file->value)
 (only-in racket/format ~r)
 (only-in racket/list last append*)
 (only-in racket/port with-input-from-string)
 (only-in racket/string string-prefix? string-join)
 scribble/core
 scribble/base
 version/utils
 ;;
 racket/contract
)
(require (only-in racket/serialize
  serialize
  deserialize
))

;; -----------------------------------------------------------------------------
;; -- Parameters
(define-syntax-rule (defparam2 a b c)
  (begin
    (define a (make-parameter c))
    (provide a)))

(defparam2 *CACHE?* Boolean #t)

(defparam2 *BENCHMARK-TABLE-CACHE* Path-String "cache-benchmark-table.rktd")
(defparam2 *BENCHMARK-SAVINGS-CACHE* Path-String "cache-benchmark-savings.rktd")
(defparam2 *DRAFT?* Boolean #t)
(defparam2 *LNM-TABLE-CACHE* Path-String "cache-lnm-table.rktd") ;; Place to store cached lnm table
(defparam2 *LNM-TABLE-DATA-CACHE* Path-String "cache-lnm-table-data.rktd")
(defparam2 *LNM-OVERHEAD* (Listof Exact-Rational) '(1/5 3 10))
(defparam2 *RKT-VERSIONS* (Listof String) '("6.2" "6.3" "6.4"))

(define BENCHMARK-NAMES ;; (Listof (U Symbol (Listof Symbol)))
  '(acquire
    dungeon
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
    take5
    tetris
    zombie
    zordoz ;(zordoz zordoz.6.2 zordoz.6.3)
))

(defparam2 *BENCHMARK-DATA* (Listof Benchmark) '())
;; Initially empty, until we give descriptions in the paper

;; -----------------------------------------------------------------------------

(define single-name? symbol?)

(define multi-name? list?)
(define multi-canonical car)
(define multi-rest cdr)

(define (name->canonical n)
  (if (multi-name? n) (multi-canonical n) n))

(define (in-name? sym name)
  (if (single-name? name)
    (eq? sym name)
    (memq sym name)))

(define (name<? n1 n2)
  (define s1 (name->canonical n1))
  (define s2 (name->canonical n2))
  (symbol<? s1 s2))

(define (count-benchmarks)
  (for/sum ([n (in-list BENCHMARK-NAMES)])
    (if (single-name? n)
      1
      (length (multi-rest n)))))

(define (count-new-oo-benchmarks)
  (length '(acquire dungeon forth fsmoo take5)))

(define COMPILED "./compiled") ;; Where Racket stores compiled files
(define MODULE-GRAPH "./module-graphs") ;; Where to store module graphs

;; -----------------------------------------------------------------------------
;; --- Assertions & Predicates

;; (-> Symbol Boolean)
(define (valid-benchmark? sym)
  (for/or ([n (in-list BENCHMARK-NAMES)])
    (in-name? sym n)))

;; (-> Symbol Void)
(define (assert-benchmark name-sym)
  (unless (valid-benchmark? name-sym)
    (unknown-benchmark-error name-sym)))

;; Like, zordoz.6.2 and zordoz.6.3 instead of zordoz
;; (-> (Listof Symbol) Void)
(define (check-missing-benchmarks name* #:exact? [exact? #f])
  (let loop ([expect* (sort BENCHMARK-NAMES name<?)]
             [given*  (sort name* symbol<?)])
    (cond
     [(null? expect*)
      (unknown-benchmark-error given*)]
     [(null? given*)
      (missing-benchmark-error (cdr expect*))]
     [else
      (define expect (car expect*))
      (define given (car given*))
      (cond
       [(name<? expect given)
        (loop (cdr expect*) given*)]
       [(in-name? given expect)
        (if (and (null? (cdr given*))
                 (null? (cdr expect*)))
            (void)
            (loop expect* (cdr given*)))]
       [(name<? expect given)
        (missing-benchmark-error expect)]
       [else
        (unknown-benchmark-error given)])])))

;; (->* (valid-benchmark? valid-version?) (#:tag string?) path-string?)
(define (data-path bm v [tag "*"])
  (define str (symbol->string bm))
  (define bm-str (if (eq? bm 'zordoz) (string-append str "." v) str))
  (glob-first (string-append (get-git-root) "/data/" v "/" bm-str "-" tag ".rktd")))

;; -----------------------------------------------------------------------------
;; --- Syntax & Utils
;;     (May be better off in libraries)

(define-syntax-rule (INFO msg arg* ...)
  (begin
    (display "INFO: ")
    (printf msg arg* ...)
    (newline)))

(define-syntax-rule (WARNING msg arg* ...)
  (begin
    (display "WARNING: ")
    (printf msg arg* ...)
    (newline)))

(define (glob-first str)
  (match (glob str)
   [(cons r '())
    r]
   ['()
    (raise-user-error 'glob-first "No results for glob '~a'" str)]
   [r*
    (WARNING "ambiguous results for glob '~a'. Returning the first." str)
    (car r*)]))

(define (count-all-configurations)
  (for/sum ([b (in-list (*BENCHMARK-DATA*))])
    (expt 2 (benchmark->num-modules b))))

(define (count-savings)
  (with-cache (benchmark-savings-cache)
    #:read uncache-dataset
    #:write cache-dataset
    new-count-savings))

(define (new-count-savings)
  (define-values (skip total)
    (for*/fold ([num-skip-runs 0]
                [num-runs 0])
               ([rktd* (in-list (get-lnm-rktd**))]
                [d (in-list rktd*)])
      (with-input-from-file d
        (lambda ()
          (for/fold ([nsr num-skip-runs]
                     [nr num-runs])
                    ([ln (in-lines)])
            (define is-run? (eq? #\( (string-ref ln 0)))
            (values
              (+ nsr (if (and is-run? (= 10 (length (read-list ln)))) 1 0))
              (+ nr (if is-run? 1 0))))))))
    (list skip total))

(define (read-list str)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (let ([r (with-input-from-string str read)])
      (and (list? r) r))))

;; Add '&' for TeX
(define (tex-row . x*)
  (let loop ([x* x*])
    (if (null? (cdr x*))
      (list (car x*) " \\\\\n")
      (list* (car x*) " & " (loop (cdr x*))))))

(define (percent-diff meas exp)
  (/ (- meas exp) exp))

;; -----------------------------------------------------------------------------
;; --- Formatting

(define bits
  tt)

(define (bm name)
  (assert-benchmark (string->symbol name))
  (tt name))

;; -----------------------------------------------------------------------------
;; --- Paths / Caching

(define ((cache-read-error cache-file) exn)
  (WARNING "Failed to read cachefile '~a', got exception:\n~a" cache-file (exn-message exn))
  #f)

(define (with-cache cache-file thunk #:read [read-proc #f] #:write [write-proc #f])
  (let ([read-proc (or read-proc values)]
        [write-proc (or write-proc values)])
    (or (and (*CACHE?*)
             (file-exists? cache-file)
             (let ([v (with-handlers ([exn:fail? (cache-read-error cache-file)])
                        (read-proc (file->value cache-file)))])
               (and v
                    (INFO "reading cachefile '~a'" cache-file)
                    v)))
        (let ([r (thunk)])
          (INFO "writing cachefile '~a'" cache-file)
          (with-output-to-file cache-file #:exists 'replace
            (lambda () (writeln (write-proc r))))
          r))))

;; Example #:write proc
(define (cache-table T)
  (cons BENCHMARK-NAMES T))

;; Example #:read proc
(define (uncache-table tag+data)
  (and (equal? (car tag+data) BENCHMARK-NAMES)
       (cdr tag+data)))

(define (cache-dataset d)
  (cons (get-lnm-rktd**) d))

(define (uncache-dataset fname+d)
  (and (equal? (car fname+d) (get-lnm-rktd**))
       (cdr fname+d)))

(define (render-table render-proc #:title title* #:cache cache-file)
  (exact
    "\\setlength{\\tabcolsep}{0.2em}"
    (format "\\begin{tabular}{l~a}" (make-string (sub1 (length title*)) #\r))
    (list " \\toprule \n" (apply tex-row title*) " \\midrule \n")
    (apply elem
      (with-cache cache-file
       #:read uncache-table
       #:write cache-table
       render-proc))
    "\\end{tabular}\n\n"))

(define (lattice-cache-file bm v tag)
  (ensure-dir COMPILED)
  (string-append COMPILED "/cache-lattice-" (symbol->string bm) "-" v "-" tag ".rktd"))

(define (benchmarks-table-cache-file)
  (ensure-dir COMPILED)
  (build-path COMPILED (*BENCHMARK-TABLE-CACHE*)))

(define (benchmark-savings-cache)
  (ensure-dir COMPILED)
  (build-path COMPILED (*BENCHMARK-SAVINGS-CACHE*)))

(define (lnm-table-cache)
  (ensure-dir COMPILED)
  (build-path COMPILED (*LNM-TABLE-CACHE*)))

(define (lnm-table-data-cache)
  (ensure-dir COMPILED)
  (build-path COMPILED (*LNM-TABLE-DATA-CACHE*)))

(define (version->data-file* v)
  (in-glob (string-append (get-git-root) "/data/" v "/*.rktd")))

;; -----------------------------------------------------------------------------
;; --- Lattice

(define (render-data-lattice bm v #:tag [tag "*"])
  (with-cache (lattice-cache-file bm v tag)
    #:read deserialize
    #:write serialize
    (lambda () (file->performance-lattice (data-path bm v tag)))))

;; -----------------------------------------------------------------------------
;; --- Benchmarks

(struct benchmark (name alt* author num-adaptor origin purpose lib* description modulegraph))
(define (make-benchmark #:name name
                        #:author author
                        #:num-adaptor num-adaptor
                        #:origin origin
                        #:purpose purpose
                        #:alt [other-names #f]
                        #:external-libraries [lib* #f]
                        description)
  (assert-benchmark name)
  (define M (symbol->modulegraph name))
  (define alt* (or other-names (list name)))
  (benchmark name alt* author num-adaptor origin purpose lib* description M))

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

;; (-> benchmark String)
(define (render-benchmark b)
  (elem
    "\\benchmark{"
    (symbol->string (benchmark-name b))
    "}{"
    (benchmark-author b)
    "}{"
    (benchmark-origin b)
    "}{"
    (benchmark-purpose b)
    "}{"
    (benchmark-description b)
    "}{"
    (benchmark->tex-file b)
    "}{"
    (or (benchmark-lib* b) "N/A")
    "}\n\n"))

(define (benchmark->num-modules b)
  (modulegraph->num-modules (benchmark-modulegraph b)))

(define (benchmark<? b1 b2)
  (define m1 (benchmark->num-modules b1))
  (define m2 (benchmark->num-modules b2))
  (or (< m1 m2)
      (and (= m1 m2)
           (symbol<? (benchmark-name b1) (benchmark-name b2)))))

(define (benchmark->name* b)
  (project-name->name* (benchmark-name b)))

(define (project-name->name* s)
  (or (for/first ([n (in-list BENCHMARK-NAMES)]
                  #:when (in-name? s n))
        (if (multi-name? n) (multi-rest n) (list n)))
      (unknown-benchmark-error s)))

(define (symbol->modulegraph n)
  (if (eq? n 'zordoz)
    (project-name->modulegraph 'zordoz.6.3)
    (let ([name* (project-name->name* n)])
      (cond
       [(single-name? name*)
        ;; Easy! Compile & return the modulegraph.
        (project-name->modulegraph name*)]
       [else
        ;; Hard!
        ;; - Compile all names in `cdr` to modulegraphs. If they fail, ignore them.
        ;; - Filter duplicate modulegraphs from the list.
        ;; - Return only a single modulegraph
        (choose-modulegraph #:src name*
          (for/fold ([acc '()])
                    ([name (in-list name*)])
            ;; Try compiling a modulegraph, don't worry if it fails.
            (define M (with-handlers ([exn:fail? (lambda (e) (WARNING (exn-message e)) #f)])
                        (project-name->modulegraph name)))
            (if (and M (not (for/or ([M2 (in-list acc)])
                              (adjlist=? (modulegraph-adjlist M) (modulegraph-adjlist M2)))))
              (cons M acc)
              acc)))]))))

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

(define (benchmark->tex-file b)
  (define M (benchmark-modulegraph b))
  (ensure-dir MODULE-GRAPH)
  (define mgd MODULE-GRAPH)
  (ensure-dir mgd)
  (define pn (modulegraph-project-name M))
  (define mgf (string-append mgd "/" pn ".tex"))
  (unless (file-exists? mgf)
    (WARNING "could not find modulegraph for project '~a', creating graph now." pn)
    (call-with-output-file mgf
      (lambda (p) (modulegraph->tex M p))))
  (elem "\\modulegraph{" mgf "}"))

;; (-> Benchmark * Any)
(define (render-benchmark-descriptions . b*)
  (check-missing-benchmarks (map benchmark-name b*))
  (define b+* (sort b* benchmark<?))
  (*BENCHMARK-DATA* b+*)
  ;(INFO "about to render ~a" (map benchmark-name b+*))
  (apply exact (map render-benchmark b+*)))

(define BENCHMARKS-TABLE-TITLE* '(
  "Benchmark"
  "\\twoline{Untyped}{LOC}"
  "\\twoline{Annotation}{LOC}"
  "\\twoline{\\# Modules}{(Adaptors)}"
  "\\# Boundaries"
  "\\# Exports"
))

(define (render-benchmarks-table)
  (render-table new-benchmarks-table
   #:title BENCHMARKS-TABLE-TITLE*
   #:cache (benchmarks-table-cache-file)))

(define (new-benchmarks-table)
  (for/list ([b (in-list (*BENCHMARK-DATA*))])
    (define M (benchmark-modulegraph b))
    (define num-adaptor (benchmark-num-adaptor b))
    (define uloc (modulegraph->untyped-loc M))
    (define tloc (modulegraph->typed-loc M))
    (tex-row
     (format "{~a}" (benchmark-name b))
     (format "~a" uloc)
     (format-percent-diff tloc uloc)
     (format-num-modules M #:adaptor num-adaptor)
     (number->string (modulegraph->num-edges M))
     (number->string (modulegraph->num-identifiers M)))))

(define (format-num-modules M #:adaptor [adaptor #f])
  (if adaptor
    (format "~a\\,~~(~a)"
      (modulegraph->num-modules M)
      adaptor)
    (number->string (modulegraph->num-modules M))))

(define (format-percent-diff meas exp)
  (define diff (- meas exp))
  (define pct (round (* 100 (percent-diff meas exp))))
  (format "~a~~~~~a(~a\\%)"
    diff
    (if (< pct 10) "\\hphantom{0}" "")
    pct))

;; =============================================================================

(struct lnm (name description))
(define (make-lnm name . descr*)
  (assert-benchmark name)
  (lnm name (apply elem descr*)))

;;;
;(define (lnm<? l1 l2)
;  (benchmark<? (lnm->benchmark l1)
;               (lnm->benchmark l2)))
;
;;; (-> Lnm * Any)
;(define (render-lnm-descriptions . l*)
;  (check-missing-benchmarks (map lnm-name l*) #:exact? #t)
;  (map render-lnm-description (sort l* lnm<?)))

(define (render-lnm-description l)
  (elem
    (parag (symbol->string (lnm-name l)))
    (elem (lnm-description l))))

(define (render-lnm-plot pict*->elem)
  ;; Sort & make figures of with 6 plots each or whatever
  (parameterize ([*AXIS-LABELS?* #f]
                 [*L* '(0 1)]
                 [*L-LABELS?* #t]
                 [*LEGEND?* #t]
                 [*LINE-LABELS?* #f]
                 [*LOG-TRANSFORM?* #t]
                 [*M* #f]
                 [*MAX-OVERHEAD* 20]
                 [*N* #f]
                 [*NUM-SAMPLES* 60] ;; 200
                 [*PLOT-FONT-SCALE* 0.04]
                 [*PLOT-HEIGHT* 100]
                 [*PLOT-WIDTH* 210]
                 [*SINGLE-PLOT?* #f]
                 [*X-MINOR-TICKS* (append (for/list ([i (in-range 12 20 2)]) (/ i 10))
                                          (for/list ([i (in-range 4 20 2)]) i))]
                 [*X-TICK-LINES?* #t]
                 [*X-TICKS* '(1 2 20)]
                 ;[*Y-MINOR-TICKS* '(25 75)]
                 [*Y-NUM-TICKS* 3]
                 [*Y-TICK-LINES?* #t]
                 [*Y-STYLE* '%])
    (define cache? (*CACHE?*))
    (pict*->elem
      (for/list ([rktd** (in-list (split-list 5 (get-lnm-rktd**)))]
                 [i (in-naturals 1)])
        (parameterize ([*CACHE-TAG* (if cache? (number->string i) #f)])
          (collect-garbage 'major)
          (render-lnm (list->vector (append* rktd**))))))))

;; Get data for each benchmark for each version of racket
(define (get-lnm-rktd**)
  (define data-root (string-append (get-git-root) "/data"))
  (define version* (*RKT-VERSIONS*))
  (for*/list ([b (in-list (*BENCHMARK-DATA*))]
              [n (in-list (benchmark->name* b))])
    (define n+ (if (eq? n 'zordoz) "zordoz.6.[23]" n))
    (for/list ([v (in-list version*)])
      (glob-first (format "~a/~a/~a-*.rktd" data-root v n+)))))

(define LNM-TABLE-TITLE* '(
  "Benchmark"
  "\\twoline{Typed/Untyped}{Ratio}"
  "Mean Overhead"
  "Max Overhead"
  ;(for/list ([over (in-list LNM-OVERHEAD*)])
  ;  (format "~a-deliverable" over))
))

(define (render-lnm-table)
  (with-cache (lnm-table-cache)
   #:read (lambda (tag+data)
            (let ([d (uncache-table tag+data)])
              (and d (deserialize d))))
   #:write (compose1 cache-table serialize)
   new-lnm-bars))

(define (get-lnm-table-data)
  (with-cache (lnm-table-data-cache)
   #:read uncache-dataset
   #:write cache-dataset
   (lambda ()
     (call-with-values new-lnm-table-data list))))

(define (new-untyped-data)
  (let loop ([rktd** (get-lnm-rktd**)])
    (if (null? rktd**)
      (values '() '())
      (let-values ([(n* r**) (loop (cdr rktd**))]
                   [(_) (collect-garbage 'major)]
                   [(S*) (for/list ([rktd (in-list (car rktd**))])
                           (from-rktd rktd))])
        (values
          (cons (fname->title (caar rktd**)) n*)
          (cons (map untyped-mean S*) r**))))))

(define (new-lnm-table-data)
  (let loop ([rktd** (get-lnm-rktd**)])
    (if (null? rktd**)
      (values '() '() '() '())
      (let-values ([(n* r** m** x**) (loop (cdr rktd**))]
                   [(_) (collect-garbage 'major)]
                   [(S*) (for/list ;: (Listof Summary)
                                   ([rktd (in-list (car rktd**))])
                           (from-rktd rktd))])
        (values
          (cons (fname->title (caar rktd**)) n*)
          (cons (map typed/untyped-ratio S*) r**)
          (cons (map avg-overhead S*) m**)
          (cons (map max-overhead S*) x**))))))

(define (new-untyped-bars)
  (parameterize ([*PLOT-WIDTH* 420]
                 [*PLOT-HEIGHT* 140]
                 [*PLOT-FONT-SCALE* 0.04]
                 [*LOG-TRANSFORM?* #f])
    (apply render-untyped-bars (call-with-values new-untyped-data list))))

(define (new-lnm-bars)
  (parameterize ([*PLOT-WIDTH* 420]
                 [*PLOT-HEIGHT* 140]
                 [*PLOT-FONT-SCALE* 0.04]
                 [*LOG-TRANSFORM?* #t])
    (apply render-bars (get-lnm-table-data))))

(define (new-lnm-dots)
  (parameterize ([*PLOT-WIDTH* 300]
                 [*PLOT-HEIGHT* 20])
    (render-dots (get-lnm-rktd**))))

(define (old-render-lnm-table)
  (render-table new-lnm-table
   #:title LNM-TABLE-TITLE*
   #:cache (lnm-table-cache)))

(define (new-lnm-table)
  (for/list ([rktd* (in-list (get-lnm-rktd**))])
    (define name (fname->title (car rktd*)))
    (collect-garbage 'major)
    (define S* (map from-rktd rktd*))
    (tex-row
      name
      (format-stat* typed/untyped-ratio S* #:precision 2)
      (format-stat* avg-overhead S*)
      (format-stat* max-overhead S*)
      ;(for/list ([overhead (in-list LNM-OVERHEAD*)])
      ;  (format-stat* (N-deliverable overhead) S*))
    )))

;; TODO put these in sub-table
(define (format-stat* f S* #:precision [p 0])
  (string-join
    (for/list ((S (in-list S*)))
      (~r (f S) #:precision p))
    "~~"))

(define (ext:typed/untyped-ratio sym version)
  (typed/untyped-ratio (data-path sym version)))

(define (ext:configuration->overhead sym version cfg)
  (string-append
    (rnd (configuration->overhead (data-path sym version) cfg))
    "x"))

;; =============================================================================

(module+ test
  (require rackunit rackunit-abbrevs)

  (check-apply* read-list
   ["()" => '()]
   ["( 1 2 3   )" => '(1 2 3)]
   ["( 2 1 () 5)" => '(2 1 () 5)]
   ["yolo" => #f]
   [")( " => #f])

  (test-case "benchmark<?"
    (let* ([name* '(acquire forth fsm gregor kcfa lnm mbta morsecode
                    quad sieve snake suffixtree synth tetris zombie zordoz)]
           [b* (for/list ([n (in-list name*)])
                 (make-benchmark #:name n #:author "N/A" #:num-adaptor 0 #:origin "N/A" #:purpose "N/A" "N/A"))]
           [b+* (sort b* benchmark<?)]
           [expect* '(sieve forth fsm mbta morsecode zombie zordoz lnm suffixtree kcfa snake acquire tetris synth  gregor quad)])
      (check-equal? (map benchmark-name b+*) expect*)))

  (test-case "typed/untyped-ratio"
    (let ([z6.2 (ext:typed/untyped-ratio 'zordoz "6.2")]
          [z6.3 (ext:typed/untyped-ratio 'zordoz "6.3")])
      (printf "YOLO 62 = ~a\n" z6.2)
      (printf "YOLO 63 = ~a\n" z6.3)
      (check-true (< z6.3 z6.2))))

)

