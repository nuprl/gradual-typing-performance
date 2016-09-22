#lang racket/base

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

  render-exact-plot
  render-typed/untyped-plot
  render-deliverable-plot
  render-uncertainty
  render-karst
  render-srs-sound
  render-srs-precise

  render-exact-table

  ;(rename-out [make-lnm lnm])
  ;; (->* [Symbol] [] #:rest (Listof String) Lnm)

  render-lnm-plot
  ;; (-> (-> (Listof Pict) Elem) Any)

  percent-diff

  deliverable*

  (rename-out
    [ext:typed/untyped-ratio typed/untyped-ratio]
    [ext:max-overhead max-overhead]
    [ext:configuration->overhead configuration->overhead]
    [ext:min-overhead min-overhead])
)

(require
 benchmark-util/data-lattice
 glob
 gtp-summarize
 (only-in gtp-summarize/bitstring natural->bitstring log2 bit-high?)
 racket/match
 (only-in pict blank hc-append vc-append)
 (only-in "common.rkt" etal cite exact parag)
 (only-in racket/file file->value)
 (only-in racket/format ~r)
 (only-in racket/list last append*)
 (only-in racket/port with-input-from-string)
 (only-in racket/string string-prefix? string-join)
 scribble/core
 scribble/base
 version/utils
 with-cache
 ;;
 racket/contract
 "benchmark.rkt"
 "util.rkt"
)
(require (only-in racket/serialize
  serialize
  deserialize
))

;; =============================================================================

(define (count-benchmarks)
  (*NUM-BENCHMARKS*))

(define (count-new-oo-benchmarks)
  (*NUM-OO-BENCHMARKS*))

(define COMPILED "./compiled") ;; Where Racket stores compiled files
(define MODULE-GRAPH "./module-graphs") ;; Where to store module graphs

;; -----------------------------------------------------------------------------
;; --- Syntax & Utils
;;     (May be better off in libraries)

(define (count-all-configurations)
  (*TOTAL-NUM-CONFIGURATIONS*))

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

(define BENCHMARK-NAMES (map benchmark-name ALL-BENCHMARKS))

(define bits
  tt)

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

(define ((list-cache-file tag) bm-or-bm*)
  (ensure-dir COMPILED)
  (define bm* (if (list? bm-or-bm*) bm-or-bm* (list bm-or-bm*)))
  (string-append
    COMPILED "/" tag
    (string-join (map (compose1 symbol->string benchmark-name) bm*) "-")
    ".rktd"))

(define exact-cache-file
  (list-cache-file "cache-exact-"))

(define typed/untyped-cache-file
  (list-cache-file "cache-tu-"))

(define (deliverable-cache-file N)
  (list-cache-file (format "cache-~a-deliverable-" N)))

(define (lattice-cache-file bm v)
  (ensure-dir COMPILED)
  (string-append COMPILED "/cache-lattice-" (symbol->string bm) "-" v ".rktd"))

(define (benchmarks-table-cache-file)
  (ensure-dir COMPILED)
  (build-path COMPILED (*BENCHMARK-TABLE-CACHE*)))

(define (benchmark-savings-cache)
  (ensure-dir COMPILED)
  (build-path COMPILED (*BENCHMARK-SAVINGS-CACHE*)))

(define (exact-table-cache)
  (ensure-dir COMPILED)
  (build-path COMPILED (*EXACT-TABLE-CACHE*)))

(define (lnm-table-cache)
  (ensure-dir COMPILED)
  (build-path COMPILED (*LNM-TABLE-CACHE*)))

(define (lnm-table-data-cache)
  (ensure-dir COMPILED)
  (build-path COMPILED (*LNM-TABLE-DATA-CACHE*)))

;; -----------------------------------------------------------------------------
;; --- Lattice

(define (render-data-lattice bm v)
  (parameterize ([*current-cache-keys* (list *LATTICE-BORDER-WIDTH* *LATTICE-BOX-BOT-MARGIN* *LATTICE-BOX-HEIGHT* *LATTICE-BOX-SEP* *LATTICE-BOX-TOP-MARGIN* *LATTICE-BOX-WIDTH* *LATTICE-CONFIG-MARGIN* *LATTICE-LEVEL-MARGIN* *LATTICE-FONT-SIZE* *LATTICE-TRUNCATE-DECIMALS?*)])
    (with-cache (lattice-cache-file (benchmark-name bm) v)
      #:read deserialize
      #:write serialize
      (lambda () (file->performance-lattice (benchmark-rktd bm v))))))

;; -----------------------------------------------------------------------------
;; --- Benchmarks

;; (-> benchmark String)
(define (render-benchmark b+d)
  (define b* (car b+d))
  (define-values (b name)
    (if (list? b*)
      (values (car b*) (string-join (map (compose1 symbol->string benchmark-name) b*) ", "))
      (values b* (symbol->string (benchmark-name b*)))))
  (define d (cdr b+d))
  (elem
    "\\benchmark{"
    name
    "}{"
    (benchmark-author b)
    "}{"
    (benchmark-origin b)
    "}{"
    (benchmark-purpose b)
    "}{"
    d
    "}{"
    (benchmark->tex-file b)
    "}{"
    (or (benchmark-lib* b) "N/A")
    "}\n\n"))

(define (benchmark->tex-file b)
  (define M (benchmark-modulegraph b))
  (define pn (modulegraph-project-name M))
  (define mgd MODULE-GRAPH)
  (ensure-dir mgd)
  (define mgf (format "~a/~a.tex"  mgd pn))
  (unless (file-exists? mgf)
    (WARNING "could not find modulegraph for project '~a', creating graph now." pn)
    (call-with-output-file mgf
      (lambda (p) (modulegraph->tex M p))))
  (elem "\\modulegraph{" mgf "}"))

;; (-> Benchmark * Any)
(define (render-benchmark-descriptions . b+d*)
  (define key (equal-hash-code (map cdr b+d*)))
  (parameterize ([*current-cache-keys* (list (lambda () key))]
                 [*use-cache?* #f]) ;; Sorry, can't serialize scribble elements
    (with-cache (cachefile "benchmark-descriptions.rktd")
      #:read deserialize
      #:write serialize
      (lambda ()
        ;(check-missing-benchmarks (map (compose1 benchmark-name car) b+d*))
        (define b+d*+ (sort b+d* benchmark<? #:key (lambda (x) (if (list? (car x)) (caar x) (car x)))))
        (apply exact (map render-benchmark b+d*+))))))

(define BENCHMARKS-TABLE-TITLE* '(
  "Benchmark"
  "\\twoline{Untyped}{LOC}"
  "\\twoline{Annotation}{LOC}"
  "\\# Mod."
  "\\# Adp."
  "\\# Bnd."
  "\\# Exp."
))

(define EXACT-TABLE-TITLE* '(
  "TBA"
))

(define (render-benchmarks-table)
  (render-table new-benchmarks-table
   #:title BENCHMARKS-TABLE-TITLE*
   #:cache (benchmarks-table-cache-file)))

(define (new-benchmarks-table)
  (for/list ([b (in-list ALL-BENCHMARKS)])
    (define M (benchmark-modulegraph b))
    (define num-adaptor (benchmark-num-adaptor b))
    (define uloc (modulegraph->untyped-loc M))
    (define tloc (modulegraph->typed-loc M))
    (tex-row
     (format "{\\tt ~a}" (benchmark-name b))
     (number->string uloc)
     (if (zero? uloc) "0" (format-percent-diff tloc uloc))
     (number->string (modulegraph->num-modules M))
     (number->string num-adaptor)
     (number->string (modulegraph->num-edges M))
     (number->string (modulegraph->num-identifiers M)))))

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

(define (render-lnm-plot pict*->elem #:rktd*** [rktd*** #f])
  ;; Sort & make figures of with 6 plots each or whatever
  (parameterize ([*AXIS-LABELS?* #f]
                 [*L* '(0 1)]
                 [*L-LABELS?* #t]
                 [*LEGEND?* (or (not rktd***)
                                (for*/or ([rktd** (in-list rktd***)]
                                          [rktd*  (in-list rktd**)])
                                  (< 1 (length rktd*))))]
                 [*LINE-LABELS?* #f]
                 [*LOG-TRANSFORM?* #t]
                 [*M* #f]
                 [*MAX-OVERHEAD* 20]
                 [*N* #f]
                 [*NUM-SAMPLES* 60] ;; 200 ;; TODO
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
      (for/list ([rktd** (in-list (or rktd*** (split-list 5 (get-lnm-rktd**))))]
                 [i (in-naturals 1)])
        (parameterize ([*CACHE-TAG* (if cache? (number->string i) #f)])
          (collect-garbage 'major)
          (render-lnm (list->vector (append* rktd**))))))))

;; Get data for each benchmark for each version of racket
(define (get-lnm-rktd**)
  (for/list ([b (in-list ALL-BENCHMARKS)])
    (for/list ([v->rktd (in-list (benchmark-rktd* b))])
      ;; Drops the version tags
      (cdr v->rktd))))

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

;; -----------------------------------------------------------------------------

(define (benchmark->rktd* bm)
  (for/list ([v (in-list (*RKT-VERSIONS*))])
    (benchmark-rktd bm v)))

(define (render-exact-plot . bm*)
  (with-cache (exact-cache-file bm*)
    #:read deserialize
    #:write serialize
    (lambda ()
      (parameterize ([*LEGEND?* #f]
                     [*PLOT-FONT-SCALE* 0.04]
                     [*PLOT-HEIGHT* 140]
                     [*PLOT-WIDTH* 430]
                     [*POINT-SIZE* 6]
                     [*POINT-ALPHA* 0.7])
        (render-exact*
          (for*/list ([bm (in-list bm*)])
            (list->vector (benchmark->rktd* bm))))))))

(define (render-exact-table bm)
  (render-table
    new-exact-table
    #:title EXACT-TABLE-TITLE*
    #:cache (cachefile "exact-table")))

(define (new-exact-table)
  (for/list ([b (in-list ALL-BENCHMARKS)])
    (tex-row "?")))

(define (render-typed/untyped-plot . bm*)
  (with-cache (typed/untyped-cache-file bm*)
    #:read deserialize
    #:write serialize
    (lambda ()
      (parameterize ([*LEGEND?* #f]
                     [*ERROR-BAR-WIDTH* 20]
                     [*ERROR-BAR-LINE-WIDTH* 1]
                     [*PLOT-FONT-SCALE* 0.04]
                     [*PLOT-HEIGHT* 180]
                     [*PLOT-WIDTH* 430]
                     [*POINT-SIZE* 5]
                     [*Y-NUM-TICKS* 4]
                     [*POINT-ALPHA* 0.7])
        (render-typed/untyped
          (for*/list ([bm (in-list bm*)])
            (list->vector (benchmark->rktd* bm))))))))

(define (render-deliverable-plot D . bm*)
  (with-cache ((deliverable-cache-file D) bm*)
    #:read deserialize
    #:write serialize
    (lambda ()
      (parameterize ([*LEGEND?* #f]
                     [*ERROR-BAR-WIDTH* (*RECTANGLE-WIDTH*)]
                     [*ERROR-BAR-LINE-WIDTH* (*RECTANGLE-WIDTH*)]
                     [*Y-NUM-TICKS* 3]
                     [*POINT-ALPHA* 0.7]
                     [*PLOT-FONT-SCALE* 0.04]
                     [*PLOT-HEIGHT* 180]
                     [*PLOT-WIDTH* 440])
        (render-deliverable D
          (for*/list ([bm (in-list bm*)])
            (list->vector (benchmark->rktd* bm))))))))

(define (render-uncertainty D bm*)
  (vc-append 20
   (hc-append 0 (blank 10 0) (apply render-typed/untyped-plot bm*))
   (apply render-deliverable-plot D bm*)
   (render-bars-xlabels 33 (map (compose1 symbol->string benchmark-name) bm*))))

(define (render-karst csv)
  (parameterize ([*current-cache-keys* (list (lambda () csv))])
                 [*LEGEND?* #f]
                 [*ERROR-BAR-WIDTH* 0.2]
                 [*ERROR-BAR-LINE-WIDTH* 1]
                 [*PLOT-FONT-SCALE* 0.04]
                 [*PLOT-HEIGHT* 140]
                 [*PLOT-WIDTH* 430]
                 [*POINT-SIZE* 5]
                 [*COLOR-OFFSET* 4]
                 [*Y-NUM-TICKS* 4]
                 [*POINT-ALPHA* 0.7])
    (with-cache (cachefile "cache-karst.rktd")
      #:read deserialize
      #:write serialize
        (lambda ()
          (render-karst-pict csv))))

(define (render-srs-sound bm* factor*)
  (define rktd**
    (for/list ([bm (in-list bm*)])
      (for/list ([v (in-list (*RKT-VERSIONS*))])
        (benchmark-rktd bm v))))
  (parameterize ([*current-cache-keys* (list (lambda () factor*) (lambda () rktd**) (lambda () version))]
                 [*AXIS-LABELS?* #f]
                 [*L* '(0)]
                 [*L-LABELS?* #t]
                 [*LEGEND?* #f]
                 [*LINE-LABELS?* #f]
                 [*LOG-TRANSFORM?* #t]
                 [*M* #f]
                 [*MAX-OVERHEAD* 20]
                 [*N* #f]
                 [*NUM-SAMPLES* 60] ;; 200 ;; TODO
                 [*PLOT-FONT-SCALE* 0.04]
                 [*SINGLE-PLOT?* #f]
                 [*X-MINOR-TICKS* (append (for/list ([i (in-range 12 20 2)]) (/ i 10))
                                          (for/list ([i (in-range 4 20 2)]) i))]
                 [*X-TICK-LINES?* #t]
                 [*X-TICKS* '(1 2 20)]
                 ;[*Y-MINOR-TICKS* '(25 75)]
                 [*Y-NUM-TICKS* 3]
                 [*Y-TICK-LINES?* #t]
                 [*Y-STYLE* '%])
    (with-cache (cachefile "cache-srs-sound.rktd")
      #:read deserialize
      #:write serialize
      (lambda ()
        (render-srs-sound-pict rktd** factor*)))))

(define (render-srs-precise bm* factor*)
  (define rktd**
    (for/list ([bm (in-list bm*)])
      (for/list ([v (in-list (*RKT-VERSIONS*))])
        (benchmark-rktd bm v))))
  (parameterize ([*current-cache-keys* (list (lambda () factor*) (lambda () rktd**) (lambda () version))]
                 [*AXIS-LABELS?* #f]
                 [*LEGEND?* #f]
                 [*LINE-LABELS?* #f]
                 [*LOG-TRANSFORM?* #t]
                 [*M* #f]
                 [*MAX-OVERHEAD* 20]
                 [*N* #f]
                 [*NUM-SAMPLES* 20] ;; 200 ;; TODO
                 [*PLOT-FONT-SCALE* 0.04]
                 [*SINGLE-PLOT?* #f]
                 [*X-MINOR-TICKS* (append (for/list ([i (in-range 12 20 2)]) (/ i 10))
                                          (for/list ([i (in-range 4 20 2)]) i))]
                 [*X-TICK-LINES?* #t]
                 [*X-TICKS* '(1 2 20)]
                 ;[*Y-MINOR-TICKS* '(25 75)]
                 [*Y-NUM-TICKS* 3]
                 [*Y-TICK-LINES?* #t]
                 )
    (with-cache (cachefile "cache-srs-precise.rktd")
      #:read deserialize
      #:write serialize
      (lambda ()
        (render-srs-precise-pict rktd** factor*)))))

(define (ext:max-overhead rktd)
  (max-overhead (from-rktd rktd)))

(define (ext:typed/untyped-ratio rktd)
  (typed/untyped-ratio rktd))

(define (ext:configuration->overhead rktd cfg)
  (add-x (rnd (configuration->overhead rktd cfg))))

(define (add-x str)
  (string-append str "x"))

(define (ext:min-overhead rktd)
  (add-x (rnd (min-overhead (from-rktd rktd)))))

(define (deliverable* D v bm*)
  (parameterize ([*current-cache-keys* (list (lambda () bm*))])
    (with-cache (cachefile (format "cache-~a-deliverable-count" D))
      (lambda ()
        (for/sum ((bm (in-list bm*)))
          (define rktd (benchmark-rktd bm v))
          ((D-deliverable D) (from-rktd rktd)))))))


;; -----------------------------------------------------------------------------
;
;;; For each benchmark, assign each function a weight of importance
;;;  (2016-06-07: unitless for now, but will eventually make this official)
;(define (weigh-functions)
;  ;; for benchmarks, get-lnm-data* ...
;  (with-output-to-file "yolo.txt" #:exists 'replace (lambda ()
;  (for ([rktd* (in-list (get-lnm-rktd**))])
;    (define rktd (last rktd*))
;    (define title (fname->title rktd))
;    (define mg (symbol->modulegraph (string->symbol title)))
;    (define f* (module-names mg))
;    (printf "title: ~a, dataset: ~a\n" title rktd)
;    (for ([w (in-list (get-weights f* rktd))])
;      (printf "- ~a\n" w))))))
;
;(define (mean2 xs)
;  (rnd (call-with-values
;         (lambda () (for/fold ((sum 0) (len 0)) ((x (in-list xs))) (values (+ x sum) (+ 1 len))))
;         /)))
;
;(define (get-weights f* rktd)
;  (define v (file->value rktd))
;  (define num-modules (length f*))
;  (define (nat->bits n)
;    (natural->bitstring n #:pad num-modules))
;  (for/list ((f (in-list f*))
;             (fun-idx (in-naturals)))
;    ;; fun-idx corresponds to a function we want to compare typed/untyped
;    (define-values (u* t*)
;      (for/fold ([u* '()]
;                 [t* '()])
;                ([row (in-vector v)]
;                 [row-idx (in-naturals)])
;        (define cfg (nat->bits row-idx))
;        (if (bit-high? cfg fun-idx)
;          (values u* (append row t*))
;          (values (append row u*) t*))))
;    (when (or (null? u*) (null? t*))
;      (raise-user-error 'weigh "not good for ~a at ~a ~a ~a" rktd f num-modules))
;    (define um (mean2 u*))
;    (define tm (mean2 t*))
;    (list f um tm (abs (- (string->number um) (string->number tm))))))
;
;; =============================================================================

(module+ test
  (require rackunit rackunit-abbrevs)

  (check-apply* read-list
   ["()" => '()]
   ["( 1 2 3   )" => '(1 2 3)]
   ["( 2 1 () 5)" => '(2 1 () 5)]
   ["yolo" => #f]
   [")( " => #f])

  (test-case "typed/untyped-ratio"
    (let ([z6.2 (ext:typed/untyped-ratio (benchmark-rktd zordoz "6.2"))]
          [z6.3 (ext:typed/untyped-ratio (benchmark-rktd zordoz "6.3"))])
      (check-true (< z6.3 z6.2)))
    (let ([fsm6.2 (ext:typed/untyped-ratio (benchmark-rktd fsm "6.2"))]
          [fsm6.3 (ext:typed/untyped-ratio (benchmark-rktd fsm "6.3"))]
          [fsmoo6.2 (ext:typed/untyped-ratio (benchmark-rktd fsmoo "6.2"))]
          [fsmoo6.3 (ext:typed/untyped-ratio (benchmark-rktd fsmoo "6.3"))])
      (check-true (> fsm6.3 fsm6.2))
      (check-true (= fsmoo6.3 fsmoo6.2))))
)

