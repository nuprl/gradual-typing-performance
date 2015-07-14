#lang typed/racket/base

;; Data structure representing the results of one experiment.
;; Handles queries for raw data and statistical summary data.

(provide
  ;; Convert a filepath to a summary object
  ;; (->* [Path-String] [#:graph Path-String] Summary)
  from-rktd
  ;; Get the number of variations from a Summary
  ;; (-> Summary Index)
  get-num-variations
  ;; Get the project name from a Summary
  ;; (-> Summary String)
  get-project-name
  ;; Get the mean runtime of the Summary's untyped variation
  ;; (-> Summary Real)
  untyped-mean
  ;; Get the mean runtime of a variation, represented as a bitstring
  ;; (-> Summary String Real)
  variation->mean-runtime
  ;; Return a stream of variations satisfying the predicate
  ;; (-> Summary (-> String Boolean) (Streamof String))
  predicate->variations
  ;; Return a stream of all variations in the Summary
  ;; (-> Summary (Streamof String))
  all-variations
  ;; Return a pict representation of the summary. A kind of TLDR.
  summary->pict
  Summary
)

;; -----------------------------------------------------------------------------

(require
  racket/path
  math/statistics
  (only-in racket/file file->value)
  (only-in racket/vector vector-append)
  (only-in racket/format ~r)
  "modulegraph.rkt"
  "bitstring.rkt"
  "pict-types.rkt"
)

(require/typed racket/stream
  [stream-map (-> (-> Index String) (Sequenceof Index) (Sequenceof String))]
  [stream-filter (-> (-> String Boolean) (Sequenceof String) (Sequenceof String))]
)

;; =============================================================================
;; -- data definition: summary

(struct summary (
  [source : Path-String] ;; the data's origin
  [dataset : (Vectorof (Listof Index))] ;; the underlying experimental data
  [modulegraph : ModuleGraph] ;; the adjacency list of the represented project
  [num-runs : Index]
))
(define-type Summary summary)

;; -----------------------------------------------------------------------------
;; -- constants

;; Default location for TiKZ module graphs
(define MODULE_GRAPH_DIR "module-graphs")

;; -----------------------------------------------------------------------------
;; -- parsing

(define-syntax-rule (parse-error msg arg* ...)
  (error 'summary (format msg arg* ...)))

;; Create a summary from a raw dataset.
;; Infers the location of the module graph if #:graph is not given explicitly
(: from-rktd (->* [String] [#:graph (U Path #f)] Summary))
(define (from-rktd filename #:graph [graph-path #f])
  (define path (string->path filename))
  (define-values (dataset num-runs) (rktd->dataset path))
  (define mg (from-tex (or graph-path (infer-graph path))))
  (validate-modulegraph dataset mg)
  (summary path dataset mg num-runs))

;; Parse a dataset from a filepath.
(: rktd->dataset (-> Path (Values (Vectorof (Listof Index)) Index)))
(define (rktd->dataset path)
  ;; Check .rktd
  (unless (bytes=? #"rktd" (or (filename-extension path) #""))
    (parse-error "Cannot parse dataset '~a', is not .rktd" (path->string path)))
  ;; Get data
  (define vec (file->value path))
  ;; Check invariants
  (validate-dataset vec))

;; Confirm that the dataset `vec` is a well-formed vector of experiment results.
(: validate-dataset (-> Any (Values (Vectorof (Listof Index)) Index)))
(define (validate-dataset vec0)
  (define vec (cast vec0 (Vectorof (Listof Index))))
  (unless (< 0 (vector-length vec)) (parse-error "Dataset is an empty vector, does not contain any entries"))
  ;; Record the number of runs in the first vector, match against other lengths
  (: num-runs (Boxof (U #f Index)))
  (define num-runs (box #f))
  (for ([row-index (in-range (vector-length vec))])
    (define inner (vector-ref vec row-index))
    (define unboxed (unbox num-runs))
    (if (not unboxed)
        (set-box! num-runs (length inner))
        (unless (= unboxed (length inner)) (parse-error "Rows 0 and ~a of dataset have different lengths (~a vs. ~a); all variations must describe the same number of runs.\n  Bad row: ~a" row-index unboxed (length inner) inner)))
    (for ([val (in-list inner)])
      (unless (exact-positive-integer? val)
        (parse-error "Row ~a contains nonsense entry '~a'" row-index val))))
    (values vec (or (unbox num-runs) (error 'neverhappens))))

;; Check that the dataset and module graph agree
(: validate-modulegraph (-> (Vectorof (Listof Index)) ModuleGraph Void))
(define (validate-modulegraph dataset mg)
  (define ds-num-modules (log2 (vector-length dataset)))
  (define mg-num-modules (length (module-names mg)))
  (unless (= ds-num-modules mg-num-modules)
    (parse-error "Dataset and module graph represent different numbers of modules. The dataset says '~a' but the module graph says '~a'" ds-num-modules mg-num-modules)))

;; Guess the location of the module graph matching the dataset
(: infer-graph (-> Path Path-String))
(define (infer-graph path)
  ;; Get the prefix of the path
  (define tag (path->project-name path))
  ;; Search in the MODULE_GRAPH_DIR directory for a matching TeX file
  (define relative-pathstring (format "../~a/~a.tex" MODULE_GRAPH_DIR tag))
  (build-path (or (path-only path) (error 'infer-graph))
              (string->path relative-pathstring)))

;; -----------------------------------------------------------------------------
;; -- querying

(: all-variations (-> Summary (Sequenceof String)))
(define (all-variations sm)
  (define M (get-num-modules sm))
  (stream-map (lambda ([n : Index]) (natural->bitstring n #:pad M))
              (in-range (get-num-variations sm))))

(: get-module-names (-> Summary (Listof String)))
(define (get-module-names sm)
  (module-names (summary-modulegraph sm)))

(: get-num-variations (-> Summary Index))
(define (get-num-variations sm)
  (vector-length (summary-dataset sm)))

(: get-num-runs (-> Summary Index))
(define (get-num-runs sm)
  (summary-num-runs sm))

(: get-num-modules (-> Summary Exact-Positive-Integer))
(define (get-num-modules sm)
  (define len (length (get-module-names sm)))
  (if (< 0 len) len (error 'too-few-modules)))

(: get-project-name (-> Summary String))
(define (get-project-name sm)
  (project-name (summary-modulegraph sm)))

(: predicate->variations (-> Summary (-> String Boolean) (Sequenceof String)))
(define (predicate->variations sm p)
  (stream-filter p (all-variations sm)))

;; Return all data for the untyped variation
(: untyped-runtimes (-> Summary (Listof Index)))
(define (untyped-runtimes sm)
  (vector-ref (summary-dataset sm) 0))

(: untyped-mean (-> Summary Real))
(define (untyped-mean sm)
  (mean (untyped-runtimes sm)))

;; Return all data for the typed variation
(: typed-runtimes (-> Summary (Listof Index)))
(define (typed-runtimes sm)
  (define vec (summary-dataset sm))
  (vector-ref vec (sub1 (vector-length vec))))

(: typed-mean (-> Summary Real))
(define (typed-mean sm)
  (mean (typed-runtimes sm)))

(: variation->mean-runtime (-> Summary String Real))
(define (variation->mean-runtime sm var)
  (index->mean-runtime sm (bitstring->natural var)))

(: index->mean-runtime (-> Summary Index Real))
(define (index->mean-runtime sm i)
  (mean (vector-ref (summary-dataset sm) i)))

;; Fold over lattice points. Excludes fully-typed and fully-untyped.
(: fold-lattice (->* [Summary (-> Real Real Real)] [#:init (U #f Real)] Real))
(define (fold-lattice sm f #:init [init #f])
  (define vec (summary-dataset sm))
  (or
    (for/fold : (U #f Real)
              ([prev : (U #f Real) init])
              ([i    (in-range 1 (sub1 (vector-length vec)))])
      (define val (mean (vector-ref vec i)))
      (or (and prev (f prev val)) val))
    (error 'whoops)))

(: max-lattice-point (-> Summary Real))
(define (max-lattice-point sm)
  (fold-lattice sm max))

(: min-lattice-point (-> Summary Real))
(define (min-lattice-point sm)
  (fold-lattice sm min))

(: avg-lattice-point (-> Summary Real))
(define (avg-lattice-point sm)
  (define 1/N (/ 1 (- (get-num-variations sm) 2)))
  (: f (-> Real Real Real))
  (define (f acc mean) (+ acc (* mean 1/N)))
  (fold-lattice sm f #:init 0))

;; Count the number of variations with performance no worse than N times untyped
(: deliverable (-> Summary Index Natural))
(define (deliverable sm N)
  (define baseline (* N (untyped-mean sm)))
  (: count-N (-> Natural Real Natural))
  (define (count-N acc val)
    (if (<= val baseline)
      (+ 1 acc)
      acc))
  (+ 1 ;;untyped
     (if (<= (typed-mean sm) baseline) 1 0)
     ;; Cast should be unnecessary, but can't do polymorphic keyword args in fold-lattice
     (cast (fold-lattice sm count-N #:init 0) Natural)))

(: usable (-> Summary Index Index Natural))
(define (usable sm N M)
  (define um (untyped-mean sm))
  (define lo (* N um))
  (define hi (* M um))
  (: count-NM (-> Natural Real Natural))
  (define (count-NM acc val)
    (if (and (<  lo val) (<= val hi))
      (+ 1 acc)
      acc))
  (+
    (let ([tm (typed-mean sm)])
      (if (and (< lo tm) (<= tm hi)) 1 0))
    (cast (fold-lattice sm count-NM #:init 0) Natural)))

;; -----------------------------------------------------------------------------
;; --- viewing

(: summary->pict (->* [Summary
                       #:font-face String
                       #:font-size Exact-Positive-Integer
                       #:N Index
                       #:M Index
                       #:height Real
                       #:width Real]
                      [#:title (U String #f)]
                      Pict))
(define (summary->pict sm
                       #:font-face face
                       #:font-size size
                       #:N PARAM-N
                       #:M PARAM-M
                       #:height height
                       #:width width
                       #:title [user-title #f])
  (define vspace (/ size 3))
  (define hspace (/ width 4))
  (define vpad (/ height 5))
  (define baseline (untyped-mean sm))
  (define numvars (get-num-variations sm))
  (: round2 (-> Real String))
  (define (round2 n) (string-append (~r n #:precision (list '= 2)) "x"))
  (: overhead (-> Real String))
  (define (overhead n) (round2 (/ n baseline)))
  (: num+percent (-> Real String))
  (define (num+percent n) (format "~a (~a%)" n (round (* 100 (/ n numvars)))))
  (: text->pict (-> String Pict))
  (define (text->pict message) (text message face size))
  (: text->title (-> String Pict))
  (define (text->title message) (text message (cons 'bold face) (+ 1 size)))
  (define left-column
    (vr-append vspace
               (text->title (or user-title (get-project-name sm))) ;; BOLDER
               (text->pict "typed/untyped ratio")
               (text->pict "max. overhead")
               (text->pict "mean overhead")
               (text->pict (format "~a-deliverable" (* 100 PARAM-N)))
               (text->pict (format "~a/~a-usable" (* 100 PARAM-N) (* 100 PARAM-M)))
               ))
  (define right-column
    (vr-append vspace (text->pict (format "(~a modules)" (get-num-modules sm)))
    (ht-append (/ hspace 2) (vr-append vspace
               (text->pict (overhead (typed-mean sm)))
               (text->pict (overhead (max-lattice-point sm)))
               (text->pict (overhead (avg-lattice-point sm)))
               (text->pict (num+percent (deliverable sm PARAM-N)))
               (text->pict (num+percent (usable sm PARAM-N PARAM-M)))
               ) (blank 0 0))))
  (vl-append vpad
             (hc-append hspace left-column right-column)
             (blank 1 vpad)))

;; =============================================================================

;(module+ test
;  (require rackunit)
;
;  ;; -- infer-graph
;  (check-equal? "foo/bar/../module-graphs/baz.tex"
;                (path->string (infer-graph (string->path "foo/bar/baz.rktd"))))
;  (check-equal? "foo/bar/../module-graphs/baz.tex"
;                (path->string (infer-graph (string->path "foo/bar/baz-and-other-ignored-stuff.rktd"))))
;
;  ;; -- all variations
;
;  ;; -- from rktd
;  (define sm (from-rktd "../data/echo.rktd"))
;  (check-equal? (stream->list (all-variations sm))
;                '("0000" "0001" "0010" "0011" "0100" "0101" "0110" "0111" "1000" "1001" "1010" "1011" "1100" "1101" "1110" "1111"))
;)
