#lang typed/racket/base

;; Data structure representing the results of one experiment.
;; Handles queries for raw data and statistical summary data.

(provide
  from-rktd
  ;; (->* [Path-String] [#:graph Path-String] Summary)
  ;; Convert a filepath to a summary object

  get-num-paths
  ;; (-> Summary Index)
  ;; Count the number of possible paths through a lattice

  get-num-configurations
  ;; (-> Summary Index)
  ;; Get the number of configurations from a Summary

  get-project-name
  ;; (-> Summary String)
  ;; Get the project name from a Summary

  has-typed?
  ;; (-> Summary String (Listof String) Boolean)
  ;; (has-typed? S v name*)
  ;; True if the module names `name*` are all typed in configuration `v`

  has-untyped?
  ;; (-> Summary String (Listof String) Boolean)
  ;; (has-untyped? S v name*)
  ;; True if all module names `name*` are untyped in configuration `v`

  typed-modules
  ;; (-> Summary BitString (Listof String))
  ;; Return a list of modules that are typed in this configuration

  untyped-modules
  ;; (-> Summary BitString (Listof String))
  ;; Return a list of modules that are untyped in this configuration

  untyped-mean
  ;; (-> Summary Real)
  ;; Get the mean runtime of the Summary's untyped configuration

  configuration->mean-runtime
  ;; (-> Summary String Real)
  ;; Get the mean runtime of a configuration, represented as a bitstring

  predicate->configurations
  ;; (-> Summary (-> String Boolean) (Streamof String))
  ;; Return a stream of configurations satisfying the predicate

  all-configurations
  ;; (-> Summary (Streamof String))
  ;; Return a stream of all configurations in the Summary

  all-paths
  ;; (-> Summary (Sequenceof LatticePath))
  ;; Return a stream of all paths through the lattice

  path->max-runtime
  ;; (-> Summary LatticePath Real)
  ;; Get the max runtime of any lattice point along a path

  summary->pict
  ;; (-> Summary Pict)
  ;; Return a pict representation of the summary. A kind of TLDR.

  summary-modulegraph
  summary->label
  Summary
)

;; -----------------------------------------------------------------------------

(require
  racket/path
  math/statistics
  (only-in math/number-theory factorial)
  (only-in racket/list last range) ;; because in-range has the wrong type
  (only-in racket/file file->value)
  (only-in racket/vector vector-append)
  (only-in racket/format ~r)
  (only-in racket/string string-split)
  "modulegraph.rkt"
  "bitstring.rkt"
  "pict-types.rkt"
  "stream-types.rkt"
)

;; =============================================================================
;; -- data definition: summary

(define-type Dataset (Vectorof (Listof Index)))

(: dataset? (-> Any Dataset))
(define (dataset? vec)
  (cast vec Dataset))

(struct summary (
  [source : Path-String] ;; the data's origin
  [dataset : Dataset] ;; the underlying experimental data
  [modulegraph : ModuleGraph] ;; the adjacency list of the represented project
  [num-runs : Index]
))

(define-type Summary summary)

(define-type LatticePath (Listof Bitstring))

(: summary->label (-> Summary String))
(define (summary->label S)
  (define p (summary-source S))
  (: s String)
  (define s (if (path? p) (path->string p) (format "~a" p)))
  (last (string-split (strip-suffix s) "/")))

;; -----------------------------------------------------------------------------
;; -- constants

;; Default location for TiKZ module graphs
(define MODULE_GRAPH_DIR "module-graphs")

;; -----------------------------------------------------------------------------
;; -- parsing

(define-syntax-rule (parse-error msg arg* ...)
  (error 'summary (format msg arg* ...)))

(define-syntax-rule (strip-suffix str)
  (car (string-split str ".")))

;; Create a summary from a raw dataset.
;; Infers the location of the module graph if #:graph is not given explicitly
(: from-rktd (->* [String] [#:graph (U Path #f)] Summary))
(define (from-rktd filename #:graph [graph-path #f])
  (define path (string->path filename))
  (define-values (dataset num-runs) (rktd->dataset path))
  (define gp (or graph-path (infer-graph path)))
  (define mg
    (if gp
      (from-tex gp)
      (from-directory (string->path (strip-suffix filename)))))
  (validate-modulegraph dataset mg)
  (summary path dataset mg num-runs))

;; Parse a dataset from a filepath.
(: rktd->dataset (-> Path (Values Dataset Index)))
(define (rktd->dataset path)
  ;; Check .rktd
  (unless (bytes=? #"rktd" (or (filename-extension path) #""))
    (parse-error "Cannot parse dataset '~a', is not .rktd" (path->string path)))
  ;; Get data
  (define vec (file->value path))
  ;; Check invariants
  (validate-dataset vec))

;; Confirm that the dataset `vec` is a well-formed vector of experiment results.
(: validate-dataset (-> Any (Values Dataset Index)))
(define (validate-dataset vec0)
  (define vec (dataset? vec0))
  (unless (< 0 (vector-length vec)) (parse-error "Dataset is an empty vector, does not contain any entries"))
  ;; Record the number of runs in the first vector, match against other lengths
  (: num-runs (Boxof (U #f Index)))
  (define num-runs (box #f))
  (for ([row-index (in-range (vector-length vec))])
    (define inner (vector-ref vec row-index))
    (define unboxed (unbox num-runs))
    (if (not unboxed)
        (set-box! num-runs (length inner))
        (unless (= unboxed (length inner)) (parse-error "Rows 0 and ~a of dataset have different lengths (~a vs. ~a); all configurations must describe the same number of runs.\n  Bad row: ~a" row-index unboxed (length inner) inner))))
  (values vec (or (unbox num-runs) (error 'neverhappens))))

;; Check that the dataset and module graph agree
(: validate-modulegraph (-> Dataset ModuleGraph Void))
(define (validate-modulegraph dataset mg)
  (define ds-num-modules (log2 (vector-length dataset)))
  (define mg-num-modules (length (module-names mg)))
  (unless (= ds-num-modules mg-num-modules)
    (parse-error "Dataset and module graph represent different numbers of modules. The dataset says '~a' but the module graph says '~a'" ds-num-modules mg-num-modules)))

;; Guess the location of the module graph matching the dataset
(: infer-graph (-> Path (U #f Path-String)))
(define (infer-graph path)
  ;; Get the prefix of the path
  (define tag (path->project-name path))
  ;; Search in the MODULE_GRAPH_DIR directory for a matching TeX file
  (define relative-pathstring (format "../~a/~a.tex" MODULE_GRAPH_DIR tag))
  (define gp (build-path (or (path-only path) (error 'infer-graph))
                         (string->path relative-pathstring)))
  (and (file-exists? gp) gp))

;; -----------------------------------------------------------------------------
;; -- querying

(: all-configurations (-> Summary (Sequenceof String)))
(define (all-configurations sm)
  (define M (get-num-modules sm))
  (stream-map (lambda ([n : Index]) (natural->bitstring n #:pad M))
              (in-range (get-num-configurations sm))))

(: all-paths (-> Summary (Sequenceof LatticePath)))
(define (all-paths S)
  (all-paths-from (natural->bitstring 0 #:pad (get-num-modules S))))

(: path->max-runtime (-> Summary LatticePath Nonnegative-Real))
(define (path->max-runtime S p*)
  (for/fold ([worst : Nonnegative-Real 0])
            ([c (in-list p*)])
    (let ([t (configuration->mean-runtime S c)])
      (max worst t))))

;; This is a hack, until we have a REAL definition of configurations that's
;; parameterized by the Summary object.
(: assert-configuration-length (-> Summary String Void))
(define (assert-configuration-length S v)
  (define N (get-num-modules S))
  (unless (= N (string-length v))
    (error 'assert-configuration-length (format "Expected a configuration with ~a modules, got '~a'" N v))))

(: get-module-names (-> Summary (Listof String)))
(define (get-module-names sm)
  (module-names (summary-modulegraph sm)))

(: get-num-paths (-> Summary Index))
(define (get-num-paths sm)
  (let ([r (factorial (get-num-modules sm))])
    (if (index? r)
        r
        (error 'get-num-paths "Factorial too large, not an index!\n"))))

(: get-num-configurations (-> Summary Index))
(define (get-num-configurations sm)
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

(: has-typed? (-> Summary String (Listof String) Boolean))
(define (has-typed? S v names*)
  (assert-configuration-length S v)
  (for/and ([name (in-list names*)])
    (bit-high? v (name->index (summary-modulegraph S) name))))

(: has-untyped? (-> Summary String (Listof String) Boolean))
(define (has-untyped? S v names*)
  (assert-configuration-length S v)
  (for/and ([name (in-list names*)])
    (bit-low? v (name->index (summary-modulegraph S) name))))

(: typed-modules (-> Summary String (Listof String)))
(define (typed-modules S v)
  (assert-configuration-length S v)
  (for/list ([i (in-list (range (string-length v)))]
             #:when (bit-high? v i))
    (index->name (summary-modulegraph S) i)))

(: untyped-modules (-> Summary String (Listof String)))
(define (untyped-modules S v)
  (assert-configuration-length S v)
  (for/list ([i : Natural (in-list (range (string-length v)))]
             #:when (bit-low? v i))
    (index->name (summary-modulegraph S) i)))

(: predicate->configurations (-> Summary (-> String Boolean) (Sequenceof String)))
(define (predicate->configurations sm p)
  (stream-filter p (all-configurations sm)))

;; Return all data for the untyped configuration
(: untyped-runtimes (-> Summary (Listof Index)))
(define (untyped-runtimes sm)
  (vector-ref (summary-dataset sm) 0))

(: untyped-mean (-> Summary Real))
(define (untyped-mean sm)
  (mean (untyped-runtimes sm)))

;; Return all data for the typed configuration
(: typed-runtimes (-> Summary (Listof Index)))
(define (typed-runtimes sm)
  (define vec (summary-dataset sm))
  (vector-ref vec (sub1 (vector-length vec))))

(: typed-mean (-> Summary Real))
(define (typed-mean sm)
  (mean (typed-runtimes sm)))

(: configuration->mean-runtime (-> Summary String Real))
(define (configuration->mean-runtime S v)
  (assert-configuration-length S v) ;; Is this going to be expensive?
  (index->mean-runtime S (bitstring->natural v)))

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
    0))

(: max-lattice-point (-> Summary Real))
(define (max-lattice-point sm)
  (fold-lattice sm max))

(: min-lattice-point (-> Summary Real))
(define (min-lattice-point sm)
  (fold-lattice sm min))

(: avg-lattice-point (-> Summary Real))
(define (avg-lattice-point sm)
  (define N (- (get-num-configurations sm) 2))
  (define 1/N (if (zero? N) 0 (/ 1 N)))
  (: f (-> Real Real Real))
  (define (f acc mean) (+ acc (* mean 1/N)))
  (fold-lattice sm f #:init 0))

;; Count the number of configurations with performance no worse than N times untyped
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
  (when (zero? baseline)
    (raise-user-error 'summary "Untyped runtime is 0ms. Cannot produce summary results."))
  (define numvars (get-num-configurations sm))
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
               (text->title (or user-title (get-project-name sm)))
               (text->pict "typed/untyped ratio")
               (text->pict "max. overhead")
               (text->pict "mean overhead")
               (text->pict (format "~a-deliverable" PARAM-N))
               (text->pict (format "~a/~a-usable" PARAM-N PARAM-M))))
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
;  ;; -- all configurations
;
;  ;; -- from rktd
;  (define sm (from-rktd "../data/echo.rktd"))
;  (check-equal? (stream->list (all-configurations sm))
;                '("0000" "0001" "0010" "0011" "0100" "0101" "0110" "0111" "1000" "1001" "1010" "1011" "1100" "1101" "1110" "1111"))
;)
