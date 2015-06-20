#lang racket/base

;; Data structure representing the results of one experiment.
;; Handles queries for raw data and statistical summary data.

(provide
  ;; (->* [Path-String] [#:graph Path-String] Summary)
  from-rktd
)

;; -----------------------------------------------------------------------------

(require
  racket/path
  racket/stream
  (only-in racket/file file->value)
  (prefix-in mg- "modulegraph.rkt")
  "bitstring.rkt"
)

;; =============================================================================
;; -- data definition: summary

(struct summary (
  source      ;; : Path-String, the data's origin
  dataset     ;; : (Vectorof (Vectorof Index)), the underlying experimental data
  modulegraph ;; : ModuleGraph, the adjacency list of the represented project
))

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
;; (: from-rktd (->* [String] [#:graph (U Path #f)] Summary))
(define (from-rktd filename #:graph [graph-path #f])
  (define path (string->path filename))
  (define dataset (rktd->dataset path))
  (define mg (mg-from-tex (or graph-path (infer-graph path))))
  (validate-modulegraph dataset mg)
  (summary path dataset mg))

;; Parse a dataset from a filepath.
;; (: rktd->dataset (-> Path (Vectorof (Vectorof Index))))
(define (rktd->dataset path)
  ;; Check .rktd
  (unless (bytes=? #"rktd" (filename-extension path))
    (parse-error "Cannot parse dataset '~a', is not .rktd" (path->string path)))
  ;; Get data
  (define vec (file->value path))
  ;; Check invariants
  (validate-dataset vec))

;; Confirm that the dataset `vec` is a well-formed vector of experiment results.
;; (: validate-dataset (-> Any (Vectorof (Vectorof Index))))
(define (validate-dataset vec)
  (unless (vector? vec) (parse-error "Dataset is not a vector"))
  (unless (< 0 (vector-length vec)) (parse-error "Dataset is an empty vector, does not contain any entries"))
  ;; Record the number of runs in the first vector, match against other lengths
  (define num-runs (box #f))
  (for ([row-index (in-range (vector-length vec))])
    (define inner (vector-ref (vec row-index)))
    (unless (vector? inner) (parse-error "Dataset is not a vector of vectors, found non-vector entry '~a'" inner))
    (if (not (unbox num-runs))
        (set-box! num-runs (vector-length inner))
        (unless (= num-runs (vector-length inner)) (parse-error "Rows 0 and ~a of dataset have different lengths; all variations must describe the same number of runs" row-index)))
    (for ([val (in-vector inner)])
      (unless (exact-positive-integer? val)
        (parse-error "Row vector contains non-integer entry '~a'" val))))
    vec)

;; Check that the dataset and module graph match
;; (: validate-modulegraph (-> (Vectorof (Vectorof Index)) ModuleGraph Void))
(define (validate-modulegraph dataset mg)
  (define ds-num-modules (log2 (vector-length dataset)))
  (define mg-num-modules (length mg-module-names mg))
  (unless (= ds-num-modules mg-num-modules)
    (parse-error "Dataset and module graph represent different numbers of modules. The dataset says '~a' but the module graph says '~a'" ds-num-modules mg-num-modules)))

;; Guess the location of the module graph matching the dataset
;; (: infer-graph (-> Path Path-String))
(define (infer-graph path)
  ;; Get the prefix of the path
  (define tag (mg-path->project-name path))
  ;; Search in the MODULE_GRAPH_DIR directory for a matching TeX file
  (define relative-pathstring (format "../~a/~a.tex" MODULE_GRAPH_DIR tag))
  (build-path (path-only path)
              (string->path relative-pathstring)))

;; -----------------------------------------------------------------------------
;; -- querying

(define (all-variations sm)
  (define M (get-num-modules sm))
  (stream-map natural->binary
              (in-range (get-num-variations sm))))

(define (get-module-names sm)
  (mg-module-names (summary-modulegraph sm)))

(define (get-num-variations sm)
  (vector-length (summary-dataset sm)))

(define (get-num-modules sm)
  (length (get-module-names sm)))

(define (get-project-name sm)
  (mg-project-name (summary-modulegraph sm)))

(define (predicate->variations p sm)
  (stream-filter p (all-variations sm)))

;; =============================================================================

(module+ test
  (require rackunit)

  ;; -- infer-graph
  (check-equal? "foo/bar/../module-graphs/baz.tex"
                (path->string (infer-graph (string->path "foo/bar/baz.rktd"))))
  (check-equal? "foo/bar/../module-graphs/baz.tex"
                (path->string (infer-graph (string->path "foo/bar/baz-and-other-ignored-stuff.rktd"))))

)
