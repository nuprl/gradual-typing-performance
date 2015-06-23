#lang racket/base

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
  ;; (-> Summary Pict)
  summary->pict
)

;; -----------------------------------------------------------------------------

(require
  racket/path
  racket/stream
  math/statistics
  (only-in racket/file file->value)
  (only-in racket/vector vector-append)
  (only-in racket/format ~r)
  (prefix-in mg- "modulegraph.rkt")
  "bitstring.rkt"
  pict
)

(define (TODO) (error "not implemented"))

;; =============================================================================
;; -- data definition: summary

(struct summary (
  source      ;; Path-String, the data's origin
  dataset     ;; (Vectorof (Listof Index)), the underlying experimental data
  modulegraph ;; ModuleGraph, the adjacency list of the represented project
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
;; (: rktd->dataset (-> Path (Vectorof (Listof Index))))
(define (rktd->dataset path)
  ;; Check .rktd
  (unless (bytes=? #"rktd" (filename-extension path))
    (parse-error "Cannot parse dataset '~a', is not .rktd" (path->string path)))
  ;; Get data
  (define vec (file->value path))
  ;; Check invariants
  (validate-dataset vec))

;; Confirm that the dataset `vec` is a well-formed vector of experiment results.
;; (: validate-dataset (-> Any (Vectorof (Listof Index))))
(define (validate-dataset vec)
  (unless (vector? vec) (parse-error "Dataset is not a vector"))
  (unless (< 0 (vector-length vec)) (parse-error "Dataset is an empty vector, does not contain any entries"))
  ;; Record the number of runs in the first vector, match against other lengths
  (define num-runs (box #f))
  (for ([row-index (in-range (vector-length vec))])
    (define inner (vector-ref vec row-index))
    (unless (list? inner) (parse-error "Dataset is not a vector of lists found non-list entry '~a'" inner))
    (if (not (unbox num-runs))
        (set-box! num-runs (length inner))
        (unless (= (unbox num-runs) (length inner)) (parse-error "Rows 0 and ~a of dataset have different lengths (~a vs. ~a); all variations must describe the same number of runs.\n  Bad row: ~a" row-index (unbox num-runs) (length inner) inner)))
    (for ([val (in-list inner)])
      (unless (exact-positive-integer? val)
        (parse-error "Row ~a contains nonsense entry '~a'" row-index val))))
    vec)

;; Check that the dataset and module graph agree
;; (: validate-modulegraph (-> (Vectorof (Listof Index)) ModuleGraph Void))
(define (validate-modulegraph dataset mg)
  (define ds-num-modules (log2 (vector-length dataset)))
  (define mg-num-modules (length (mg-module-names mg)))
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
  (stream-map (lambda (n) (natural->bitstring n #:pad M))
              (in-range (get-num-variations sm))))

(define (get-module-names sm)
  (mg-module-names (summary-modulegraph sm)))

(define (get-num-variations sm)
  (vector-length (summary-dataset sm)))

(define (get-num-runs sm)
  ;; All rows should have the same length, else `from-rktd` screwed up.
  (define arbitrary-row (vector-ref (summary-dataset sm) 0))
  (length arbitrary-row))

(define (get-num-modules sm)
  (length (get-module-names sm)))

(define (get-project-name sm)
  (mg-project-name (summary-modulegraph sm)))

(define (predicate->variations sm p)
  (stream-filter p (all-variations sm)))

;; Return all data for the untyped variation
(define (untyped-runtimes sm)
  (vector-ref (summary-dataset sm) 0))

(define (untyped-mean sm)
  (mean (untyped-runtimes sm)))

;; Return all data for the typed variation
(define (typed-runtimes sm)
  (define vec (summary-dataset sm))
  (vector-ref vec (sub1 (vector-length vec))))

(define (typed-mean sm)
  (mean (typed-runtimes sm)))

(define (variation->mean-runtime sm var)
  (index->mean-runtime sm (bitstring->natural var)))

(define (index->mean-runtime sm i)
  (mean (vector-ref (summary-dataset sm) i)))

;; Efficient enough?
(define (all-gt-runtimes sm)
  (define data (summary-dataset sm))
  (apply append
         (for/list ([i (in-range 1 (vector-length data))])
           (vector-ref data i))))

;; Get the worst of all GRADUALLY typed running times (excludes typed & untyped)
(define (max-runtime sm)
  (for/fold ([prev-max #f])
            ([val (in-list (all-gt-runtimes sm))])
    (or (and prev-max (max prev-max val)) val)))

;; Get the average over all gradually typed running times (exludes typed & untyped)
;; Takes into account every single measured data point.
(define (avg-runtime sm)
  (mean (all-gt-runtimes sm)))

;; -----------------------------------------------------------------------------
;; --- viewing

(define (summary->pict sm
                       #:font-face face
                       #:font-size size
                       #:height height
                       #:width width
                       #:title [user-title #f])
  (define vspace (/ size 3))
  (define hspace (/ width 3))
  (define vpad (/ height 5))
  (define baseline (untyped-mean sm))
  (define (round2 n) (string-append (~r n #:precision (list '= 2)) "x"))
  (define (overhead n) (round2 (/ n baseline)))
  (define (text->pict message) (text message face size))
  (define (text->title message) (text message (cons 'bold face) (+ 1 size)))
  (define left-column
    (vr-append vspace
               (text->title (or user-title (get-project-name sm))) ;; BOLDER
               (text->pict "τ overhead")
               (text->pict "max. overhead")
               (text->pict "avg. overhead")))
  (define right-column
    (vr-append vspace
               (text->pict (format "(~a modules)" (get-num-modules sm)))
               (text->pict (overhead (typed-mean sm)))
               (text->pict (overhead (max-runtime sm)))
               (text->pict (overhead (avg-runtime sm)))))
  (vl-append vpad
             (hc-append hspace left-column right-column)
             (blank 1 vpad)))

;; =============================================================================

(module+ test
  (require rackunit)

  ;; -- infer-graph
  (check-equal? "foo/bar/../module-graphs/baz.tex"
                (path->string (infer-graph (string->path "foo/bar/baz.rktd"))))
  (check-equal? "foo/bar/../module-graphs/baz.tex"
                (path->string (infer-graph (string->path "foo/bar/baz-and-other-ignored-stuff.rktd"))))

  ;; -- all variations

  ;; -- from rktd
  (define sm (from-rktd "../data/echo.rktd"))
  (check-equal? (stream->list (all-variations sm))
                '("0000" "0001" "0010" "0011" "0100" "0101" "0110" "0111" "1000" "1001" "1010" "1011" "1100" "1101" "1110" "1111"))
)
