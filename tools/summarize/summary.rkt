#lang typed/racket/base

;; TODO
;; - add immutable vectors to typed racket, so can parse the file

;; Data structure representing the results of one experiment.
;; Handles queries for raw data and statistical summary data.

(provide Summary)
(provide:
  (from-rktd
   (->* [Path-String] [#:graph (U Path #f)] Summary))
  ;; Convert a filepath to a summary object

  (get-num-paths
   (-> Summary Natural))
  ;; Count the number of possible paths through a lattice

  (get-num-modules
   (-> Summary Natural))
  ;; Get the number of configurations from a Summary

  (get-num-configurations
   (-> Summary Natural))
  ;; Get the number of configurations from a Summary

  (get-project-name
   (-> Summary String))
  ;; Get the project name from a Summary

  (has-typed?
   (-> Summary Bitstring (Listof String) Boolean))
  ;; (has-typed? S v name*)
  ;; True if the module names `name*` are all typed in configuration `v`

  (has-untyped?
   (-> Summary Bitstring (Listof String) Boolean))
  ;; (has-untyped? S v name*)
  ;; True if all module names `name*` are untyped in configuration `v`

  (typed-modules
   (-> Summary Bitstring (Listof String)))
  ;; Return a list of modules that are typed in this configuration

  (untyped-modules
   (-> Summary Bitstring (Listof String)))
  ;; Return a list of modules that are untyped in this configuration

  (untyped-mean
   (-> Summary Real))
  ;; Get the mean runtime of the Summary's untyped configuration

  (untyped-runtimes
   (-> Summary (Listof Index)))
  ;; Return data for the untyped configuration

  (typed-runtimes
   (-> Summary (Listof Index)))
  ;; Return data for the typed configuration

  (configuration->mean-runtime
   (-> Summary Bitstring Real))
  ;; Get the mean runtime of a configuration

  (configuration->runtimes
   (-> Summary Bitstring (Listof Index)))
  ;; Return runtimes for a configuration

  (configuration->stddev
   (-> Summary Bitstring Real))
  ;; Get the standard deviation runtime of a configuration

  (configuration->overhead
   (-> Summary Bitstring Real))
  ;; Get the overhead of a configuration, relative to untyped

  (predicate->configurations
   (-> Summary (-> Bitstring Boolean) (Sequenceof Bitstring)))
  ;; Return a stream of configurations satisfying the predicate

  (all-configurations
   (-> Summary (Sequenceof Bitstring)))
  ;; Return a stream of all configurations in the Summary

  (all-paths
   (-> Summary (Sequenceof LatticePath)))
  ;; Return a stream of all paths through the lattice

  (path->max-runtime
   (-> Summary LatticePath Real))
  ;; Get the max runtime of any lattice point along a path

  (min-runtime
   (-> Summary Real))
  ;; Lowest individual running time in lattice

  (max-runtime
   (-> Summary Real))
  ;; Highest individual running time in lattice

  (get-num-iterations
   (-> Summary Index))
  ;; Count number of iterations (columns) in dataset

  (summary->label
   (-> Summary String))

  (summary->pict
   (->* [Summary
         #:font-face String
         #:font-size Index
         #:N Index
         #:M Index
         #:height Real
         #:width Real]
        [#:title (U String #f)]
        Pict))
  ;; Return a pict representation of the summary. A kind of TLDR.

  (summary-modulegraph
   (-> Summary ModuleGraph))

  (summary-random-sample
   (-> Summary Natural #:replacement? Boolean (Listof Real)))

  (configuration->standard-error
   (-> Summary Bitstring Real))

  (configuration->confidence-lo
   (-> Summary Bitstring Real))

  (configuration->confidence-hi
   (-> Summary Bitstring Real))

  (untyped-configuration
   (-> Summary Bitstring))

  (typed-configuration
   (-> Summary Bitstring))

  (min-overhead
   (-> Summary Real))
  (max-overhead
   (-> Summary Real))
  (avg-overhead
   (-> Summary Real))

  (typed/untyped-ratio
   (-> (U Path-String Summary) Real))

  (summary->version
   (-> Summary String))

  (path->version
   (-> Path-String (U #f String)))

  (string->version
   (-> String (U #f String)))

  (D-deliverable
   (-> Real (-> Summary Natural)))
)
(provide
  ;; -- re-provides from modulegraph.rkt
  path->project-name
)

;; -----------------------------------------------------------------------------

(require
  (only-in math/number-theory factorial)
  (only-in racket/format ~r)
  (only-in racket/list last range) ;; because in-range has the wrong type
  (only-in racket/string string-split)
  (only-in racket/port with-input-from-string)
  (only-in racket/vector vector-append)
  (only-in typed/racket/random random-sample)
  gtp-summarize/bitstring
  gtp-summarize/modulegraph
  math/statistics
  racket/path
  racket/sequence
  typed/pict
)

(require/typed version/utils
  (valid-version? (-> String Boolean)))
(require/typed racket/string
  [string-suffix? (-> String String Boolean)])
(require/typed racket/file
  [file->value (-> Path-String Dataset)])
(require/typed gtp-summarize/stats-helpers
  [confidence-interval (-> (Listof Real) (Pairof Real Real))])

(define-type Pict pict)

;; =============================================================================
;; -- data definition: summary

(define-type Dataset (Vectorof (Listof Index)))

;; Cast an untyped value to a Dataset
(struct summary (
  [source : Path-String] ;; the data's origin
  [dataset : Dataset] ;; the underlying experimental data
  [modulegraph : ModuleGraph] ;; the adjacency list of the represented project
))

(define-type Summary summary)

(define-type LatticePath (Listof Bitstring))

(: summary->label (-> Summary String))
(define (summary->label S)
  (define p (summary-source S))
  (: s String)
  (define s (if (path? p) (path->string p) (format "~a" p)))
  (car (string-split (last (string-split s "/")) ".")))

(: summary->version (-> Summary String))
(define (summary->version S)
  (define p (summary-source S))
  (: s String)
  (define s (if (path? p) (path->string p) (format "~a" p)))
  (or
    (for/or : (Option String)
               ([x (in-list (string-split s "/"))]
                #:when (valid-version? x))
      x)
    (summary->label S)))

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
 (: from-rktd (->* [Path-String] [#:graph (U Path #f)] Summary))
(define (from-rktd filename #:graph [graph-path #f])
  (define path (if (path? filename) filename (string->path filename)))
  (define dataset (rktd->dataset path))
  (define gp (or graph-path (infer-graph path)))
  (define mg
    (if gp
      (tex->modulegraph gp)
      (begin
        (printf "Inferring module graph for '~a'.\n" filename)
        (project-name->modulegraph (path->project-name filename)))))
  (validate-modulegraph dataset mg)
  (summary path dataset mg))

;; Parse a dataset from a filepath.
(: rktd->dataset (-> Path Dataset))
(define (rktd->dataset path)
  ;; Check .rktd
  (unless (bytes=? #"rktd" (or (filename-extension path) #""))
    (parse-error "Cannot parse dataset '~a', is not .rktd" (path->string path)))
  (validate-dataset (file->value path)))

;; Confirm that the dataset `vec` is a well-formed vector of experiment results.
(: validate-dataset (-> Dataset Dataset))
(define (validate-dataset vec)
  (unless (< 0 (vector-length vec))
    (parse-error "Dataset is an empty vector, does not contain any entries"))
  (for ([row-index (in-range (vector-length vec))])
    (when (zero? (length (vector-ref vec row-index)))
      (parse-error "Row ~a has no data" row-index)))
  vec)

;; Check that the dataset and module graph agree
(: validate-modulegraph (-> Dataset ModuleGraph Void))
(define (validate-modulegraph dataset mg)
  (define ds-num-modules (log2 (vector-length dataset)))
  (define mg-num-modules (length (module-names mg)))
  (unless (= ds-num-modules mg-num-modules)
    (parse-error "Dataset and module graph represent different numbers of modules. The dataset says '~a' but the module graph says '~a'" ds-num-modules mg-num-modules)))

;; Guess the location of the module graph matching the dataset
(: infer-graph (-> Path (U #f Path)))
(define (infer-graph path)
  ;; Get the prefix of the path
  (define tag (path->project-name path))
  ;; Search in the MODULE_GRAPH_DIR directory for a matching TeX file
  (define relative-pathstring (string->path (format "./~a/~a.tex" MODULE_GRAPH_DIR tag)))
  (and (file-exists? relative-pathstring)
       relative-pathstring))

;; -----------------------------------------------------------------------------
;; -- querying

(: all-configurations (-> Summary (Sequenceof Bitstring)))
(define (all-configurations sm)
  (define M (get-num-modules sm))
  (sequence-map
    (lambda ([n : Index]) (natural->bitstring n #:pad M))
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
(: assert-configuration-length (-> Summary Bitstring Void))
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

(: get-num-modules (-> Summary Exact-Positive-Integer))
(define (get-num-modules sm)
  (define len (length (get-module-names sm)))
  (if (< 0 len) len (error 'too-few-modules)))

(: get-project-name (-> Summary String))
(define (get-project-name sm)
  (project-name (summary-modulegraph sm)))

(: has-typed? (-> Summary Bitstring (Listof String) Boolean))
(define (has-typed? S v names*)
  (assert-configuration-length S v)
  (for/and ([name (in-list names*)])
    (bit-high? v (name->index (summary-modulegraph S) name))))

(: has-untyped? (-> Summary Bitstring (Listof String) Boolean))
(define (has-untyped? S v names*)
  (assert-configuration-length S v)
  (for/and ([name (in-list names*)])
    (bit-low? v (name->index (summary-modulegraph S) name))))

(: typed-modules (-> Summary Bitstring (Listof String)))
(define (typed-modules S v)
  (assert-configuration-length S v)
  (for/list ([i (in-list (range (string-length v)))]
             #:when (bit-high? v i))
    (index->name (summary-modulegraph S) i)))

(: untyped-modules (-> Summary Bitstring (Listof String)))
(define (untyped-modules S v)
  (assert-configuration-length S v)
  (for/list ([i : Natural (in-list (range (string-length v)))]
             #:when (bit-low? v i))
    (index->name (summary-modulegraph S) i)))

(: predicate->configurations (-> Summary (-> Bitstring Boolean) (Sequenceof Bitstring)))
(define (predicate->configurations sm p)
  (sequence-filter p (all-configurations sm)))

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

(: configuration->mean-runtime (-> Summary Bitstring Real))
(define (configuration->mean-runtime S v)
  (assert-configuration-length S v) ;; Is this going to be expensive?
  (index->mean-runtime S (bitstring->natural v)))

(: configuration->runtimes (-> Summary Bitstring (Listof Index)))
(define (configuration->runtimes S v)
  (index->runtimes S (bitstring->natural v)))

(: index->runtimes (-> Summary Index (Listof Index)))
(define (index->runtimes S i)
  (vector-ref (summary-dataset S) i))

(: configuration->stddev (-> Summary Bitstring Real))
(define (configuration->stddev S v)
  (let ([m (configuration->mean-runtime S v)]
        [i (bitstring->natural v)])
    (stddev/mean m (vector-ref (summary-dataset S) i))))

(: index->confidence (-> Summary Index (Pairof Real Real)))
(define (index->confidence S i)
  (confidence-interval (index->runtimes S i)))

(: configuration->confidence-lo (-> Summary Bitstring Real))
(define (configuration->confidence-lo S v)
  (car (configuration->confidence S v)))

(: configuration->confidence-hi (-> Summary Bitstring Real))
(define (configuration->confidence-hi S v)
  (cdr (configuration->confidence S v)))

(: configuration->confidence (-> Summary Bitstring (Pairof Real Real)))
(define (configuration->confidence S v)
  (assert-configuration-length S v)
  (index->confidence S (bitstring->natural v)))

(: configuration->overhead (-> Summary Bitstring Real))
(define (configuration->overhead S v)
  (/ (configuration->mean-runtime S v) (untyped-mean S)))

(: index->mean-runtime (-> Summary Index Real))
(define (index->mean-runtime sm i)
  (mean (vector-ref (summary-dataset sm) i)))

;; Fold over lattice points. Excludes fully-typed and fully-untyped.
;; #:init : initial value for the fold
;; #:pre : converts a row of absolute runtimes to a single real number
;;         defaults to the `mean` of the absolute runtimes
(: fold-lattice (->* [Summary (-> Real Real Real)] [#:init (U #f Real) #:pre (U #f (-> (Listof Real) Real))] Real))
(define (fold-lattice S f #:init [init #f] #:pre [pre #f])
  (define pre+ (if pre pre mean))
  (define D (summary-dataset S))
  (fold-lattice/vector D f init pre+))

(: fold-lattice/vector (-> Dataset (-> Real Real Real) (U #f Real) (-> (Listof Real) Real) Real))
(define (fold-lattice/vector D f init pre)
 (or
  (for/fold : (U #f Real)
            ([prev : (U #f Real) init])
            ([i    (in-range (vector-length D))])
    (define val (pre (vector-ref D i)))
    (if prev
      (f prev val)
      val))
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

(: min* (-> (Listof Real) Real))
(define (min* r*)
  (or (for/fold ([prev : (U #f Real) #f])
                ([r (in-list r*)])
        (if prev
          (min prev r)
          r))
      (raise-user-error 'min* "Empty list")))

(: max* (-> (Listof Real) Real))
(define (max* r*)
  (or
    (for/fold ([prev : (U #f Real) #f])
              ([r (in-list r*)])
      (if prev
        (max prev r)
        r))
    (raise-user-error 'max* "Empty list")))

(: min-runtime (-> Summary Real))
(define (min-runtime S)
  (fold-lattice S min #:pre min*))

(: max-runtime (-> Summary Real))
(define (max-runtime S)
  (fold-lattice S max #:pre max*))

(: get-num-iterations (-> Summary Index))
(define (get-num-iterations S)
  (assert (fold-lattice S max #:pre length) index?))

;; Count the number of configurations with performance no worse than N times untyped
(: deliverable (-> Summary Real Natural))
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

(: D-deliverable (-> Real (-> Summary Natural)))
(define ((D-deliverable D) S)
  (deliverable S D))

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

(: index->stddev (-> Summary Index Real))
(define (index->stddev S i)
  (stddev (index->runtimes S i)))

;; -----------------------------------------------------------------------------
;; misc from JFP branch

(: untyped-configuration (-> Summary Bitstring))
(define (untyped-configuration S)
  (make-string (get-num-modules S) #\0))

(: typed-configuration (-> Summary Bitstring))
(define (typed-configuration S)
  (make-string (get-num-modules S) #\1))

(: configuration->standard-error (-> Summary Bitstring Real))
(define (configuration->standard-error S v)
  (assert-configuration-length S v)
  (index->stddev S (bitstring->natural v)))

(: summary-random-sample (-> Summary Natural #:replacement? Boolean (Listof Real)))
(define (summary-random-sample S sample-size #:replacement? r)
  (summary-random-sample/vector (summary-dataset S) sample-size r))

(: summary-random-sample/vector (-> Dataset Natural Boolean (Listof Real)))
(define (summary-random-sample/vector D sample-size r)
  ;; Get a bunch of random means
  (define t**
    ((inst random-sample (Listof Index)) D sample-size #:replacement? r))
  (for/list : (Listof Real)
            ([t* (in-list t**)])
    (mean t*)))

(: min-overhead (-> Summary Real))
(define (min-overhead S)
  (/ (min-lattice-point S) (untyped-mean S)))

(: max-overhead (-> Summary Real))
(define (max-overhead S)
  (/ (max-lattice-point S) (untyped-mean S)))

(: avg-overhead (-> Summary Real))
(define (avg-overhead S)
  (/ (avg-lattice-point S) (untyped-mean S)))

(: data-line? (-> (U String EOF) Boolean))
(define (data-line? str)
  (if (string? str)
    (and (< 0 (string-length str))
         (eq? #\( (string-ref str 0)))
    #f))

(: string->index* (-> String (Listof Index)))
(define (string->index* str)
  (cast (with-input-from-string str read) (Listof Index)))

;;bg: originally accepted path-string input
(: typed/untyped-ratio (-> (U Path-String Summary) Real))
(define (typed/untyped-ratio d)
  (if (summary? d)
    (typed/untyped-ratio/S d)
    (typed/untyped-ratio/path d)))

(: typed/untyped-ratio/S (-> Summary Real))
(define (typed/untyped-ratio/S S)
  (/ (typed-mean S) (untyped-mean S)))

(: read-untyped (-> (Listof Real)))
(define (read-untyped)
  (or (for/or : (U #f (Listof Index))
              ([ln (in-lines)])
        (and (data-line? ln) (string->index* ln)))
      (raise-user-error 'read-untyped "No data lines in current input port")))

(: read-typed (-> (Listof Real)))
(define (read-typed)
  (define last-line
    (or (for/fold : (U #f String)
                  ([acc : (U #f String) #f])
                  ([ln (in-lines)])
          (if (data-line? ln) ln acc))
        (raise-user-error 'read-typed "No data lines in current input port")))
  (string->index* last-line))

(: typed/untyped-ratio/path (-> Path-String Real))
(define (typed/untyped-ratio/path p)
  (with-input-from-file p
    (lambda ()
      (let ([u (mean (read-untyped))]
            [t (mean (read-typed))])
        (/ t u)))))

(: string->version (-> String (U #f String)))
(define (string->version s)
  (for/or : (Option String)
             ([x (in-list (string-split s "/"))]
              #:when (valid-version? x))
    x))

(: path->version (-> Path-String (U #f String)))
(define (path->version ps)
  (string->version (if (path? ps) (path->string ps) ps)))

;; -----------------------------------------------------------------------------
;; --- viewing

(: summary->pict (->* [Summary
                       #:font-face String
                       #:font-size Index
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
  (define (text->title message) (text message (cons 'bold face) (assert (+ 1 size) index?)))
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

;; Open a REPL with 
(module+ main
  (require racket/cmdline)
  (command-line
   #:program "gtp-summarize"
   #:args (filename)
   (when (and (string? filename) (string-suffix? filename ".rktd"))
     (parameterize ([current-namespace (make-base-namespace)])
       (printf "[INFO] Starting REPL ...\n")
       (namespace-require 'gtp-summarize/summary)
       (namespace-require 'xrepl) ;; Optional
       (printf "[INFO] (Command-line argument bound to variable FNAME)\n")
       (namespace-set-variable-value! 'FNAME filename)
       (read-eval-print-loop)))
   (raise-user-error 'gtp-summarize (format "Expected .rktd file, got '~a'" filename))
))

;; =============================================================================

(module+ test
  (require
    typed/rackunit)

  ;; -- infer-graph
  (define-syntax-rule (check-infer-graph in out)
    (let ([inferred (infer-graph (string->path in))])
      (check-not-false inferred)
      (check-equal? (path->string (or inferred (error 'test-fail))) out)))
  ;(check-infer-graph "foo/bar/baz.tex" "foo/bar/../module-graphs/baz.tex")
  ;(check-equal? "foo/bar/baz-and-other-ignored-stuff.rktd" "foo/bar/../module-graphs/baz.tex")

  ;;; -- from rktd
  (define S (from-rktd "test/morsecode-data.rktd"))


  (check-equal? (get-num-paths S) 24)

  (check-equal? (get-num-modules S) 4)

  (check-equal? (get-num-configurations S) (expt 2 4))

  (check-equal? (get-project-name S) "morsecode")

  ;; -- has-typed?
  (check-true (has-typed? S "1111" '("main")))
  (check-true (has-typed? S "0110" '("main")))
  (check-true (has-typed? S "1001" '("levenshtein" "morse-code-table")))
  (check-true (has-typed? S "0000" '()))

  (check-false (has-typed? S "0001" '("levenshtein")))
  (check-false (has-typed? S "0000" '("main")))
  (check-false (has-typed? S "0011" '("levenshtein" "main")))

  ;; -- has-untyped?
  (check-true (has-untyped? S "0001" '("levenshtein")))
  (check-true (has-untyped? S "0000" '("main")))
  (check-true (has-untyped? S "0011" '("levenshtein" "main")))

  (check-false (has-untyped? S "1111" '("main")))
  (check-false (has-untyped? S "1010" '("morse-code-strings")))
  (check-false (has-untyped? S "1001" '("levenshtein" "main")))

  ;; -- typed-modules
  (check-equal? (typed-modules S "1111")
                '("levenshtein" "main" "morse-code-strings" "morse-code-table"))
  (check-equal? (typed-modules S "0000") '())
  (check-equal? (typed-modules S "1001") '("levenshtein" "morse-code-table"))

  ;; -- untyped-modules
  (check-equal? (untyped-modules S "0000")
                '("levenshtein" "main" "morse-code-strings" "morse-code-table"))
  (check-equal? (untyped-modules S "1111") '())
  (check-equal? (untyped-modules S "1001") '("main" "morse-code-strings"))

  ;; -- untyped-mean
  (check-equal? (untyped-mean S) 5666/3)
  
  ;; -- config->mean
  (check-equal? (configuration->mean-runtime S "0000") (untyped-mean S))
  (check-equal? (configuration->mean-runtime S "0010") 10563/5)

  ;; -- config->stddev
  (check-equal? (configuration->stddev S "0000") 59.43923133942953)
  (check-equal? (configuration->stddev S "0111") 27.806394148748513)

  (define (check-confidence-interval [str : String])
    (let* ([ci (configuration->confidence S str)]
           [m (configuration->mean-runtime S str)])
      (check-true (< (car ci) (cdr ci)))
      (check-true (< (car ci) m))
      (check-true (< m (cdr ci)))))

  (check-confidence-interval "0000")
  (check-confidence-interval "0010")

  ;; -- config->stddev
  (check-equal? (configuration->stddev S "0000") 59.43923133942953)
  (check-equal? (configuration->stddev S "0111") 27.806394148748513)

  ;; -- config->overhead
  (check-equal? (configuration->overhead S "0000") 1)
  (check-equal? (configuration->overhead S "0101") 15221/14165)

  ;; -- predicate->configs
  (check-equal?
    (sort
      (sequence->list (predicate->configurations S (lambda (cfg) (has-typed? S cfg '("morse-code-strings")))))
      string<?)
    '("0010" "0011" "0110" "0111" "1010" "1011" "1110" "1111"))

  ;; -- all-configs
  (check-equal?
    (sequence->list (all-configurations S))
    '("0000" "0001" "0010" "0011" "0100" "0101" "0110" "0111" "1000" "1001" "1010" "1011" "1100" "1101" "1110" "1111"))

  ;; -- all-paths
  (check-equal?
    (sequence-ref (all-paths S) 0)
    '("0000" "1000" "1100" "1110" "1111"))

  ;; -- path->max
  (check-equal?
    (sequence->list
      (sequence-map (lambda ([p : LatticePath]) (path->max-runtime S p)) (all-paths S)))
   '(20883/10 20463/10 20883/10 62093/30 20463/10 62017/30 20883/10 20463/10 12703/6 12703/6 20463/10 31594/15 10563/5 10563/5 12703/6 12703/6 10563/5 10563/5 20463/10 62017/30 20463/10 31594/15 62777/30 31594/15))

  ;; -- summary->label
  (check-equal? (summary->label S) "morsecode-data")

  ;; -- summary->version
  (check-equal? (summary->version S) "morsecode-data")

  ;; -- get-num-iterations
  (check-equal? (get-num-iterations S) 30)

  ;; -- min-overhead max-overhead min-runtime max-runtime
  (let ([lo-r (min-runtime S)]
        [lo-o (min-overhead S)]
        [hi-o (max-overhead S)]
        [hi-r (max-runtime S)]
        [u-r (untyped-mean S)])
    (check-true (< lo-r hi-r))
    (check-true (< lo-o hi-o))
    (check-true (< (/ lo-r u-r) lo-o))
    (check-true (< hi-o (/ hi-r u-r))))

  ;; -- summary->pict
  ;; -- summary-modulegraph

  ;(check-equal? (sequence->list (all-configurations sm))
  ;              '("0000" "0001" "0010" "0011" "0100" "0101" "0110" "0111" "1000" "1001" "1010" "1011" "1100" "1101" "1110" "1111"))

  ;; -- all configurations

)

