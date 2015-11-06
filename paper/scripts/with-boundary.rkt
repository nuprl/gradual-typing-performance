#lang racket/base

;; Quick script: just print the runtime with/without a given
;;  set of modules being part of ANY typed/untyped boundary.

(require
 racket/set
 racket/stream
 math/statistics
 racket/format
 "modulegraph.rkt"
 "summary.rkt"
 )

;; Get the index matching the module name
;; (: name->index (-> ModuleGraph String Index))
(define (name->index mg name)
  (define maybe-i
    (for/first
               ([node+neighbors (in-list (modulegraph-adjlist mg))]
                [i (in-range 0 (length (modulegraph-adjlist mg)))]
                #:when (string=? name (car node+neighbors)))
      i))
  (unless maybe-i
    (error 'name->index (format "Invalid module name ~a" name)))
  maybe-i)

;; Get the name of the module with index `i`
;; (: index->name (-> ModuleGraph Index String))
(define (index->name mg i)
  (car (list-ref (modulegraph-adjlist mg) i)))

;; Get the names of modules required by `name`.
;; (: requires (-> ModuleGraph String (Listof String)))
(define (required-by mg name)
  (for/list ([node+neighbors (in-list (modulegraph-adjlist mg))]
             #:when (member name (cdr node+neighbors)))
    (car node+neighbors)))

(define (my-round n)
  (~r n #:precision (list '= 2)))

(define (show-boundary S data-modules)
  (define MG (summary-modulegraph S)) ;; yuck
  (define data+required-by
    (for/list ([d (in-list data-modules)])
      (define d+r (cons d (required-by MG d)))
           (map (lambda (n) (name->index MG n)) d+r)))
  (when (not (null? (cdr (car data+required-by))))
    (define (has-boundary? v)
      (for/or ([d+r* (in-list data+required-by)])
        (define d-index (car d+r*))
        (for/or ([r-index (in-list (cdr d+r*))])
          (not (char=? (string-ref v d-index) (string-ref v r-index))))))
    (define (no-boundary? v) (not (has-boundary? v)))
    (define with-data    (predicate->configurations S has-boundary?))
    (define without-data (predicate->configurations S no-boundary?))
    (define (stream->mean s)
     (define-values (len sum)
       (for/fold ([len 0] [sum 0])
                 ([v (in-stream s)])
         (values (add1 len) (+ (configuration->mean-runtime S v) sum))))
     (when (zero? len)
       (raise-user-error (format "Empty subset for boundary defined by module list ~a. Adjacency list is ~a and computed data+required-by is ~a\n" data-modules (modulegraph-adjlist MG) data+required-by)))
     (/ sum len))
    (define with (stream->mean with-data))
    (define without (stream->mean without-data))
    (define pct (* 100 (/ (- with without) without)))
    (printf "~a\t~a\t~a\t~a\n" data-modules (my-round without) (my-round with) (my-round pct))))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  (command-line
   #:args (FILE.rktd . data-modules)
   (begin
     ;; Given multiple arguments, show results when any/none are boundaries
     ;; Given 1 argument, show results for each FILE in the project
     (define S (from-rktd FILE.rktd))
     (printf "MODULES\tNEVER-BOUNDARY\tALWAYS-BOUNDARY\tPCT.DIFF\n")
     (cond
      [(null? data-modules)
       (for ([mn (in-list (module-names (summary-modulegraph S)))])
         (show-boundary S (list mn)))]
      [else
       (show-boundary S data-modules)]))))
