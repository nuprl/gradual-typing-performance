#lang racket/base

;; In-progress: script to build a data frame from a project folder.
;;
;; 0. Assumes the benchmark was instrumented to print performance stats
;;    and run at least once on all configurations
;; 1. Parses & collects data from each config.
;; 2. Writes a data frame, for later use with diagnose.rkt
;;
;; This code is mostly ugly, just beautifying the raw data enough to explore

(provide
  ;; -- Indexes for parsing the result of a `vector-set-performance-stats!`
  CPU-TIME
  REAL-TIME
  GC-TIME
  NUM-GC
  NUM-CTX
  NUM-SO
  NUM-THREADS
  NUM-STX
  NUM-HASH
  NUM-HASH-EXTRA
  NUM-EXTRA-BYTES
  PEAK-BYTES

  ;; -- 

  mean/vec
  ;; (-> (Vectorof Real) * (Vectorof Real))
  ;; Takes the component-wise mean of its argument vectors
)

;; -----------------------------------------------------------------------------

(require
  benchmark-util
  benchmark-util/count-chap-utils
  glob
  math/statistics
)

;; =============================================================================

(define NUM-ITERS 4)

(define CPU-TIME 0)
(define REAL-TIME 1)
(define GC-TIME 2)
(define NUM-GC 3)
(define NUM-CTX 4) ;; Context Switches
(define NUM-SO 5) ;; Internal Stack Overflows
(define NUM-THREADS 6)
(define NUM-STX 7)
(define NUM-HASH 8)
(define NUM-HASH-EXTRA 9)
(define NUM-EXTRA-BYTES 10)
(define PEAK-BYTES 11)

(define (get-time v)
  (vector-ref v CPU-TIME))

(define (get-gctime v)
  (vector-ref v GC-TIME))

(define (get-numgc v)
  (vector-ref v NUM-GC))

(define (get-peak-bytes v)
  (vector-ref v PEAK-BYTES))

(define (fname->configuration f)
  (cadr (regexp-match #rx"configuration([01]*)" f)))

;; Precondition: even length
(define (split-list x*)
  (cond
   [(null? x*)
    (values '() '())]
   [else
    (let-values ([(l r) (split-list (cddr x*))])
      (values (cons (car x*) l)
              (cons (cadr x*) r)))]))

;; Assumes each vector is the same length
(define (mean/vec vec*)
  (define acc (car vec*))
  (define len (vector-length acc))
  ;; Collapse into a new "total sum" vector
  (for* ([vec (in-list (cdr vec*))]
         [i (in-range len)])
    (let* ([acc_i (vector-ref acc i)]
           [acc_k (car acc_i)]
           [acc_v (cdr acc_i)]
           ;;
           [vec_i (vector-ref vec i)]
           [vec_k (car vec_i)]
           [vec_v (cdr vec_i)])
      (unless (= acc_k vec_k)
        (raise-user-error "Vector length mismatch" vec*))
      (vector-set! acc i (cons acc_k (+ acc_v vec_v)))))
  ;; Average each element
  (for ([i (in-range len)])
    (let* ([acc_i (vector-ref acc i)]
           [acc_k (car acc_i)]
           [acc_v (cdr acc_i)])
    (vector-set! acc i (cons acc_k (/ acc_v len)))))
  acc)

;; Return an indeterminately long list of labeled statistic vectors
(define (glob->row* g)
  (define-values (
     var*
     time*
     gctime*
     num-gc*
     peak-bytes*
     ;;
     proc-makes*
     proc-apps*
     proc-maxd*
     proc-depth*
     ;;
     struct-makes*
     struct-apps*
     struct-maxd*
     struct-depth*
     ;;
     vec-makes*
     vec-apps*
     vec-maxd*
     vec-depth*
    )
     (for/fold ([var* '()]
                [time* '()]
                [gctime* '()]
                [num-gc* '()]
                [peak-bytes* '()]
                ;;
                [proc-makes* '()]
                [proc-apps* '()]
                [proc-maxd* '()]
                [proc-depth* '()]
                ;;
                [struct-makes* '()]
                [struct-apps* '()]
                [struct-maxd* '()]
                [struct-depth* '()]
                ;;
                [vec-makes* '()]
                [vec-apps* '()]
                [vec-maxd* '()]
                [vec-depth* '()]
               )
               ([stat-file (in-glob g)])
       (define-values (vec* chap*)
         (with-input-from-file stat-file
           (lambda ()
             ;; Ignore first 2
             (read) (read)
             ;; Pair up the rest
             (split-list (for/list ([i (in-range (* 2 NUM-ITERS))]) (read))))))
       (values
         (cons (fname->configuration stat-file) var*)
         (cons (mean (map get-time vec*)) time*)
         (cons (mean (map get-gctime vec*)) gctime*)
         (cons (mean (map get-numgc vec*)) num-gc*)
         (cons (mean (map get-peak-bytes vec*)) peak-bytes*)
         ;;
         (cons (mean (map chaps-proc_makes chap*)) proc-makes*)
         (cons (mean (map chaps-proc_apps chap*)) proc-apps*)
         (cons (mean (map chaps-proc_maxdepth chap*)) proc-maxd*)
         (cons (mean/vec (map chaps-proc_depth* chap*)) proc-depth*)
         ;;
         (cons (mean (map chaps-struct_makes chap*)) struct-makes*)
         (cons (mean (map chaps-struct_apps chap*)) struct-apps*)
         (cons (mean (map chaps-struct_maxdepth chap*)) struct-maxd*)
         (cons (mean/vec (map chaps-struct_depth* chap*)) struct-depth*)
         ;;
         (cons (mean (map chaps-vec_makes chap*)) vec-makes*)
         (cons (mean (map chaps-vec_apps chap*)) vec-apps*)
         (cons (mean (map chaps-vec_maxdepth chap*)) vec-maxd*)
         (cons (mean/vec (map chaps-vec_depth* chap*)) vec-depth*)
        )))
  (list
    `(config ,(reverse var*))
    `(time ms ,(reverse time*))
    `(gctime ms ,(reverse gctime*))
    `(num-gc count ,(reverse num-gc*))
    `(peak-bytes bytes ,(reverse peak-bytes*))
    ;;
    `(proc-makes count ,(reverse proc-makes*))
    `(proc-apps count ,(reverse proc-apps*))
    `(proc-maxd count ,(reverse proc-maxd*))
    `(proc-depth count2 ,(reverse proc-depth*))
    ;;
    `(struct-makes count ,(reverse struct-makes*))
    `(struct-apps count ,(reverse struct-apps*))
    `(struct-maxd count ,(reverse struct-maxd*))
    `(struct-depth* count2 ,(reverse struct-depth*))
    ;;
    `(vec-makes count ,(reverse vec-makes*))
    `(vec-apps count ,(reverse vec-apps*))
    `(vec-maxd count ,(reverse vec-maxd*))
    `(vec-depth* count2 ,(reverse vec-depth*))
   ))

;; -----------------------------------------------------------------------------

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "collect-chaps"
   #:args FNAME*
   (unless (directory-exists? "perf")
     (make-directory  "perf"))
   ;; TODO doctor the 'main.rkt' scripts
   ;; TODO trigger a setup & run
   (for ([fname (in-list FNAME*)])
     (define row* (glob->row* (format "~a/benchmark/configuration*/chaps6.3.rktd" fname)))
     (with-output-to-file (format "perf/~a-6.3.rktd" fname) #:exists 'replace
       (lambda ()
         ;(displayln "#lang racket/base\n(provide (all-defined-out))")
         ;(printf ";; ~a\n" fname)
         (displayln "(")
         (for ([r (in-list row*)])
           (writeln r))
         (displayln ")"))))))
