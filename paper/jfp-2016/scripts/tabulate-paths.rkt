#lang racket/base

;; TODO add parameters
;; TODO get quad/gregor results

;; Answers the question:
;;    "For a given overhead N, how many paths
;;     never have unacceptable performance?"
;; By creating a table:
;; - rows    = overhead vs. untyped (variable N* in this file)
;; - columns = number of unacceptable configs along a path
;; - values  = percent of paths where `column` configs have overhead greater than `row`
;;
;; The number of columns varies by the project (num columns = num modules).
;; When counting the number of configs with/without bad overhead, we exclude the
;;  fully-untyped and fully-typed configuration (because they're constant over all paths)
;;
;; Caveat: script does not run for quad & gregor. Those are too large.

(require
  (only-in math/number-theory factorial)
  (only-in racket/string string-suffix?)
  gtp-summarize/summary
  racket/format
  racket/list
  racket/match
)

;; =============================================================================

(define *OUTPUT* (make-parameter #f))

;; -----------------------------------------------------------------------------

;; Overheads to test. Add as many as you want.
(define N* (vector 1 3 10 20))

(define (rnd n)
  (~r n #:precision (list '= 2)))

(define (print-header)
  (void)
  #;(printf ";; rows are N in ~a\n;; columns are % with at most M points above N\n" N*))

(define (print-footer)
  (void))

;; Create a 2D matrix for a given number of modules.
;; Returns two values: the matrix and a procedure for incrementing a matrix entry.
;; (-> Index (Values (Vectorof (Vectorof Natural)) (-> Natural Natural Void))))
(define (init-pathtbl num-modules)
  (define v* (for/vector ([_n (in-vector N*)])
               (make-vector num-modules 0)))
  (define (v++ r c)
    (let ([old (vector-ref (vector-ref v* r) c)])
      (vector-set! (vector-ref v* r) c (add1 old))))
  (values v* v++))

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "list-paths"
   #:once-each
   [("-o" "--output")
    o-p
    "Output path"
    (*OUTPUT* o-p)]
   #:args FNAME*
   (for ([fname (in-list FNAME*)])
     ;; -- For each valid .rktd file
     (unless (and (file-exists? fname)
                  (string-suffix? fname ".rktd"))
       (printf "Skipping invalid data file '~a'\n" fname))
     (printf "Processing '~a' ...\n" fname)
     (define S (from-rktd fname))
     ;; -- Create a matrix to fill
     (define-values (T T++) (init-pathtbl (get-num-modules S)))
     ;; -- For every path in the lattice
     (for ([p (all-paths S)])
       (define overhead*
         (map (lambda (c) (configuration->overhead S c))
              ;; Ignore the first and last variation
              (cdr (drop-right p 1))))
       ;; -- For every overhead (row), count the configs in each path that run
       ;;    no slower than the overhead
       (for ([r (in-range (vector-length N*))])
         (define n (vector-ref N* r))
         (T++ r (count (lambda (overhead) (> overhead n)) overhead*))))
     (define (print-paths)
       (print-header)
       (define total-paths (factorial (get-num-modules S)))
       (printf "#( ;; ~a, ~a total paths\n" (get-project-name S) total-paths)
       (for ([row (in-vector T)])
         (displayln (for/list ([v (in-vector row)])
           (~a (round (* (/ v total-paths) 100))
                        #:min-width 3
                        #:align 'right))))
       (displayln ")")
       (print-footer))
     (match (*OUTPUT*)
       [#f
        (print-paths)]
       [_
        (define out (format "paths-~a.rktd" (get-project-name S)))
        (with-output-to-file out #:exists 'replace
          print-paths)]))))
