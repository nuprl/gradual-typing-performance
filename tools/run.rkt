#lang racket/base

;; Benchmark driver

(require "data-lattice.rkt"
         math/statistics
         pict
         racket/class
         racket/cmdline
         racket/draw
         racket/match
         racket/system
         racket/vector)

;; Flag to set compilation-only mode. If #f it will compile and
;; run, otherwise only compiles.
(define only-compile? (make-parameter #f))

;; The number of times to run the benchmark as a string (to
;; be parsed later)
(define num-iterations (make-parameter "1"))

;; Paths to write results/diagrams
(define output-path (make-parameter #f))
(define lattice-path (make-parameter #f))

;; Get paths for all variation directories
;; Path Path -> Listof Path
(define (mk-variations basepath entry-point)
  (for/list ([file (in-list (directory-list (build-path basepath "benchmark")))]
             #:when (not (equal? "base" (path->string file))))
    (build-path basepath "benchmark" file entry-point)))

;; Run the variations for each variation directory
;; (Listof Path) Nat -> Results
(define (run-benchmarks basepath entry-point iters)
  (define variations (mk-variations basepath entry-point))
  (define results (make-vector (length variations)))
  (for ([(var var-idx) (in-indexed (in-list variations))])
    (define-values (new-cwd file _2) (split-path var))
    (define times null)
    (for ([i (in-range iters)])
      (printf "iteration #~a of ~a~n" i var)
      ;; FIXME: use benchmark library
      (parameterize ([current-directory new-cwd])
        (system (string-append "raco make -v " (path->string file)))
        (unless (only-compile?)
          (define-values (_ cpu real gc)
            (time-apply
             (λ () (system (string-append "racket " (path->string file))
                           #:set-pwd? #t))
             null))
          (printf "cpu: ~a real: ~a gc: ~a~n" cpu real gc)
          (set! times (cons real times)))))
    ;; the order of runs doesn't really matter, but keep them in the order
    ;; they were run anyway just in case
    (vector-set! results var-idx (reverse times)))
  results)

(struct benchmark (empty-results results) #:prefab)

(module+ main
  (match-define (list basepath entry-point)
    (command-line #:program "benchmark-runner"
                  #:once-each
                  [("-c" "--only-compile") "Only compile and don't run"
                                           (only-compile? #t)]
                  [("-o" "--output") o-p
                                     "A path to write data to"
                                     (output-path o-p)]
                  [("-l" "--latice") l-p
                                     "A path to write the lattice diagram to"
                                     (lattice-path l-p)]
                  #:multi
                  [("-i" "--iterations") n-i
                                         "The number of iterations to run"
                                         (num-iterations n-i)]
                  #:args (basepath entry-point)
                  (list basepath entry-point)))
  (unless (path-string? basepath)
    (raise-user-error (format "expected a path, given ~a" basepath)))
  (unless (and (path-string? entry-point)
               (regexp-match? #rx"\\.rkt$" entry-point))
    (raise-user-error (format "expected a Racket file, given ~a" entry-point)))
  (define iters (string->number (num-iterations)))
  (unless (number? iters)
    (raise-user-error (format "expected a number, given ~a" (num-iterations))))

  (define empty-results    (run-benchmarks "empty" entry-point iters))
  (define specific-results (run-benchmarks basepath entry-point iters))
  (define full-results (benchmark empty-results specific-results))

  (when (output-path)
    (with-output-to-file (output-path)
      (λ () (write full-results))
      #:mode 'text
      #:exists 'replace))

  ;; TODO: update lattice to account for empty-result startup time
  (when (lattice-path)
    (define averaged-results
      (vector-map (λ (times) (cons (mean times) (stddev times))) specific-results))
    (send ;; default size is too small to see, so apply a scaling factor
          (pict->bitmap (scale (make-performance-lattice averaged-results) 3))
          save-file
          (lattice-path)
          'png)))
