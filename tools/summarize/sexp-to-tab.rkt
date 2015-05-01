#lang racket/base

;; The raw .rktd data files are S-expressions representing a 2-D matrix.
;; - The rows are a vector with 2^n entries, one for each configuration.
;;   For example, the 0010 configuration is at entry 2 in this vector.
;; - Each entry of the rows vector is a LIST of absolute runtimes.
;; Also, the data all lives on a single line.
;;
;; This script converts those S-expressions into a spreadsheet.
;; Most titles of the sheet are the configuration bitstrings i.e. 000, 001, ...
;; The exception is the leftmost one, which is an index (the run number).
;; Data are just the runtimes for each run.

;; ----------------------------------------------------------------------------

(require racket/file
         racket/string
         racket/format
         racket/math)

(define (log2 n) (exact-ceiling (/ (log n) (log 2))))

;; Convert a natural to a binary string, padded to the supplied width
(define (natural->binary n pad-width) (~r n #:base 2 #:min-width pad-width #:pad-string "0"))

;; Read in-file as a vector, transpose data & print
(define (copy-data-transpose in-file out-file)
  (define vec (file->value in-file))
  (with-output-to-file out-file #:exists 'replace
    (lambda ()
      ;; First print titles
      (define num-configs (vector-length vec))
      (define len (log2 num-configs))
      (display "Run")
      (for ([n num-configs])
        (printf "\t~a" (natural->binary n len)))
      (newline)
      ;; For each run
      (for ([n (length (vector-ref vec 0))])
        (display n)
        ;; Get the n-th data value from each row
        (for ([row vec]) (printf "\t~a" (list-ref row n)))
        (newline)))))

(define (copy-data in-file out-file)
  (define vec (file->value in-file))
  (with-output-to-file out-file #:exists 'replace
    (lambda ()
      ;; First print index
      (define num-configs (vector-length vec))
      (define num-runs (length (vector-ref vec 0)))
      (display "Run")
      (for ([n num-runs])
        (printf "\t~a" (add1 n)))
      (newline)
      ;; For each row, print the config ID and all the values
      (for ([(row n) (in-indexed vec)])
        (display (natural->binary n (log2 num-configs)))
        (for ([v row]) (printf "\t~a" v))
        (newline)))))

(module+ main 
  (require racket/cmdline)
  (command-line #:args (filename)
                (copy-data filename (path-replace-suffix filename ".tab"))))

