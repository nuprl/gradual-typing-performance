#lang racket/base

;; Convert a list of .rktd data files into one large spreadsheet.
;; INPUT:
;;  List of 'output.rktd' from running 'run.rktd'
;;  Optional argument `--title` changes name of written file from `spreadsheet.tab`.
;; OUTPUT:
;;  A spreadsheet, with format
;;     RUN 0 1 2 <index> ...
;;     CONFIG RUNTIME ...
;;     CONFIG RUNTIME ...
;;     ...
;;  i.e. each row except the first is the configuration's ID followed by
;;       the exact runtimes.
;;  The first row just communicates how many runtimes each configuration has.

;; The raw .rktd data files are S-expressions representing a 2-D matrix.
;; - The rows are a vector with 2^n entries, one for each configuration.
;;   For example, the 0010 configuration is at entry 2 in this vector.
;; - Each entry of the rows vector is a LIST of absolute runtimes.
;; Also, the data all lives on a single line.

;; The run.rkt script should really output more meaningful data.
;; This script FAILS SILENTLY if the data is not input in the right order.
;; indices are assigned as the rows come in. Incorrect order = Bad Index

;; ----------------------------------------------------------------------------

(require racket/file
         racket/string
         racket/format
         racket/math)

;; =============================================================================

(define DEFAULT_TITLE "spreadsheet.tab")
(define title-param (box DEFAULT_TITLE))
(define num-runs-param (box #f))
(define orig-file-param (box #f))
(define offset-param (box 0))
(define num-configs-param (box #f))

;; -----------------------------------------------------------------------------

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

;; Count the total number of configurations, out of all files.
;; TODO reads each file as a vector. Not sure there's a better way,
;; but I definitely do NOT want to store all the vectors in memory simultaneously.
(define (count-num-configs filename*)
  (for/fold ([total 0])
            ([filename (in-list filename*)])
    (+ total (vector-length (file->value filename)))))

;; Convert a single .rktd file to a string.
;; Send results along stdout
(define (copy-file in-file)
  (define vec (file->value in-file))
  ;; First print index
  (define num-runs (length (vector-ref vec 0)))
  (if (not (unbox num-runs-param))
      ;; First file, print row headings
      ;; Vector does not contain header, so need to add that explicitly
      (begin (set-box! num-runs-param num-runs)
             (set-box! orig-file-param in-file)
             (display "Run")
             (for ([n num-runs])
               (printf "\t~a" (add1 n)))
             (newline))
      ;; Not the first file, just check that `num-runs` is the same
      (or (= num-runs (unbox num-runs-param))
          (error "File '~a' has different number of columns than first file '~a'" in-file (unbox orig-file-param))))
  (define offset (unbox offset-param))
  (set-box! offset-param (+ offset (vector-length vec)))
  ;; For each row, print the config ID and all the values
  (for ([(row n) (in-indexed vec)])
    (display (natural->binary (+ offset n) (log2 (unbox num-configs-param))))
    (for ([v row]) (printf "\t~a" v))
    (newline)))

;; Copy a bunch of files to a single output.
(define (copy-file* filename* #:title out-filename)
  (set-box! num-configs-param (count-num-configs filename*))
  (with-output-to-file out-filename #:exists 'replace
    (lambda ()
      (for ([filename (in-list filename*)])
        (copy-file filename)))))

(module+ main
  (require racket/cmdline)
  (command-line #:once-each
                [("-t" "--title") title "Set title of output spreadsheet."
                                  (set-box! title-param title)]
                #:args filename*
                (copy-file* filename* #:title (unbox title-param))))
