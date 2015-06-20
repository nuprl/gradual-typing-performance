#lang racket/base

;; Convert a dataset to a spreadsheet.

;; Printed spreadsheets have:
;; - A title row, counting the number of runs for each variation
;; - A data row for each variation.
;;   The leftmost column is the configuration's bitstring, the rest of the
;;   columns are experimental results.

(provide
  ;; Convert a vector of experimental data.
  ;; Vector must follow the format specified in the `data/` directory
  ;; (->* (Path-String) (#:output (U Path-String #f) #:format Symbol) String)
  vector->spreadsheet
)
;; ----------------------------------------------------------------------------

(require racket/file
         racket/string
         racket/format
         racket/math)

;; =============================================================================

(define-syntax-rule (spreadsheet-error sym)
  (error 'from-rktd (format "Unknown spreadsheet format '~a'" sym)))

;; Convert a symbol representing a spreadsheet format into a file
;; extension for the spreadsheet.
;; (: symbol->extension (-> Symbol String))
(define (symbol->extension sym)
  (case sym
    [(csv tab) (string-append "." (symbol->string sym))]
    [else (spreadsheet-error sym)]))

;; Convert a symbol to a spreadsheet column separator.
;; (: symbol->separator (-> Symbol String))
(define (symbol->separator sym)
  (case sym
    [(csv) ","]
    [(tab) "\t"]
    [else (spreadsheet-error sym)]))

;; log, base 2
;; (: log2 (-> Integer Flonum))
(define (log2 n)
  (exact-ceiling (/ (log n) (log 2))))

;; Convert a natural number to a binary string, padded to the supplied width
;; (: natural->binary (-> Index Index String))
(define (natural->binary n pad-width)
  (~r n #:base 2 #:min-width pad-width #:pad-string "0"))

;; -----------------------------------------------------------------------------

;; (copy-data rktd-vector out-file sep)
;; Copy the data from `rktd-vector` to the file `out-file`.
;; Format the data to a human-readable spreadsheet using `sep` to separate rows
;; (: copy-data (-> (Vectorof (Vectorof Index)) Path-String String Void))
(define (copy-data vec out-file sep)
  (with-output-to-file out-file #:exists 'replace
    (lambda ()
      ;; First print the index
      (define num-configs (vector-length vec))
      (define num-runs (length (vector-ref vec 0)))
      (display "Run")
      (for ([n num-runs])
        (printf "~a~a" sep (add1 n)))
      (newline)
      ;; For each row, print the config ID and all the values
      (for ([(row n) (in-indexed vec)])
        (display (natural->binary n (log2 num-configs)))
        (for ([v row]) (printf "~a~a" sep v))
        (newline)))))

;; Print the rktd data stored in file `input-filename` to a spreadsheet.
;; (: vector->spreadsheet (->* (Path-String) (#:output (U Path-String #f) #:format Symbol) String))
(define (vector->spreadsheet input-filename
                             #:output [output #f]
                             #:format [format 'tab])
  (define vec (file->value input-filename))
  (define suffix (symbol->extension format))
  (define out (or output (path-replace-suffix input-filename suffix)))
  (define sep (symbol->separator format))
  (copy-data vec out sep))

;; -----------------------------------------------------------------------------

(module+ main
  (require racket/cmdline)
  (command-line #:args (filename)
    (vector->spreadsheet filename))
)
