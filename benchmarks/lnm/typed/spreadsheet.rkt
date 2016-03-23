#lang typed/racket/base

;; Convert a dataset to a spreadsheet.

;; Printed spreadsheets have:
;; - A title row, counting the number of runs for each variation
;; - A data row for each variation.
;;   The leftmost column is the configuration's bitstring, the rest of the
;;   columns are experimental results.

;; ----------------------------------------------------------------------------

(require
  benchmark-util
  (only-in racket/file file->value)
)
(require/typed/check "bitstring.rkt"
  [log2 (-> Index Index)]
  [natural->bitstring (-> Index #:pad Index String)])

(safe-and-unsafe-provide
  ;; Convert a data file containing a vector to a spreadsheet
  ;; Vector must follow the format specified in the `data/` directory
  ;; (->* (Path-String) (#:output (U Path-String #f) #:format Symbol) String)
  rktd->spreadsheet
)
;; =============================================================================

(define-syntax-rule (spreadsheet-error sym)
  (error 'from-rktd (format "Unknown spreadsheet format '~a'" sym)))

;; Convert a symbol representing a spreadsheet format into a file
;; extension for the spreadsheet.
(: symbol->extension (-> Symbol String))
(define (symbol->extension sym)
  (case sym
    [(csv tab) (string-append "." (symbol->string sym))]
    [else (spreadsheet-error sym)]))

;; Convert a symbol to a spreadsheet column separator.
(: symbol->separator (-> Symbol String))
(define (symbol->separator sym)
  (case sym
    [(csv) ","]
    [(tab) "\t"]
    [else (spreadsheet-error sym)]))

;; -----------------------------------------------------------------------------

;; (vector->spreadsheet rktd-vector out-file sep)
;; Copy the data from `rktd-vector` to the file `out-file`.
;; Format the data to a human-readable spreadsheet using `sep` to separate rows
(: vector->spreadsheet (-> (Vectorof (Listof Index)) Path-String String Void))
(define (vector->spreadsheet vec out-file sep)
  (with-output-to-file out-file #:exists 'replace
    (lambda ()
      ;; First print the index
      (: num-configs Index)
      (define num-configs (vector-length vec))
      (define num-runs (length (vector-ref vec 0)))
      (display "Run")
      (for ([n num-runs])
        (printf "~a~a" sep (add1 n)))
      (newline)
      ;; For each row, print the config ID and all the values
      (for ([(row n) (in-indexed vec)])
        (display (natural->bitstring (cast n Index) #:pad (log2 num-configs)))
        (for ([v row]) (printf "~a~a" sep v))
        (newline)))))

;; Print the rktd data stored in file `input-filename` to a spreadsheet.
(: rktd->spreadsheet (->* (Path-String) (#:output (U Path-String #f) #:format Symbol) Void))
(define (rktd->spreadsheet input-filename
                             #:output [output #f]
                             #:format [format 'tab])
  (define vec (cast (file->value input-filename) (Vectorof (Listof Index))))
  (define suffix (symbol->extension format))
  (define out (or output (path-replace-suffix input-filename suffix)))
  (define sep (symbol->separator format))
  (vector->spreadsheet vec out sep))
