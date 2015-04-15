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

(require
  racket/file
  racket/string
)

;; Get the base-2 log of a number
(define (log2 n)
  (log2-aux n 1))
(define (log2-aux n acc)
  (cond [(= n (expt 2 acc)) acc]
        [else (log2-aux n (add1 acc))]))

;; Convert a natural to a binary string, padded to the supplied width
(define (natural->binary n pad-width)
  (string-reverse (nat->bin-aux n (make-string pad-width #\0))))
(define (nat->bin-aux n str)
  (cond [(= n 0) str]
        [else (nat->bin-aux (sub1 n) (incr-bit str))]))

;; Increment a string representation of a binary number
(define (incr-bit str)
  (incr-bit-aux str 0)
  str)
(define (incr-bit-aux str i)
  (cond [(>= i (string-length str)) (void)] ;; Silently fail
        [(eq? #\0 (string-ref str i))
         (string-set! str i #\1)]
        [else
         (string-set! str i #\0)
         (incr-bit-aux str (add1 i))]))

;; Reverse a string
(define (string-reverse str)
  (list->string (reverse (string->list str))))

;; Read in-file as a vector, transpose data & print
(define (copy-data-transpose in-file out-file)
  (define vec (file->value in-file))
  (with-output-to-file out-file #:exists 'replace
    (lambda ()
      ;; First print titles
      (define num-configs (vector-length vec))
      (define configs (for/list ([n (in-range num-configs)]) (natural->binary n (log2 num-configs))))
      (displayln (string-join (cons "Run" configs) "\t"))
      ;; For each run
      (for ([n (in-range (length (vector-ref vec 0)))])
        ;; Get the n-th data value from each row
        (define vals (for/list ([row vec]) (format "~a" (list-ref row n))))
        ;; Pre-pend run number and print
        (displayln (string-join (cons (number->string n) vals) "\t"))))))

(define (copy-data in-file out-file)
  (define vec (file->value in-file))
  (with-output-to-file out-file #:exists 'replace
    (lambda ()
      ;; First print index
      (define num-configs (vector-length vec))
      (define num-runs (length (vector-ref vec 0)))
      (displayln (string-join (cons "Run" (for/list ([n (in-range num-runs)]) (format "~a" (add1 n)))) "\t"))
      ;; Start counter for configurations
      (define N (make-string (log2 num-configs) #\0))
      ;; For each row, print the config ID and all the values
      (for ([row vec])
        (displayln (string-join (cons (string-reverse N) (for/list ([v row]) (format "~a" v))) "\t"))
        (incr-bit N)))))
    
;; Replace the characters after the "." in the first arguemnt with the second
(define (replace-extension str ext)
  (string-append (car (string-split str ".")) "." ext))

;; Parse command-line argument
(define (main args)
  (cond [(= 1 (vector-length args))
         (define in-file (vector-ref args 0))
         (define out-file (replace-extension in-file "tab"))
         (copy-data in-file out-file)]
        [else (displayln "Usage: sexp-to-tab.rkt FILENAME")]))

(main (current-command-line-arguments))

