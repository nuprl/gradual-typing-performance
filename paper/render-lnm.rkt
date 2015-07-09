#lang racket/base

;; Specific tools for rendering L-N/M pictures in the current paper.

(provide
 data->pict
 PARAM-N
 PARAM-M
 PARAM-L
 PARAM-MAX-OVERHEAD
 PARAM-NUM-SAMPLES
)

;; -----------------------------------------------------------------------------

(require
 pict
 (only-in "scripts/lnm-plot.rkt" lnm-plot)
 (only-in "scripts/summary.rkt" from-rktd summary->pict)
 (only-in racket/file file->value)
 racket/serialize
 )

;; =============================================================================
;; --- constants

(define DEBUG #t)
(define-syntax-rule (debug msg arg* ...)
  (when DEBUG (printf msg arg* ...) (newline)))

;; Experiment parameters
(define PARAM-N 3)
(define PARAM-M 10)
(define PARAM-L 2)
(define PARAM-MAX-OVERHEAD 20)
(define PARAM-NUM-SAMPLES 60)

(define (l-index->string i)
  (cond [(zero? i)
         (format "L = ~a" i)]
        [(= PARAM-L i)
         (format "\t~a  (steps)" i)]
        [else
         (number->string i)]))
(define L*+title*
  (for/list ([i (in-range (add1 PARAM-L))])
    (cons i (l-index->string i))))
(define L* (map car L*+title*))
(define L-title* (map cdr L*+title*))

(define FONT-FACE "Liberation Serif")

(define H 100)
(define W 130)
(define GRAPH-FONT-SIZE 6)
(define TEXT-FONT-SIZE 10)
(define GRAPH-HSPACE 10)
(define GRAPH-VSPACE 20)
(define TITLE-STYLE FONT-FACE)
(define TITLE-SIZE (+ 2 TEXT-FONT-SIZE))
(define TITLE-VSPACE (/ GRAPH-VSPACE 2))

(define CACHE-PREFIX "./compiled/lnm-cache-")

;; Try to read a cached pict, fall back to making a new one.
(define (data->pict data* #:tag [tag ""])
  (define title* (map car data*))
  (define rktd* (map cadr data*))
  (or (get-cached rktd* #:tag tag)
      (get-new-lnm-pict rktd* #:tag tag #:titles title*)))

;; Create a summary and L-N/M picts for a data file.
;; (-> Path-String (Listof Pict))
(define (file->pict* data-file #:title title)
  (define S (from-rktd data-file))
  (define S-pict
    (let ([p (summary->pict S
                   #:title title
                   #:font-face FONT-FACE
                   #:font-size TEXT-FONT-SIZE
                   #:N PARAM-N
                   #:M PARAM-M
                   #:width (* 0.6 W)
                   #:height H)])
      (vc-append p (blank 0 (- H (pict-height p))))))
  (define L-pict*
    (lnm-plot S
              #:L L*
              #:N PARAM-N
              #:M PARAM-M
              #:max-overhead PARAM-MAX-OVERHEAD
              #:num-samples PARAM-NUM-SAMPLES
              #:font-face FONT-FACE
              #:font-size GRAPH-FONT-SIZE
              #:labels? #f
              #:plot-height H
              #:plot-width W)) ;;TODO adjust, to keep figure sizes all equal?
  (cons S-pict L-pict*))

(define (format-filepath tag)
  (string-append CACHE-PREFIX (or tag "") ".rktd"))

;; Save a pict, tagging with with `tag` and the `rktd*` filenames
(define (cache-pict pict rktd* tag)
  (define filepath (format-filepath tag))
  (debug "Caching new pict at '~a'" filepath)
  (with-output-to-file filepath
    (lambda () (write (cons rktd* (serialize pict))))
    #:mode 'text
    #:exists 'replace))

(define (get-cached rktd* #:tag [tag ""])
  (define filepath (format-filepath tag))
  (and (file-exists? filepath)
       (read-cache rktd* filepath)))

(define (read-cache rktd* filepath)
  (define tag+pict (file->value filepath))
  (unless (and (pair? tag+pict)
               (list? (car tag+pict)))
    (error 'render-lmn (format "Malformed data in cache file '~a'" filepath)))
  (and (equal? rktd* (car tag+pict))
       (debug "Reading cached pict from '~a'" filepath)
       (deserialize (cdr tag+pict))))

;; Create a pict, cache it for later use
(define (get-new-lnm-pict rktd* #:tag [tag ""] #:titles [maybe-title* #f])
  (define title* (or maybe-title* (for/list ([x (in-list rktd*)]) #f)))
  ;; Align all picts vertically first
  (define columns
    (for/fold ([prev* #f])
              ([rktd (in-list rktd*)]
               [title (in-list title*)])
      (define pict* (file->pict* rktd #:title title))
      (if prev*
          ;; Right-align the old picts with the new ones
          (for/list ([old (in-list prev*)]
                     [new (in-list pict*)])
            (vr-append GRAPH-VSPACE old new))
          ;; Generate titles. Be careful aligning the summary row
          (cons (car pict*)
          (for/list ([l-str (in-list L-title*)]
                     [new (in-list (cdr pict*))])
            (vc-append TITLE-VSPACE (text l-str TITLE-STYLE TITLE-SIZE) new))))))
  ;; Paste the columns together, insert a little extra space to make up for
  ;;  the missing title in the first column
  (define pict
    (for/fold ([prev-pict #f])
              ([c columns])
      (if prev-pict
          (hc-append GRAPH-HSPACE prev-pict c)
          (vc-append TITLE-VSPACE (blank 0 10) c))))
  (cache-pict pict rktd* tag)
  pict)


