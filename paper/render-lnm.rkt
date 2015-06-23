#lang racket/base

;; Specific tools for rendering L-N/M pictures in the current paper.

(provide
 rktd*->pict
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

(define L* (for/list ([i (in-range (add1 PARAM-L))]) i))
(define FONT-FACE "Liberation Serif")

;; TODO make sizes as large as possible
(define H 100)
(define W 130)
(define GRAPH-FONT-SIZE 7)
(define TEXT-FONT-SIZE 11)
(define GRAPH-HSPACE 30)
(define GRAPH-VSPACE 30)
(define TITLE-STYLE FONT-FACE)
(define TITLE-SIZE (+ 3 TEXT-FONT-SIZE))
(define TITLE-VSPACE (/ GRAPH-VSPACE 2))

(define CACHE-PREFIX "./compiled/lnm-cache-")

;; Put titles for the L-values above each pict
;; NOTE: works only for args with exactly 3 members
;; (-> (Listof Pict) (Listof Pict))
(define (add-titles pict*)
  (for/list ([p (in-list pict*)]
             [title (in-list '("L = 0" "1" "\t2  (steps)"))])
    (vc-append TITLE-VSPACE (text title TITLE-STYLE TITLE-SIZE) p)))

;; Attach a new pict, representing the summary object,
;;  to the previous pict.
;; If the previous is #f, append a title above this pict
;; (-> (U Pict #f) Path-String Pict)
(define (render-lnm-pict prev-pict data-file)
  (define S (from-rktd data-file))
  (define S-pict (summary->pict S
                                #:font-face FONT-FACE
                                #:font-size TEXT-FONT-SIZE
                                #:width W
                                #:height H))
  (define L-pict*
    (let ([pict*
           (lnm-plot S #:L L*
                     #:N PARAM-N
                     #:M PARAM-M
                     #:max-overhead PARAM-MAX-OVERHEAD
                     #:num-samples PARAM-NUM-SAMPLES
                     #:font-face FONT-FACE
                     #:font-size GRAPH-FONT-SIZE
                     #:labels? #f
                     #:plot-height H
                     #:plot-width W)])
      ;; Add L labels if there is no previous pict
      (if prev-pict pict* (add-titles pict*))))
  (define row
    (for/fold ([pict S-pict])
        ([L-pict (in-list L-pict*)])
      (hb-append GRAPH-HSPACE pict L-pict)))
  (if prev-pict (vc-append GRAPH-VSPACE prev-pict row) row))

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
(define (get-new-lnm-pict rktd* #:tag [tag ""])
  (debug "Generating new picture from data files ~a" rktd*)
  (define pict
    (for/fold ([prev-pict #f])
              ([data-file (in-list rktd*)])
      (render-lnm-pict prev-pict data-file)))
  (cache-pict pict rktd* tag)
  pict)

;; Try to read a cached pict, fall back to making a new one.
(define (rktd*->pict rktd* #:tag [tag ""])
  (or (get-cached rktd* #:tag tag)
      (get-new-lnm-pict rktd* #:tag tag)))
