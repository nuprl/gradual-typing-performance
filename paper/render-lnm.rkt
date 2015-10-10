#lang typed/racket/base

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
 (only-in "scripts/lnm-plot.rkt" lnm-plot)
 (only-in "scripts/summary.rkt" from-rktd summary->pict Summary)
 (only-in racket/file file->value)
 "scripts/pict-types.rkt"
)

(require/typed racket/serialize
  [serialize (-> Pict Any)]
  [deserialize (-> Any Pict)])

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

(: l-index->string (-> Integer String))
(define (l-index->string i)
  (cond [(zero? i)
         (format "L = ~a" i)]
        [(= PARAM-L i)
         (format "\t~a  (steps)" i)]
        [else
         (number->string i)]))
(define L*+title*
  (for/list : (Listof (Pairof Index String)) ([i (in-range (add1 PARAM-L))])
    (cons (cast i Index) (l-index->string i))))
(define L*
  (for/list : (Listof Index) ([x (in-list L*+title*)]) (car x)))
(define L-title*
  (for/list : (Listof String) ([x (in-list L*+title*)]) (cdr x)))

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
(: data->pict (->* [(Listof (List String String))] [#:tag String] Pict))
(define (data->pict data* #:tag [tag ""])
  (define title* (for/list : (Listof String) ([x (in-list data*)]) (car x)))
  (define rktd* (for/list : (Listof String) ([x (in-list data*)]) (cadr x)))
  (or (get-cached rktd* #:tag tag)
      (get-new-lnm-pict rktd* #:tag tag #:titles title*)))

;; Create a summary and L-N/M picts for a data file.
(: file->pict* (-> String #:title (U String #f) (Listof Pict)))
(define (file->pict* data-file #:title title)
  (define S (from-rktd data-file))
  (define S-pict : Pict
    (let ([p (summary->pict S
                   #:title title
                   #:font-face FONT-FACE
                   #:font-size TEXT-FONT-SIZE
                   #:N PARAM-N
                   #:M PARAM-M
                   #:width (* 0.6 W)
                   #:height H)])
      (vc-append 0 p (blank 0 (- H (pict-height p))))))
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

(: format-filepath (-> (U #f String) String))
(define (format-filepath tag)
  (unless (directory-exists? "./compiled") (make-directory "./compiled"))
  (string-append CACHE-PREFIX (or tag "") ".rktd"))

;; Save a pict, tagging with with `tag` and the `rktd*` filenames
(: cache-pict (-> Pict (Listof String) (U #f String) Void))
(define (cache-pict pict rktd* tag)
  (define filepath (format-filepath tag))
  (debug "Caching new pict at '~a'" filepath)
  (with-output-to-file filepath
    (lambda () (write (cons rktd* (serialize pict))))
    #:mode 'text
    #:exists 'replace))

(: get-cached (->* [(Listof String)] [#:tag String] (U Pict #f)))
(define (get-cached rktd* #:tag [tag ""])
  (define filepath (format-filepath tag))
  (and (file-exists? filepath)
       (read-cache rktd* filepath)))

(define-type CachedPict (Pairof (Listof String) Any))
(define-predicate cachedpict? CachedPict)

(: read-cache (-> (Listof String) String (U #f Pict)))
(define (read-cache rktd* filepath)
  (define tag+pict (file->value filepath))
  (cond
    [(cachedpict? tag+pict)
     (and (equal? rktd* (car tag+pict))
     (debug "Reading cached pict from '~a'" filepath)
     (deserialize (cdr tag+pict)))]
    [else
     (error 'render-lnm (format "Malformed data in cache file '~a'" filepath))]))

;; Create a pict, cache it for later use
(: get-new-lnm-pict (->* [(Listof String)] [#:tag String #:titles (U #f (Listof String))] Pict))
(define (get-new-lnm-pict rktd* #:tag [tag ""] #:titles [maybe-title* #f])
  (: title* (U (Listof String) (Listof #f)))
  (define title* (or maybe-title* (for/list : (Listof #f) ([x (in-list rktd*)]) #f)))
  ;; Align all picts vertically first
  (define columns : (Listof Pict)
    (or (for/fold : (U #f (Listof Pict))
              ([prev* : (U #f (Listof Pict)) #f])
              ([rktd (in-list rktd*)]
               [title : (U #f String) (in-list title*)])
      (define pict* (file->pict* rktd #:title title))
      (if prev*
          ;; Right-align the old picts with the new ones
          (for/list : (Listof Pict)
                    ([old (in-list prev*)]
                     [new (in-list pict*)])
            (vr-append GRAPH-VSPACE old new))
          ;; Generate titles. Be careful aligning the summary row
          (cons (car pict*)
           (for/list : (Listof Pict)
                     ([l-str (in-list L-title*)]
                      [new (in-list (cdr pict*))])
             (vc-append TITLE-VSPACE (text l-str TITLE-STYLE TITLE-SIZE) new)))))
         (error 'invariant)))
  ;; Paste the columns together, insert a little extra space to make up for
  ;;  the missing title in the first column
  (define pict : Pict
    (or (for/fold : (U #f Pict)
              ([prev-pict : (U #f Pict) #f])
              ([c columns])
      (if prev-pict
          (hc-append GRAPH-HSPACE prev-pict c)
          (vc-append TITLE-VSPACE (blank 0 10) c)))
        (error 'invariant)))
  (cache-pict pict rktd* tag)
  pict)

