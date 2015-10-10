#lang racket

(require
  pict
  (only-in racket/string string-join)
  "../paper/render-lnm.rkt")

(define OUTPUT "benchmarks/output.png")

(define (filename->tag fname)
  (first (string-split (last (string-split fname "/")) ".")))

(define (filter-valid-filenames arg*)
  (for/list ([fname (in-list arg*)]
             #:when (valid-filename? fname))
    fname))

(define (valid-filename? fname)
  (cond
   [(and (file-exists? fname)
         (regexp-match? #rx"\\.rktd$" fname))
    #t]
   [else
    (printf "Skipping invalid file '~a'\n" fname)
    #f]))

(module+ main
  (require racket/cmdline)
  (define *output* (make-parameter "./benchmarks/output.png"))
  (command-line
   #:program "view-pict"
   #:once-each
   [("-o" "--output") o-param
    "Location to save results" (*output* o-param)]
   #:args FNAME*
   ;; -- Filter valid arguments, assert that we got anything to render
   (define arg* (filter-valid-filenames FNAME*))
   (when (null? arg*)
     (raise-user-error "Usage: view.rkt DATA.rktd ..."))
   ;; -- Create a pict
   (define BM
     (pict->bitmap
       (data->pict #:tag (string-join (map filename->tag arg*) "-")
         (for/list ([fname (in-list arg*)])
           (list (filename->tag fname) fname)))))
   ;; -- Show the pict
   (send BM save-file (*output*) 'png 100)))
