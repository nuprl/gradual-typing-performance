#lang racket

(require
  pict
  "../paper/render-lnm.rkt")

(define OUTPUT "benchmarks/output.png")

(define (valid-filename? fname)
  (cond
   [(and (file-exists? fname)
         (regexp-match? #rx"\\.rktd$" fname))
    #t]
   [else
    (printf "Skipping invalid file '~a'\n" fname)
    #f]))

(define (filter-valid-filenames arg*)
  (for/list ([fname (in-list arg*)]
             #:when (valid-filename? fname))
    fname))

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
       (data->pict #:tag "sample"
         (for/list ([fname (in-list arg*)])
           (list (last (string-split fname "/")) fname)))))
   ;; -- Show the pict
   (send BM save-file (*output*) 'png 100)
   (printf "Saved output to '~a'\n" (*output*))))
