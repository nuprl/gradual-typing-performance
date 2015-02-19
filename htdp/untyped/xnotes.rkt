#lang racket/base

;; ---------------------------------------------------------------------------------------------------
;; runs scribble and opens preview for section, draft, release

(provide
 ;; -> Void
 ;; ./xnotes
 ;; renders the stable and the draft version of Notes
 notes:main)

;; ---------------------------------------------------------------------------------------------------
(require "x-info.rkt" net/sendurl)

(define (notes:main (draft? #f))
  (if draft?
      (process-whole #t scribble-it NOTES (build-path DRAFT-DESTINATION DRAFT))
      (process-whole #f scribble-it NOTES (build-path HTDP2-DESTINATION NOTES))))

;; Boolean String String [Maybe String] { [Class -> Class] } -> Void 
;; produce destination/stem.html by
;; -- initialize the is-draft parameter in shared.ss with the draft? flag 
;; -- rendering stem.scrbl
;; -- using the redirect? url as source of documentation
;; -- using renderer, which implements render<%>, to scribble 
;; produce [draft-]info-file for cross-references to HtDP
;; open browser on stem.html 
(define (scribble-it draft? stem destination redirect? renderer)
  (define stem.scrbl (string-append stem ".scrbl"))
  (define stem.html  (string-append stem ".html"))
  (displayln `(rendering ,stem.scrbl draft: ,draft?))
  (define stem.doc (dynamic-require stem.scrbl 'doc))
  (define-values (in-file out-file)
    (if draft?
        (values draft-info-htdp draft-info-note)
        (values info-htdp       info-note)))
  (unless (file-exists? in-file)
    (copy-file "../base/x-info.dat" in-file)
    (printf "WARNING: xnotes is using an old info file. RUN xnotes AGAIN"))
  (run renderer stem stem.doc  destination redirect? in-file #:info-out-file out-file)
  (displayln `(done rendering))
  );; --- Uncomment to render HTML
  ;;(parameterize ([current-directory destination])
  ;;  (displayln `(opening browser at ,destination ,stem.html))
  ;;  (send-url/file (if (file-exists? stem.html) stem.html (build-path stem "index.html"))))
