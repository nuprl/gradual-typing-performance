#lang typed/racket

;; ---------------------------------------------------------------------------------------------------
;; runs scribble and opens preview for section, draft, release

(provide
 ;; -> Void
 ;; ./xnotes
 ;; renders the stable and the draft version of Notes
 main)

;; ---------------------------------------------------------------------------------------------------
(require
; net/sendurl
 "x-info.rkt")

(define (main (draft? #f))
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
(: scribble-it (-> Boolean
                   String
                   Path-String
                   String
                   (-> Any Any)
                   Void))
(define (scribble-it draft? stem destination redirect? renderer)
  ;; (: stem.scrbl Resolved-Module-Path)
  ;; (define stem.scrbl (make-resolved-module-path
  ;;                     (build-path stem ".scrbl")))
  (: stem.scrbl String)
  (define stem.scrbl 
                      (string-append stem ".scrbl"))
  (define stem.html  (string-append stem ".html"))
  (displayln `(rendering ,stem.scrbl draft: ,draft?))
  (: stem.doc AnyValues) ; ?????
  (define stem.doc (dynamic-require stem.scrbl 'doc))
    ;; (let ([x (dynamic-require stem.scrbl 'doc)])
    ;;   (if (eq? #f x)
    ;;       (error "false")
    ;;       x)))
  ;; (define-values (in-file out-file)
  ;;   (if draft?
  ;;       (values draft-info-htdp draft-info-note)
  ;;       (values info-htdp       info-note)))
  ;; (unless (file-exists? in-file)
  ;;   (copy-file "x-info.dat" in-file)
  ;;   (printf "WARNING: xnotes is using an old info file. RUN xnotes AGAIN"))
  ;; ;; (printf "CALLING RENDER WITH\ndestination = ~a\nin-file = ~a\nout-file = ~a\n" destination in-file out-file)
  ;; ;; (printf "is path? dest = ~a\n" (path-for-some-system? destination))
  ;; (run renderer stem stem.doc  destination redirect? in-file #:info-out-file out-file)
  (displayln `(done rendering))
)  ;; ---
  ;; (parameterize ([current-directory destination])
  ;;   (displayln `(opening browser at ,destination ,stem.html))
  ;;   (send-url/file (if (file-exists? stem.html) stem.html (build-path stem "index.html")))))
