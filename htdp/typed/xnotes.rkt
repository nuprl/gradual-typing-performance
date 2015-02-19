#! /bin/sh
#|
exec /home/ben/code/racket/fork/racket/bin/racket -tm "$0" ${1+"$@"}
|#
#lang typed/racket

;; ---------------------------------------------------------------------------------------------------
;; runs scribble and opens preview for section, draft, release

(provide
 ;; -> Void
 ;; ./xnotes
 ;; renders the stable and the draft version of Notes
 notes:main)

;; ---------------------------------------------------------------------------------------------------
(require benchmark-util
         "../base/types.rkt")

(require/typed/check "x-info.rkt"
  [process-whole (->* (Boolean
                       (-> Boolean
                           String
                           Path-String
                           String
                           (-> (Class) (Class))
                           Void)
                       String
                       Path-String)
                      (Boolean)
                      Void)]
  [NOTES String]
  [DRAFT-DESTINATION Path-String]
  [DRAFT String]
  [HTDP2-DESTINATION Path-String]
  [draft-info-htdp String]
  [draft-info-note String]
  [info-htdp String]
  [info-note String]
  [run  (->* ((-> (Class) (Class))
             Path-String
             part
             Path-String
             (U String #f)
             Path-String)
            (#:info-out-file Path-String)
           Any)]
)

;; Uncomment if RENDER IN BROWSER is uncommented
;; (require/typed
;;  net/sendurl
;;  [send-url/file (-> Path-String Void)])

(: notes:main (->* () (Boolean) Void))
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
(: scribble-it (->* (Boolean
                     String
                     Path-String
                     (U String #f)
                     (-> (Class) (Class)))
                    (Boolean)
                    Void))
(define (scribble-it draft? stem destination redirect? renderer [unused-flag #f])
  ;; (: stem.scrbl Resolved-Module-Path)
  ;; (define stem.scrbl (make-resolved-module-path
  ;;                     (build-path stem ".scrbl")))
  (: stem.scrbl String)
  (define stem.scrbl 
                      (string-append "../base/" stem ".scrbl"))
  (define stem.html  (string-append "../base/" stem ".html"))
  (displayln `(rendering ,stem.scrbl draft: ,draft?))
  (define stem.doc (cast (dynamic-require stem.scrbl 'doc) part))
  (define-values (in-file out-file)
    (if draft?
        (values draft-info-htdp draft-info-note)
        (values info-htdp       info-note)))
  (unless (file-exists? in-file)
    (copy-file "../base/x-info.dat" in-file)
    (printf "WARNING: xnotes is using an old info file. RUN xnotes AGAIN"))
  (run renderer stem stem.doc  destination redirect? in-file #:info-out-file out-file)
  (displayln `(done rendering))
)  ;; --- RENDER IN BROWSER
  ;; (parameterize ([current-directory destination])
  ;;   (displayln `(opening browser at ,destination ,stem.html))
  ;;   (send-url/file (if (file-exists? stem.html) stem.html (build-path stem "index.html")))))
