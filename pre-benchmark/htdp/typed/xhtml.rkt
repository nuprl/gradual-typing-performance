#lang typed/racket

;; ---------------------------------------------------------------------------------------------------
;; runs scribble and opens preview for section, draft, release

(provide
 ;; String -> Void
 ;; ./xhtml [release | draft | file[.scrbl]]
 ;; renders the stable version of HtDP2e, its draft version, or just the specified part or chapter 
 ;; when a plain file name f is given, xhtml looks for f.scrbl 
 main)

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
  [HTDP2 String]
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
           Any)])

(require/typed/check "xnotes.rkt"
  [notes:main (->* () (Boolean) Void)])

;; Uncomment if RENDER IN BROWSER snippet is uncommented, below
;; (require/typed
;;  net/sendurl
;;  [send-url/file (-> Path-String Void)])

(require/typed
 scribble/html-render
 [render-mixin (-> (Class) (Class))])

(: main (-> String Void))
(define (main arg)
  (cond 
    [(string=? "release" arg) 
     (process-whole #f scribble-it HTDP2 HTDP2-DESTINATION #t)]
    [(string=? "draft" arg)
     (create-draft-file)
     (process-whole #t scribble-it DRAFT DRAFT-DESTINATION #t)]
    [(file-exists? (string-append "../base/" arg ".scrbl"))
     (process arg)]
    [(and (file-exists? arg) (regexp-match "(.*)\\.scrbl" arg))
     (error "ben could not type this")] ;; TODO: though, the case appears unused
     ;; (compose process second)]
    [else
     (error 'xhtml "no such file: ~a.scrbl" arg)]))

;; -> Void
(: create-draft-file (-> Void))
(define (create-draft-file)
  (when (file-exists? "../base/Draft.scrbl") (delete-file "../base/Draft.scrbl"))
  (copy-file "../base/HtDP2e.scrbl" "../base/Draft.scrbl"))

;; String -> Void
;; create a destination directory, then scribble the desired document
(: process (-> String Void))
(define (process stem)
  (define destination (string-append "../base/HTML/" stem))
  (unless (directory-exists? destination) (make-directory destination))
  (scribble-it #f stem destination #f))

;; Boolean String String [Maybe String] {[Class -> Class]} {Boolean} -> Void 
;; produce destination/stem.html by
;; -- initialize the is-draft parameter in shared.ss with the draft? flag 
;; -- rendering stem.scrbl
;; -- using the redirect? url as source of documentation
;; -- using renderer, which implements render<%>, to scribble 
;; produce [draft-]info-file for cross-references to HtDP
;; -- but only if produce-info? calls for it (DON'T DO IT FOR CHAPTERS and INTERMEZZOS)
;; open browser on stem.html
(: scribble-it (->* (Boolean
                     String
                     Path-String
                     (U String #f))
                    ((-> (Class) (Class))
                     Boolean)
                    Void))
(define (scribble-it draft? stem destination redirect? (renderer render-mixin) (produce-info? #f))
  (define stem.scrbl (string-append "../base/" stem ".scrbl"))
  (define stem.html  (string-append "../base/" stem ".html"))
  ((cast (dynamic-require "../base/Shared/shared.rkt" 'is-draft?) (Parameterof Boolean)) draft?)
  (: stem.doc part)
  (define stem.doc (cast (dynamic-require stem.scrbl 'doc) part))
  (define-values (in-file out-file)
    (if draft?
        (values draft-info-note draft-info-htdp)
        (values info-note       info-htdp)))
  (unless (file-exists? in-file) (notes:main draft?))
  (displayln `(rendering ,stem.scrbl))
  (if produce-info?
      (run renderer stem stem.doc  destination redirect? in-file #:info-out-file out-file)
      (run renderer stem stem.doc  destination redirect? in-file))
  (displayln `(done rendering))
)  ;; --- RENDER IN BROWSER
  ;; (parameterize ([current-directory destination])
  ;;   (displayln `(cleaning up ,stem))
  ;;   (cleanup)
  ;;   (displayln `(opening browser at ,destination ,stem.html))
  ;;   (send-url/file (if (file-exists? stem.html) stem.html (build-path stem "index.html")))))

;; ---------------------------------------------------------------------------------------------------
;; the code below used to be a standalone script 

;; -> Void
(: cleanup (-> Void))
(define (cleanup)
  (for ((f (directory-list)) #:when (regexp-match ".html$" (path->string f)))
    (fix-1-file (path->string f))))

;; String -> Void 
;; given the file name ... 
;; [i know i can do this with regexp-replace ...]
(: fix-1-file (-> String Void))
(define (fix-1-file file-name)
  (: change-all (-> Void))
  (define (change-all)
    (: loop (-> Void))
    (define (loop)
      (: next (U EOF Char))
      (define next (read-char))
      (unless (eof-object? next) 
        (cond
          [(char=? next #\-)
           (let ([next2 (read-char)])
             (if (and (not (eof-object? next2))
                      (char=? #\1 next2))
                 (let ([next3 (read-char)])
                   (if (and (not (eof-object? next3))
                            (char=? #\. next3))
                       (let ([next4 (read-char)])
                         (if (and (not (eof-object? next4))
                                  (char=? #\0 next4))
                             (display "#i-1.0")
                             (for-each display (list #\- #\1 #\. next4))))
                       (for-each display (list #\- #\1 next3))))
                 (for-each display (list #\- next2))))]
          [else (display next)])
      (loop))
    )
    ;; -----------------------------------------------------------------------------------------------
    (with-handlers ([exn:fail? (lambda ([e : exn]) (displayln `(failing in change-all ,(exn-message e))))])
      (with-output-to-file "tmp.html" loop #:exists 'truncate)))
  (with-input-from-file file-name change-all)
  (rename-file-or-directory "tmp.html" file-name #t))
