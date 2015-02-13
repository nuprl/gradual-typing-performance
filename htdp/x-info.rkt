#lang typed/racket/base

(provide
 ;; Boolean [Boolean String String [Maybe String] [Class -> Class] . Any -> Void] String String -> Any
 process-whole 

 ;; [ -> ] PathString PathString PathString [Maybe PathString] PathString X ... -> Any
 ;; syntax
 run

 ;; String
 NOTES HTDP2 DRAFT HTDP2-DESTINATION DRAFT-DESTINATION

 ;; PathString:
 ;; where is cross-referencing information for htdp2e and notes stored to/retrieved from
 info-htdp draft-info-htdp info-note draft-info-note
 ;; typed identifiers
 part Render)

;; The documents end up in: 
;; ROOT
;; -- HtDP2e/Notes ## notes for the stable version of HtDP/2e  [UNTESTED]
;; -- HtDP2e       ## the stable version of HtDP/2e
;; -- HtDP2e/Notes ## notes for the draft version of HtDP/2e 
;; -- HtDP2e/Draft ## the draft version of HtDP/2e

;; ---------------------------------------------------------------------------------------------------
(require/typed
 scribble/core
 [#:opaque Tag tag?]
 [#:opaque Block block?]
 [#:struct part ([tag-prefix : (U #f String)]
                 [tags : (Listof Tag)]
                 [title-content : (U #f (Listof Any))]
                 [style : Any] ;; "A style property can be anything, including a symbol or a structure such as color-property." http://docs.racket-lang.org/scribble/core.html?q=scribble%2Frender#%28tech._style._property%29
                 [to-collect : (Listof Any)]
                 [blocks : (Listof Block)]
                 [parts : (Listof part)])])

(require/typed
 scribble/xref
 [#:opaque Xref xref?])

(define-type Render (Class)) ;; TODO actually give types to the interface

(require/typed
 scribble/render
 ;; bg: Only typing the optional args I'm using
 [render (-> (Listof part)
             (Listof Path-String)
             [#:render-mixin (-> Render Render)]
             [#:dest-dir (U #f Path-String)]
             [#:xrefs (Listof Xref)]
             [#:quiet? Any]
             [#:redirect-main (U #f String)]
             [#:info-in-files (Listof Path-String)]
             [#:info-out-file (U #f Path-String)]
             Any)])

(require/typed
 scribble/html-render
 [render-mixin (-> Render Render)]
 [render-multi-mixin (-> Render Render)])

(require/typed
 setup/xref
 [load-collections-xref (->* () ((-> Any)) Xref)])

(define ROOT "./Trash")

(define NOTES "notes")
(define HTDP2 "HtDP2e")
(define DRAFT "Draft")

(define HTDP2-DESTINATION (build-path ROOT))
(define DRAFT-DESTINATION (build-path ROOT "HtDP2e"))

(define info-fmt "info-~a~a.rktl")

(define info-htdp (format info-fmt HTDP2 ""))
(define info-note (format info-fmt NOTES ""))
(define draft-info-htdp (format info-fmt HTDP2 DRAFT))
(define draft-info-note (format info-fmt NOTES DRAFT))

;; create a renderer and a path for the documentation, then scribble the desired document
;; bg: hacked the apply
(: process-whole (->* (Boolean
                       (-> Boolean
                           String
                           Path-String
                           String
                           (-> Render Render)
                           Void)
                       String
                       Path-String)
                      (Boolean)
                      Void))
(define (process-whole draft? scribble-it stem destination [maybe-flag #f])
  (define redirect 
    (if draft?
        "http://plt.eecs.northwestern.edu/snapshots/current/doc/"
        "http://docs.racket-lang.org/"))
  (define renderer (compose render-multi-mixin render-mixin))
  (scribble-it draft? stem destination redirect renderer ));maybe-flag))
  ;; (apply scribble-it draft? stem destination redirect renderer stuff))


;; run renderer on the remaining arguments with keywords supplied 
;; it's a syntax rule because I don't know how to supply an optional keyword otherwise 
;; (without running a decision again and thus duplicating the whole thing)
;; TODO bg: passing optional argument as false, if missing. But that syntax-rule trick was fun.
;; (define-syntax-rule (run renderer stem stem.doc destination redirect? in-file out-file ...)
(: run (->* ((-> Render Render)
             Path-String
             part
             Path-String
             (U String #f)
             Path-String)
            (#:info-out-file Path-String)
           Any))
(define (run renderer stem stem.doc destination redirect? in-file #:info-out-file [out-file #f])
  (render (list stem.doc)
          (list stem)
          #:render-mixin renderer
          #:dest-dir destination
          #:xrefs (list (load-collections-xref))
          #:quiet? #false
          #:redirect-main redirect?
          #:info-in-files (list in-file)
          #:info-out-file out-file))
