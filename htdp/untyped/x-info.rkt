#lang racket/base

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
 info-htdp draft-info-htdp info-note draft-info-note)

;; The documents end up in: 
;; ROOT
;; -- HtDP2e/Notes ## notes for the stable version of HtDP/2e  [UNTESTED]
;; -- HtDP2e       ## the stable version of HtDP/2e
;; -- HtDP2e/Notes ## notes for the draft version of HtDP/2e 
;; -- HtDP2e/Draft ## the draft version of HtDP/2e

;; ---------------------------------------------------------------------------------------------------
(require scribble/render scribble/html-render setup/xref)

(define ROOT "../base/Trash")

(define NOTES "notes")
(define HTDP2 "HtDP2e")
(define DRAFT "Draft")

(define HTDP2-DESTINATION (build-path ROOT))
(define DRAFT-DESTINATION (build-path ROOT "../base/HtDP2e"))

(define info-fmt "../base/info-~a~a.rktl")

(define info-htdp (format info-fmt HTDP2 ""))
(define info-note (format info-fmt NOTES ""))
(define draft-info-htdp (format info-fmt HTDP2 DRAFT))
(define draft-info-note (format info-fmt NOTES DRAFT))

;; create a renderer and a path for the documentation, then scribble the desired document 
(define (process-whole draft? scribble-it stem destination [maybe-flag #f])
  (define redirect
    (if draft?
        "http://plt.eecs.northwestern.edu/snapshots/current/doc/"
        "http://docs.racket-lang.org/"))
  (define renderer (compose render-multi-mixin render-mixin))
  (scribble-it draft? stem destination redirect renderer ));maybe-flag))
  ;(apply scribble-it draft? stem destination redirect renderer stuff))


;; run renderer on the remaining arguments with keywords supplied 
;; it's a syntax rule because I don't know how to supply an optional keyword otherwise 
;; (without running a decision again and thus duplicating the whole thing)
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
