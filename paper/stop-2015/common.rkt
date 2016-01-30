#lang racket/base

(require "bib.rkt"
         "gradual-bib.rkt" ; copied from the github repo
         benchmark-util/data-lattice
         (except-in pict table)
         racket/file
         racket/require
         scribble/core
         scribble/eval
         scribble/manual
         scriblib/autobib
         scriblib/figure
         scriblib/footnote
         setup/main-collects
         scribble/html-properties
         scribble/latex-properties
         racket/class
         (for-label ;; no predicates, just "forms"
                    (except-in racket/base string? eq? displayln void?)
                    (except-in (subtract-in typed/racket/base racket/base)
                               Integer String Void Boolean Natural Any)
                    ;; don't highlight object% because that
                    ;; looks weird for random readers
                    (except-in racket/class object%)))

(provide (all-from-out "bib.rkt")
         (all-from-out "gradual-bib.rkt")
         (all-from-out benchmark-util/data-lattice)
         (all-from-out pict)
         (all-from-out racket/file)
         (all-from-out scriblib/footnote)
         (all-from-out scriblib/figure)
         (all-from-out scribble/eval)
         (all-from-out scriblib/autobib)
         (for-label Classof
                    (all-from-out racket/base racket/class typed/racket/base))
         (except-out (all-from-out scribble/manual)
                     author)
         ~cite
         citet
         etal
         exact
         generate-bibliography
         nrightarrow
         parag
         sf
         toor)

;; Just for syntax highlighting in Scribble
(module dummy racket (define Classof #f) (provide Classof))
(require (for-label (submod "." dummy)))

;; module that just binds stuff for highlighting
;; no longer needed in 6.0.0.3 with TR classes
;(require (for-label "dummy.rkt"))
;(provide (for-label (all-from-out "dummy.rkt")))


(define autobib-style-extras
  (let ([abs (lambda (s)
               (path->main-collects-relative
                (collection-file-path s "scriblib")))])
    (list
     (make-css-addition (abs "autobib.css"))
     (make-tex-addition (abs "autobib.tex")))))

(define bib-single-style (make-style "AutoBibliography" autobib-style-extras))

(define bibentry-style (make-style "Autobibentry" autobib-style-extras))
(define colbibnumber-style (make-style "Autocolbibnumber" autobib-style-extras))
(define colbibentry-style (make-style "Autocolbibentry" autobib-style-extras))

(define small-number-style
  (new
   (class object%
     (define/public (bibliography-table-style) bib-single-style)
     (define/public (entry-style) colbibentry-style)
     (define/public (disambiguate-date?) #f)
     (define/public (collapse-for-date?) #f)
     (define/public (get-cite-open) "[")
     (define/public (get-cite-close) "]")
     (define/public (get-group-sep) ", ")
     (define/public (get-item-sep) ", ")
     (define/public (render-citation date-cite i) 
       (make-element 
        (make-style "Thyperref" (list (command-extras (list (make-label i)))))
        (list (number->string i))))
     (define/public (render-author+dates author dates) dates)
     (define (make-label i)
       (string-append "autobiblab:" (number->string i)))
     (define/public (bibliography-line i e)
       (list (make-paragraph plain
                             (make-element colbibnumber-style 
                                           (list
                                            (make-element (make-style "label" null)
                                                          (make-label i))
                                            "[" (number->string i) "]")))
             e))
     (super-new))))

(define author+date-style/link
  (new
   (class object%
     (define/public (bibliography-table-style) bib-single-style)
     (define/public (entry-style) bibentry-style)
     (define/public (disambiguate-date?) #t)
     (define/public (collapse-for-date?) #t)
     (define/public (get-cite-open) "(")
     (define/public (get-cite-close) ")")
     (define/public (get-group-sep) "; ")
     (define/public (get-item-sep) ", ")
     (define/public (render-citation date-cite i) 
       (make-element 
        (make-style "Thyperref" (list (command-extras (list (make-label i)))))
        date-cite))
     (define/public (render-author+dates author dates)
        (list* author " " dates))
     (define (make-label i)
       (string-append "autobiblab:" (number->string i)))
     (define/public (bibliography-line i e) 
       (list (make-compound-paragraph
              plain
              (list (make-paragraph plain (list (make-element (make-style "label" null)
                                                              (make-label i))))
                     e))))
     (super-new))))


(define-cite ~cite citet generate-bibliography
  ;; change this to small-number-style if you want the other way
  ;;#:style small-number-style
  )

(define etal (emph "et al."))

(define nrightarrow (elem #:style "mynra"))

(define (sf x) (elem #:style "sfstyle" x))

(define (parag . x) (apply elem #:style "paragraph" x))

(define (exact . items)
  (make-element (make-style "relax" '(exact-chars))
                items))

(define (toor) (elem "TR"))
