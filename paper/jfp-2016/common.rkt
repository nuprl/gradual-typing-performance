#lang at-exp racket/base

(provide (all-from-out "bib.rkt")
         (all-from-out "data.rkt")
         (all-from-out "gradual-bib.rkt")
         (all-from-out scriblib/footnote)
         (all-from-out scriblib/figure)
         (all-from-out scribble/eval)
         (all-from-out scriblib/autobib)
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

         step
         def
         deliverable
         usable

         id
         )

(require "bib.rkt"
         "data.rkt"
         "gradual-bib.rkt" ; copied from the github repo
         racket/class
         racket/require
         scribble/core
         scribble/eval
         scribble/manual
         scriblib/autobib
         scriblib/figure
         scriblib/footnote
         setup/main-collects
         scribble/html-properties
         scribble/latex-properties)

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


(define-cite ~cite citet generate-bibliography)

(define etal "et al.")

(define nrightarrow (elem #:style "mynra"))

(define (sf x) (elem #:style "sfstyle" x))

(define (parag . x) (apply elem #:style "paragraph" x))

(define (exact . items)
  (make-element (make-style "relax" '(exact-chars))
                items))

(define (mt-line) (parag))

(define (def #:term (term #false) . x)
  (make-paragraph plain
    (list
      (mt-line)
      (bold "Definition")
      (cons (if term (element #f (list " (" (defterm term) ") ")) " ") x)
      (mt-line))))

(define (deliverable N)
  (make-element plain @list{@math{@N}-deliverable}))

(define (usable N M)
  (make-element plain @list{@(math N"/"M)-usable}))

(define (step L N M)
  (make-element plain @list{@(math L)-step @(math N "/" M)-usable}))

;; Format an identifier
;; Usage: @id[x]
(define (id x)
  (make-element plain @format["~a" x]))

