#lang at-exp racket/base

(provide (all-from-out "bib.rkt")
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
         remark
         deliverable
         usable

         red
         green
         blue

         id
         library
         PHIL
         todo ;; Remove this export before submission

         type
         ctc ;; aka, 'contract'

         PFDS-BEFORE
         PFDS-BEFORE-str
         PFDS-AFTER
         PFDS-AFTER-str

         noindent
         )

(require "bib.rkt"
         "gradual-bib.rkt" ; copied from the github repo
         "util.rkt"
         glob
         racket/class
         racket/require
         racket/string
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
       (printf "HELLO DATE CITE ~a\n" date-cite)
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

(define (exact #:style [st "relax"] . items)
  (make-element (make-style st '(exact-chars))
                items))

(define (mt-line) (parag))

(define (make-defthing str)
  (lambda (#:term (term #false) . x)
    (make-paragraph plain
      (list
        (mt-line)
        (exact #:style #f "\\vspace{-4ex}\\begin{center}\\begin{minipage}{0.88\\textwidth}\n")
        (bold str)
        (cons (if term (element #f (list " (" (defterm term) ") ")) " ") x)
        (exact #:style #f "\\end{minipage}\\end{center}\n")
        ))))

(define def
  (make-defthing "Definition"))

(define remark
  (make-defthing "Remark "))

(define (deliverable [d "D"])
  (make-element plain @list{@(math d)-deliverable}))

(define (usable [d "D"] [u "U"])
  (make-element plain @list{@(math d"/"u)-usable}))

(define (step [k "k"] [d "D"])
  (make-element plain @list{@(math k)-step @(math d)-deliverable}))

;; Format an identifier
;; Usage: @id[x]
(define (id x)
  (make-element plain @format["~a" x]))

(define (library x)
  (tt x))

(define PHIL
  ;; Dammit Phil
  "Nguy{\\~{\\^{e}}}n")

(define (todo x)
  (make-element 'bold @string-append["TODO: " x]))

(define-values (PFDS-BEFORE PFDS-BEFORE-str)
  (let ([v 12]
        [u "seconds"])
    (values (format "~a ~a" v u)
            (format "~a ~a" (integer->word v) u))))

(define-values (PFDS-AFTER PFDS-AFTER-str)
  (let ([v 1]
        [u "millisecond"])
    (values (format "~a ~a" v u)
            (format "~a ~a" (integer->word v) u))))

(define (type t)
  (exact (string-append "\\RktMeta{" t "}")))

(define (ctc . t)
  (exact (string-append "$\\ctc{\\RktMeta{" (string-join t) "}}$")))

(define (make-colorizer c)
  (lambda (txt)
    (raise-user-error 'colorizer "not implemented")))

(define red
  (make-colorizer 1)) ;(->pen-color 1)))

(define green
  (make-colorizer 2)) ;(->pen-color 2)))

(define blue
  (make-colorizer 3)) ;(->pen-color 3)))

(define (noindent)
  (exact "\\hspace{-0.8em}"))
