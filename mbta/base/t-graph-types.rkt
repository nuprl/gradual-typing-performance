#lang typed/racket

(provide 
 ;; types 
 Graph
 MBTA
 Path 
 Station-x-Line
 Station*
 Station 
 Line*
 Line
 Colors->Lines)

;; ===================================================================================================
(require/typed "./graph/graph/main.rkt" [#:opaque Graph graph?])

(define-type MBTA
  (Class 
   (init-field
    [G Graph]
    [stations [Listof Station]]
    [connection-on (-> Station Station [Setof Line])]
    [bundles Colors->Lines])
   [find-path (-> Station Station [Listof Path])]
   [render (-> [Setof Station] String)]
   [station?  (-> String Boolean)]
   [station   (-> String (U Station [Listof Station]))]))

(define-type Path [Listof Station-x-Line])
;; interpretation: a sequence of stations with a set of lines to take to next station on the path

(define-type Station-x-Line [List Station [Setof Line]])
;; interpretation: connect a station to a set of lines

(define-type Station* [Listof Station])

(define-type Station String)

(define-type Line* [Listof Line])

(define-type Line (U "green" "E" "D" "C" "B" "red" "Mattapan" "Braintree" "orange" "blue"))

(define-type Colors->Lines [Listof [List String [Setof Line]]])
;; interpretation: associates external line names with the internally used, sub-lines
