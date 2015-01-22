#lang typed/racket

(provide 
 ;; types 
 Graph
 MBTA
 Path 
 Station*
 Station 
 Line
 Bundles)

;; ===================================================================================================
(require/typed graph [#:opaque Graph graph?])

(define-type MBTA
  (Class 
   (init-field
    [G Graph]
    [stations [Listof Station]]
    [connection-on [-> Station Station [Setof Line]]]
    [bundles Bundles])
   [find-path (-> Station Station [Listof Path])]
   [render (-> [Setof Station] String)]
   [station?  (-> String Boolean)]
   [station   (-> String (U Station [Listof Station]))]))

(define-type Path [Listof Station*])
;; interpretation: take the specified lines to the next station from here 

(define-type Station* [List Station [Setof Line]])

(define-type Station String)

(define-type Line 
  (U "green"
     "E"
     "D"
     "C"
     "B"
     "red"
     "Mattapan"
     "Braintree"
     "orange"
     "blue"
     ))

(define-type Bundles [Listof [List String [Setof Line]]])