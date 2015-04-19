#lang typed/racket

(provide 
 (struct-out unweighted-graph)
 ;; types 
 MBTA
 Path
 Station-x-Line
 Station*
 Station
 Line*
 Line
 Colors->Lines
 Manage)

;; ===================================================================================================

(require benchmark-util)
(require/typed/check "graph-struct.rkt"
  [#:struct unweighted-graph
            ([get-vertices : (-> (Listof String))]
             [in-neighbors : (-> String (Sequenceof String))])])
                                 
(define-type MBTA
  (Class 
   (init-field
    [G unweighted-graph]
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

(define-type Manage
  (Class 
   (field [mbta-subways [Instance MBTA]]
          [disabled [Listof Station]])
   ;; disable the given station: #f for sucecss, String for failure
   [add-to-disabled (-> String [U False String])]
   ;; enable the given station: #f for sucecss, String for failure
   [remove-from-disabled (-> String [U False String])]
   ;; turn the inquiry strings into stations and find a path from the first to the second  
   [find (-> String String String)]))

