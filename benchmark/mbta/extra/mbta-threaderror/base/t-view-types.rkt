#lang typed/racket

(provide 
 ;; types 
 Manage)

;; =============================================================================
(require "t-graph-types.rkt")

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

