#lang typed/racket

;; implement the view (renderer) for the T path finder

(provide 
 ;; type Manage 
 
 ;; Manage
 ;; a class for managing the 'view' of paths through the T system
 manage%)

;; ===================================================================================================
(require benchmark-util)
(require "graph-types.rkt")
(require/typed/check "t-graph.rkt"
  [read-t-graph (-> [Instance MBTA])])

;; BAD: 
;; [X -> Real] [Listof X] -> X
(: selector (All (X) (-> [Listof [Listof X]] [Listof X])))
;; argmax also okay 
;; select an [Listof X] that satisfies certain length criteria 
(define (selector l) ; eta expansion needed to bring X into scope of body 
  [((inst curry [-> [Listof X] Real] [Listof X] [Listof [Listof X]])
    argmin
    (lambda ({p : [Listof X]}) (length (filter string? p)))) l])

;; ---------------------------------------------------------------------------------------------------

(define INTERNAL           "find path: it is impossible to get from ~a to ~a [internal error]")

(define CURRENT-LOCATION   "disambiguate your current location: ~a") 
(define CURRENT-LOCATION-0 "no such station: ~a") 
(define DESTINATION        "disambiguate your destination: ~a")
(define DESTINATION-0      "no such destination: ~a")
(define NO-PATH            "it is currently impossible to reach ~a from ~a via subways")
(define DISABLED           "clarify station to be disabled: ~a")
(define ENABLED            "clarify station to be enabled: ~a")
(define DISABLED-0         "no such station to disable: ~a")
(define ENABLED-0          "no such station to enable: ~a")

(define ENSURE             "---ensure you are on ~a")
(define SWITCH             "---switch from ~a to ~a")

;; type Path* ~~ Path with "switch from Line to Line" strings in the middle 
(define-type Path* [Listof (U String Station-x-Line)])
;; BAD: this ought to appear inside of the class only 

(: manage% Manage)
(define manage%
  (class object% 
    (super-new)
    
    (field 
     [mbta-subways (read-t-graph)]
     [disabled '()])
    
    ;; -----------------------------------------------------------------------------------------------
    (define/public (add-to-disabled s)
      (define station (send mbta-subways station s))
      (cond
        [(string? station) (set! disabled (cons station disabled)) #f]
        [(empty? station) (format DISABLED-0 s)]
        [else (format DISABLED (string-join station))]))
    
    ;; -----------------------------------------------------------------------------------------------
    (define/public (remove-from-disabled s)
      (define station (send mbta-subways station s))
      (cond
        [(string? station) (set! disabled (remove* (list station) disabled)) #f]
        [(empty? station) (format ENABLED-0 s)]
        [else (format ENABLED (string-join station))]))
    
    ;; -----------------------------------------------------------------------------------------------
    (define/public (find from to)
      (define from-station (send mbta-subways station from))
      (define to-station   (send mbta-subways station to))
      (cond
        [(cons? from-station) (format CURRENT-LOCATION (string-join from-station))]
        [(cons? to-station)   (format DESTINATION (string-join to-station))]
        [(empty? from-station) (format CURRENT-LOCATION-0 from)]
        [(empty? to-station)   (format DESTINATION-0 to)]
        [else 
         (define paths (send mbta-subways find-path from-station to-station))
         (define path* (removed-paths-with-disabled-stations paths))
         (cond
           [(empty? paths) (format INTERNAL from-station to-station)]
           [(empty? path*) (format NO-PATH to-station from-station)]
           [else 
            (define paths-with-switch (for/list : [Listof Path*] ([p path*]) (insert-switch p)))
            (define best-path-as-string*
              (for/list : [Listof String] ([station-or-comment (pick-best-path paths-with-switch)])
                (match station-or-comment
                  [`(,name ,line) (string-append name ", take " (send mbta-subways render line))]
                  [(? string? comment) comment])))
            (string-join best-path-as-string* "\n")])]))
    
    ;; -----------------------------------------------------------------------------------------------
    (: removed-paths-with-disabled-stations (-> [Listof Path] [Listof Path]))
    (define/private (removed-paths-with-disabled-stations paths*)
      (for/list ([p : Path paths*] 
                 #:unless ;; any of the disabled stations is on the path 
                 (let ([stations ((inst map Station Station-x-Line) first p)])
                   ;; MF: why is this Any needed? 
                   (for/or : Any ((s stations)) (member s disabled))))
        p))

    ;; -----------------------------------------------------------------------------------------------
    (: pick-best-path (-> [Listof Path*] Path*))
    (define/private (pick-best-path paths*)
      (selector paths*))
    
    ;; -----------------------------------------------------------------------------------------------
    (: insert-switch : (-> Path Path*))
    (define/private (insert-switch path0)  
      (define start (first path0))
      (define pred-lines0 (second start))
      (define pred-string0 (send mbta-subways render pred-lines0))
      (cons start
            (let loop : Path* ([pred-lines : [Setof Line] pred-lines0]
                               [pred-string : String pred-string0]
                               [path : Path (rest path0)])
              (cond
                [(empty? path) '()]
                [else 
                 (define stop (first path))
                 (define name (first stop))
                 (define stop-lines (second stop))
                 (define stop-string (send mbta-subways render stop-lines))
                 (define remainder (loop stop-lines stop-string (rest path)))
                 (cond
                   [(proper-subset? stop-lines pred-lines)
                    (list* (format ENSURE stop-string) stop remainder)]
                   [(set-empty? (set-intersect stop-lines pred-lines)) 
                    (list* (format SWITCH pred-string stop-string) stop remainder)]
                   [else (cons stop remainder)])]))))))

