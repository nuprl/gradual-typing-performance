#lang typed/racket

;; implements the model for the T path finder 

(provide read-t-graph)

;; ===================================================================================================
(require "t-graph-types.rkt")
(require/typed
 "my-graph.rkt"
 [unweighted-graph/directed (-> Connections Graph)]
 [attach-edge-property 
  (->* (Graph) (#:init [Setof Line] #:for-each Any) 
       (Values (-> Station Station (Setof Line)) Any (-> Station Station [Setof Line] Void)))]
 [in-neighbors (-> Graph Station [Sequenceof Station])])

(define-type Lines [Listof [List Line Connections]])
(define-type Connections [Listof Connection])
(define-type Connection  [List Station Station])
(define-type ***Bundles [Listof [List String [Setof Line]]])

(define SOURCE-DIRECTORY "../Data/~a.dat")
(define COLORS '("blue" "orange" "green" "red"))

;; ---------------------------------------------------------------------------------------------------
(: line-specification? (-> String (U False [Listof Line])))
(define (line-specification? line)
  (define r (regexp-match #px"--* (.*)" line))
  (and r #;"for type checker:" (car (cdr r)) (string-split (cast (car (cdr r)) String)) #f))

#| ASSUMPTIONS about source files:

   A data file has the following format: 
   LineSpecification 
   [Station
    | 
    LineSpecification
    ]* 

   A LineSpecification consists of dashes followed by the name of lines, separated by blank spaces. 

   A Station is the string consisting of an entire line, minus surrounding blank spaces. 
|#

;; ---------------------------------------------------------------------------------------------------
(: read-t-graph (-> [Instance MBTA]))
(define (read-t-graph)
  (define-values (all-lines bundles)
    (for/fold : (Values Lines Bundles)
      ((all-lines : Lines '()) (all-bundles : Bundles '())) ((color COLORS))
      (define next (read-t-line-from-file color))
      (define los ((inst map Line [List Line Connections]) first next))
      (values (append next all-lines)
              (cons (list color (apply set los)) all-bundles))))
  
  (define connections 
    (apply append ((inst map Connections [List Line Connections]) second all-lines)))
  (define stations 
    (set-map
     (for/fold : [Setof Station] ((s* : [Setof Station] (set))) ((c : Connection connections)) 
       (set-add s* (first c)))
     (lambda ({x : Station}) x)))
  
  (define graph (unweighted-graph/directed connections))
  (define set-of-lines : (Setof Line) (set))
  (define-values (connection-on _  connection-on-set!)
    (attach-edge-property graph #:init set-of-lines))
  (for ((line (in-list all-lines)))
    (define name (first line))
    (define connections* (second line))
    (for ((c connections*))
      (define from (first c))
      (define to (second c))
      (connection-on-set! from to (set-add (connection-on from to) name))))
  
  (new mbta% [G graph][stations stations][bundles bundles][connection-on connection-on]))

;; ---------------------------------------------------------------------------------------------------
(: read-t-line-from-file (-> String Lines))
(define (read-t-line-from-file line-file)
  (define files-as-lines (file->lines (format SOURCE-DIRECTORY line-file)))
  (for/list : Lines ([({name : Line} {line : PartialLine}) (lines->hash files-as-lines)])
    (list name (partial-line-connections line))))

;; ---------------------------------------------------------------------------------------------------
(struct partial-line ({pred : Station} {connections : Connections}))
(define-type PartialLine partial-line)
(define-type HLines [HashTable Line PartialLine])

(: lines->hash (-> [Listof String] HLines))
(define (lines->hash lines0)
  (define names0 (line-specification? (first lines0)))
  (cond
    [(boolean? names0) (error "KNOWLEDGE: we know that the file is properly formatted")]
    [else 
     (define pred0 (second lines0))
     (define Hlines0 : HLines 
       (make-immutable-hash
        (for/list : [Listof [Pairof Line PartialLine]] ([name : Line names0])
          (cons name (partial-line pred0 (ann '() Connections))))))
     (let read-t-line : HLines ([lines (cddr lines0)][names names0][Hlines Hlines0])
       (cond
         [(empty? lines) Hlines]
         [else 
          (define current-stop (string-trim (first lines)))
          (cond
            [(line-specification? current-stop) 
             => (lambda (names) (read-t-line (rest lines) names Hlines))]
            [else 
             (define new-connections
               (for/fold : HLines ([Hlines1 Hlines]) ([name : Line names])
                 (define line 
                   ((inst hash-ref Line PartialLine Nothing)
                    Hlines1 name (lambda () (error "KNOLWEDGE: impossible"))))
                 (define predecessor (partial-line-pred line))
                 (define connections 
                   (list* (list predecessor current-stop)
                          (list current-stop predecessor)
                          (partial-line-connections line)))
                 (define station-connections (partial-line current-stop connections))
                 ((inst hash-set Line PartialLine) Hlines1 name station-connections)))
             (read-t-line (rest lines) names new-connections)])]))]))

;; ---------------------------------------------------------------------------------------------------

(: mbta% MBTA)
(define mbta%
  (class object% (init-field G stations connection-on bundles)
    
    (super-new)
    
    (define/public (render b)
      (define r (memf (lambda ({c : [List String [Setof Line]]}) (subset? (second c) b)) bundles))
      (if r (first (first r)) (string-join (set-map b (lambda ({x : String}) x)) " ")))
    
    (define/public (station word)
      (define word# (regexp-quote word))
      (define candidates
        (for/list : [Listof Station] ([s stations] #:when (regexp-match word# s))
          s))
      (if (and (cons? candidates) (empty? (rest candidates)))
          (first candidates)
          candidates))
    
    (define/public (station? s)
      (cons? ((inst member Station) s stations)))
    
    (define/public (find-path from0 to)
      (define paths* (find-path/aux from0 to))
      (for/list : [Listof Path] ((path paths*))
        (define start (first path))
        (cond
          [(empty? (rest path)) (list (list start (ann (set) [Setof Line])))]
          [else             
           (define next (connection-on start (second path)))
           (define-values (_ result)
             (for/fold : (Values Any Path)
               ([predecessor : Station start]
                [r : Path (list (list start next))])
               ((station : Station (rest path)))
               (values station (cons (list station (connection-on station predecessor)) r))))
           (reverse result)])))
    
    (: find-path/aux (-> Station Station [Listof [Pairof Station [Listof Station]]]))
    (define/private (find-path/aux from0 to)
      (let search :  [Listof [Pairof Station [Listof Station]]] ([from from0][visited '()])
        (cond
          [(equal? from to) (list (list from))]
          [(member from visited) (list)]
          [else
           (define visited* (cons from visited))
           (for/fold : [Listof [Pairof Station [Listof Station]]] 
             ((all-paths : [Listof [Pairof Station [Listof Station]]] '())) 
             ([n (in-neighbors G from)])
             (define paths-from-from-to-to
               (map (lambda ({p : [Listof Station]}) : [Pairof Station [Listof Station]] 
                      (cons from p))
                    (search n visited*)))
             (append all-paths paths-from-from-to-to))])))))
