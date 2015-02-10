#lang typed/racket

;; implements the model for the T path finder 

(provide read-t-graph)

;; ===================================================================================================
(require "../base/t-graph-types.rkt")
(require/typed "../base/my-graph.rkt"
               [unweighted-graph/directed (-> Connection* Graph)]
               [in-neighbors (-> Graph Station [Sequenceof Station])]
               [attach-edge-property 
                (->* (Graph) (#:init [Setof Line] #:for-each Any) 
                     (Values 
                      (-> Station Station (Setof Line))
                      Any
                      (-> Station Station [Setof Line] Void)))])

(define-type Line->Connection* [Listof [List Line Connection*]])
(define-type Connection* [Listof Connection])
(define-type Connection  [List Station Station])

(define SOURCE-DIRECTORY "../base/~a.dat")
(define COLORS '("blue" "orange" "green" "red"))

;; ---------------------------------------------------------------------------------------------------
(: line-specification (-> String (U False Line*)))
(define (line-specification line)
  (define r (regexp-match #px"--* (.*)" line))
  (and r #;"for type checker:" (cadr r) 
       ;; the outer cast is for conversting strings to lines
       (cast (string-split (cast (cadr r) String)) Line*)))

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
  (define-values (stations connections lines color->lines) (read-parse-organize))
  (define-values (graph connection-on) (generate-graph connections lines))
  (new mbta% [G graph][stations stations][bundles color->lines][connection-on connection-on]))

;; ---------------------------------------------------------------------------------------------------
(: read-parse-organize (-> (Values Station* Connection* Line->Connection* Colors->Lines)))
(define (read-parse-organize)
  (for/fold : (Values Station* Connection* Line->Connection* Colors->Lines) 
    ((s* : Station* '()) (c* : Connection* '()) (lc : Line->Connection* '()) (cl : Colors->Lines '()))
    ((color COLORS))
    (define-values (new-s* new-lc) (read-t-line-from-file color))
    (define new-c* ((inst map Connection* [List Line Connection*]) second new-lc))
    (define new-cl (list color (apply set (map {inst first Line Connection*}  new-lc))))
    (values (append new-s* s*) (apply append c* new-c*) (append new-lc lc) (cons new-cl cl))))

;; ---------------------------------------------------------------------------------------------------
(: generate-graph (-> Connection* Line->Connection* (Values Graph (Station Station -> [Setof Line]))))
(define (generate-graph connections lines)
  (define graph (unweighted-graph/directed connections))
  (define-values (connection-on _  connection-on-set!)
    (attach-edge-property graph #:init (ann (set) [Setof Line])))
  (for ((line (in-list lines)))
    (define name (first line))
    (for ((c (second line)))
      (define from (first c))
      (define to (second c))
      (connection-on-set! from to (set-add (connection-on from to) name))))
  (values graph connection-on))

;; ---------------------------------------------------------------------------------------------------
;; auxiliary type defs to break down the problem, struct for type checking
;; these lines should be moved into the lexical scope of lines->hash 
;; or they should be in a submodule but drracket doesn't do a good job on those
(struct partial-line ({pred : Station} {connections : Connection*}))
(define-type PartialLine partial-line)
(define-type H-Line->Connection* [HashTable Line PartialLine])

(: read-t-line-from-file (-> String (Values Station* Line->Connection*)))
(define (read-t-line-from-file line-file)
  (define file*0 (file->lines (format SOURCE-DIRECTORY line-file)))
  ;; this re-ordering matches the type-oriented decomposition I used in the untyped world
  ;; -------------------------------------------------------------------------------------------------
  (: lines->hash (-> Line* (Values Station* Line->Connection*)))
  (define (lines->hash lines0)
    (define mt-partial-line : Connection* '())
    (define first-station (second file*0))
    (define hlc0 
      (make-immutable-hash
       (for/list : [Listof [Pairof Line PartialLine]] ([line lines0])
         (cons line (partial-line first-station mt-partial-line)))))
    (define-values (stations hlc) (process-file-body (cddr file*0) (list first-station) lines0 hlc0))
    (define line->connection*
      (for/list : Line->Connection* ([({line : Line} {partial-line : PartialLine}) hlc])
        (list line (partial-line-connections partial-line))))
    (values (reverse stations) line->connection*))
  ;; -------------------------------------------------------------------------------------------------
  (: process-file-body 
     (-> [Listof String] Station* Line* H-Line->Connection*
         (Values Station* H-Line->Connection*)))
  (define (process-file-body file* stations lines hlc)
    (cond
      [(empty? file*) (values stations hlc)]
      [else 
       (define ?station (string-trim (first file*)))
       (cond
         [(line-specification ?station) 
          => (lambda (lines) (process-file-body (rest file*) stations lines hlc))]
         [else ;; now we know ?station is a station
          (define new-hlc (add-station-to-lines hlc lines ?station))
          (process-file-body (rest file*) (cons ?station stations) lines new-hlc)])]))
  ;; -------------------------------------------------------------------------------------------------
  (: add-station-to-lines (-> H-Line->Connection* Line* Station H-Line->Connection*))
  (define (add-station-to-lines hlc lines station)
    (for/fold : H-Line->Connection* ([hlc1 hlc]) ([line lines])
      (define the-partial-line (hash-ref hlc1 line (lambda () (error "KNOLWEDGE: impossible"))))
      (define predecessor (partial-line-pred the-partial-line))
      (define prefix (partial-line-connections the-partial-line))
      (define connections (list* (list predecessor station) (list station predecessor) prefix))
      (hash-set hlc1 line (partial-line station connections))))
  ;; -------------------------------------------------------------------------------------------------
  ;; IN: check file format (prefix), then process proper file content 
  (define lines0 (line-specification (first file*0)))
  (unless lines0 (error "KNOWLEDGE: we know that the file is properly formatted"))
  (lines->hash lines0))

;; ---------------------------------------------------------------------------------------------------

(: mbta% MBTA)
(define mbta%
  (class object% (init-field G stations connection-on bundles)
    
    (: stations-set [Setof Station])
    (define stations-set (apply set stations))
    
    (super-new)
    
    (define/public (render b)
      (define r (memf (lambda ({c : [List String [Setof Line]]}) (subset? (second c) b)) bundles))
      (if r (first (first r)) (string-join (set-map b (lambda ({x : String}) x)) " ")))
    
    (define/public (station word)
      (define word# (regexp-quote word))
      (define candidates
        (for/list : Station* ([s stations-set] #:when (regexp-match word# s))
          s))
      (if (and (cons? candidates) (empty? (rest candidates)))
          (first candidates)
          candidates))
    
    (define/public (station? s) 
      (set-member? stations-set s))
    
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
    
    (: find-path/aux (-> Station Station [Listof [Pairof Station Station*]]))
    (define/private (find-path/aux from0 to)
      (let search :  [Listof [Pairof Station Station*]] ([from from0][visited '()])
        (cond
          [(equal? from to) (list (list from))]
          [(member from visited) (list)]
          [else
           (define visited* (cons from visited))
           (for/fold : [Listof [Pairof Station Station*]] 
             ((all-paths : [Listof [Pairof Station Station*]] '())) 
             ([n (in-neighbors G from)])
             (define paths-from-from-to-to
               (map (lambda ({p : Station*}) : [Pairof Station Station*] 
                      (cons from p))
                    (search n visited*)))
             (append all-paths paths-from-from-to-to))])))))
