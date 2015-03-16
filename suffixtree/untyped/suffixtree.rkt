#lang racket/base
(require (prefix-in u: "private/ukkonen2.rkt")
         (prefix-in l: "private/label.rkt")
         (prefix-in s: "private/structs.rkt")
         racket/contract)


;; Basic predicates
(provide/contract (tree? (-> any/c boolean?)))
(define tree? s:suffix-tree?)

(provide/contract (node? (-> any/c boolean?)))
(define node? s:node?)

(provide/contract (label? (-> any/c boolean?)))
(define label? l:label?)

(provide/contract (label-element? (-> any/c boolean?)))
(define label-element? l:label-element?)


;; Labels
(define nonempty-label/c
  (flat-named-contract "nonempty-label"
                       (lambda (datum)
                         (and (l:label? datum)
                              (not (l:label-empty? datum))))))

(provide/contract (string->label (-> string? label?)))
(define string->label l:string->label)

(provide/contract (string->label/with-sentinel (-> string? label?)))
(define string->label/with-sentinel l:string->label/with-sentinel)

(provide/contract (vector->label (-> vector? label?)))
(define vector->label l:vector->label)

(provide/contract (vector->label/with-sentinel (-> vector? label?)))
(define vector->label/with-sentinel l:vector->label/with-sentinel)

(provide/contract (label->vector (-> label? vector?)))
(define label->vector l:label->vector)

(provide/contract (label->string (-> label? string?)))
(define label->string l:label->string)

(provide/contract (label->string/removing-sentinel (-> label? string?)))
(define label->string/removing-sentinel l:label->string/removing-sentinel)

(provide/contract (label-equal? (-> label? label? boolean?)))
(define label-equal? l:label-equal?)

(provide/contract (label-length (-> label? natural-number/c)))
(define label-length l:label-length)

(provide/contract (label-ref (-> label? natural-number/c label-element?)))
(define label-ref l:label-ref)

(provide/contract (sublabel (case->
                             (-> label? natural-number/c natural-number/c label?)
                             (-> label? natural-number/c label?))))
(define sublabel l:sublabel)

(provide/contract (label-source-id (-> label? integer?)))
(define label-source-id l:label-source-id)

(provide/contract (label-source-eq? (-> label? label? boolean?)))
(define label-source-eq? l:label-same-source?)



;; Trees
(provide/contract (make-tree (-> tree?)))
(define make-tree s:new-suffix-tree)

(provide/contract (tree-root (-> tree? node?)))
(define tree-root s:suffix-tree-root)

(provide/contract (tree-add! (-> tree? nonempty-label/c void?)))
(define tree-add! u:suffix-tree-add!)



;; Nodes
(provide/contract (node-up-label (-> node? label?)))
(define node-up-label s:node-up-label)

(provide/contract (node-children (-> node? (listof node?))))
(define node-children s:node-children)

(provide/contract (node-parent (-> node? (or/c false/c node?))))
(define node-parent s:node-parent)

(provide/contract (node-find-child (-> node? label-element?
                                       (or/c false/c node?))))
(define node-find-child s:node-find-child)

(provide/contract (node-suffix-link (-> node? (or/c false/c node?))))
(define node-suffix-link s:node-suffix-link)


;; tree-walk: tree label (node number -> A)
;;                       (node number number -> B) -> (union A B)
(provide/contract (tree-walk (-> tree? label? procedure? procedure?
                                 any/c)))
(define (tree-walk tree input-label succeed-f fail-f)
  (letrec [(comparing-label-elements
            (lambda (node up-label up-label-offset input-label-offset)
              (cond
               ((= input-label-offset (label-length input-label))
                (succeed-f node up-label-offset))
               ((= up-label-offset (label-length up-label))
                (choosing-next-edge node input-label-offset))
               ((l:label-element-equal?
                 (label-ref up-label up-label-offset)
                 (label-ref input-label input-label-offset))
                (comparing-label-elements node up-label
                                          (add1 up-label-offset)
                                          (add1 input-label-offset)))
               (else
                (fail-f node up-label-offset input-label-offset)))))
           (choosing-next-edge
            (lambda (node input-label-offset)
              (let ((child
                     (node-find-child
                      node (label-ref input-label input-label-offset))))
                (if child
                    (comparing-label-elements child
                                              (node-up-label child) 0
                                              input-label-offset)
                    (fail-f node (label-length (node-up-label node))
                            input-label-offset)))))]
    (if (= 0 (label-length input-label))
        (begin
          (succeed-f (tree-root tree) 0))
        ;; Start off the walk at the root.
        (choosing-next-edge (tree-root tree) 0))))


(provide/contract (tree-contains? (-> tree? label? boolean?)))
(define (tree-contains? tree label)
  (tree-walk tree label 
             (lambda (node edge-label-offset) #t)
             (lambda (node edge-label-offset input-label-offset) #f)))

