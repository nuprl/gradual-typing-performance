#lang racket/base
(require "suffixtree.rkt"
         "label.rkt"
         "structs.rkt"
         "ukkonen2.rkt"
         racket/list
         racket/contract
         racket/local
         (only-in srfi/1 find lset-union))

(provide longest-common-substring)

(provide/contract [longest-common-sublabels
                   ((listof label?) . -> . (listof label?))])


;; leaf?: node -> boolean
;; Returns true if this is a leaf node.
(define (leaf? node)
  (empty? (node-children node)))



;; longest-common-substring: string string -> string
;; Returns the longest common substring between s1 and s2.
(define (longest-common-substring . words)
  (let*-values ([(labels) (map string->label/with-sentinel words)])
    (apply string-append
           (map label->string/removing-sentinel
                (longest-common-sublabels labels)))))


;; longest-common-sublabels: (listof label) -> (listof label)
;; Returns the longest common sublabel shared by all.
(define (longest-common-sublabels labels)
  (let*-values ([(tree annotations)
                 (make-tree/annotations labels)])
    (local ((define N (length labels))
            (define (shared-by-all? node)
              (= N (length (hash-ref annotations node))))
            
            (define best-so-far '())
            (define best-so-far-length 0)
            
            (define (visit-children! node labels len visit!)
              (for-each
               (lambda (n)
                 (visit! n
                         (cons (node-up-label node) labels)
                         (+ len (label-length (node-up-label node)))))
               (node-children node)))
            
            (define (update-best! labels len)
              (when (> len best-so-far-length)
                (set! best-so-far labels)
                (set! best-so-far-length len))))
           
           ;; visit!: node (listof label) number -> void
           ;; The inner loop here will just walk the tree as long
           ;; as the node is shared by both.
           (let visit! ([node (tree-root tree)]
                        [labels '()]
                        [len 0])
             (cond
              [(shared-by-all? node)
               (cond [(leaf? node)
                      (update-best!
                       (cons (node-up-label node) labels)
                       (+ len (label-length (node-up-label node))))]
                     [else
                      (visit-children! node labels len visit!)])]
              [else
               (update-best! labels len)]))
           (reverse best-so-far))))


;; make-tree/annotations: (listof label) -> 
;;                          (values tree
;;                                  (hash-table-of node (listof label)))
(define (make-tree/annotations labels)
  (let* ((tree (make-tree))
         (_ (for-each (lambda (l) (tree-add! tree l)) labels))
         (ht (annotate-tree tree labels)))
    (values tree ht)))


;; annotate-tree: tree (listof label) -> 
;;                    (hash-table-of node -> (listof label))
;; Builds a hash table mapping nodes of the tree to the labels
;; that node belongs to.
(define (annotate-tree a-tree original-labels)
  (local ((define ht (make-hasheq))

          ;; FIXME: optimize using a hash table lookup structure.
          (define (label->original-label l)
            (find (lambda (o) (label-source-eq? l o))
                  original-labels))
          
          
          
          (define (update-leaf! node)
            (hash-set!
             ht node
             (list (label->original-label (node-up-label node)))))
          
          (define (update-parent! node)
            (hash-set!
             ht node
             (apply lset-union eq?
                    (map
                     (lambda (c) (hash-ref ht c))
                     (node-children node))))))
         (let loop ([node (tree-root a-tree)])
           (cond [(leaf? node)
                  (update-leaf! node)]
                 [else
                  (for-each loop (node-children node))
                  (update-parent! node)]))
         ht))
