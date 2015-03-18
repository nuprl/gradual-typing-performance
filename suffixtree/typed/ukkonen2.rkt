#lang typed/racket/base

;; Much of this comes from reading Dan Gusfield's Algorithms on
;; strings, trees, and sequences: computer science and computational
;; biology.

(require "label.rkt"
         "structs.rkt"
         "debug.rkt")


;; Let's add a debug log statement.
(debug-add-hooks ukkonen2
                 (debug enable-ukkonen-debug-messages
                        disable-ukkonen-debug-messages))
;; The following syntax and functions are generated:
;;
;; debug: string -> void   (syntax)
;; enable-ukkonen-debug-messages: -> void
;; disable-ukkonen-debug-messages: -> void
;;
;; We can call these functions to see what Ukkonen's algorithm is
;; really doing.
#;(provide enable-ukkonen-debug-messages)
#;(provide disable-ukkonen-debug-messages)


;; current-node->id: parameter of node -> string
;;
;; A debug-related parameter for mapping nodes to strings used in
;; debug messages.
(: current-node->id (Parameter (-> Node String)))
(define current-node->id
  (make-parameter 
   (let: ((hash : (HashTable Node String) (make-weak-hasheq)) ;; make sure we hold onto
         ;; the node keys weakly to
         ;; avoid memory leaking!
         (n : Integer 0)
         (sema : Semaphore (make-semaphore 1))
         (dummy : Node (node (make-label "dummy") #f '() #f)))
     (hash-set! hash dummy "#f")
     (lambda ([node : Node])
       (call-with-semaphore sema
                            (lambda ()
                              (hash-ref hash node
                                              (lambda ()
                                                (hash-set! hash node
                                                                 (number->string n))
                                                (set! n (add1 n))
                                                (hash-ref hash node)))))))))



;; skip-count: node label -> (values node number)
;;
;; Follows down the node using the skip-count rule until we exhaust
;; the label.  Assumes that there does exist a labeled path starting
;; from the node that exactly matches label.
(provide skip-count)
(: skip-count (-> Node Label (values Node Index)))
(define (skip-count node label)
  (define l-l (label-length label))
  (unless (index? l-l) (error "skip-count"))
  (skip-count-helper node label 0 l-l))

;; Utility function for skip count, but also visible for those in
;; the know to skip-count from an arbitrary position in label.
(: skip-count-helper (-> Node Label Index Index (values Node Index)))
(define (skip-count-helper node label k N)
  (: loop (-> Node Index (values Node Index)))
  (define (loop node k)
    (define l-k (label-ref label k))
    (unless (index? l-k) (error "skip-count-helper"))
    (: child Node)
    (define child 
      (let ([n (node-find-child node l-k)])
        (unless n (error "skip-count-helper")) n))
    (define child-label (node-up-label child))
    (define child-label-length (label-length child-label))
    (define rest-of-chars-left-to-skip (- N k))
    (define k* (+ k child-label-length))
    (cond [(and (> rest-of-chars-left-to-skip child-label-length)
                (index? k*))
           (loop child k*)]
          [(index? rest-of-chars-left-to-skip)
           (values child rest-of-chars-left-to-skip)]
          [else (error "skip-count-helper")]))
  (define u-l (label-length (node-up-label node)))
  (if (and (>= k N) (index? u-l))
      (values node u-l)
      (loop node k)))


;; jump-to-suffix: node -> (values node (union boolean number))
;;
;; Given an internal node, jumps to the suffix from that node.
;; According to the theory of suffix trees, such a node will exist
;; in the tree if we follow the Ukkonen construction.  If we had to
;; go up a few characters, returns the number of chars at the suffix
;; end that need to be compared to get the real suffix.

;; If we hit the root, that offset is #f to indicate that we have to
;; start searching the suffix from scratch.
(provide jump-to-suffix)
(: jump-to-suffix (-> Node (values Node (U Boolean Integer))))
(define (jump-to-suffix node)
  (define parent (node-parent node))
  (cond ((eq? #f parent) (error "jump"))
        ((node-root? node)
         (values node #f))
        ((node-suffix-link node)
         (begin (debug "following suffix link from ~a to ~a"
                       ((current-node->id) node)
                       ((current-node->id) (node-suffix-link node)))
                (let ([n (node-suffix-link node)])
                  (unless n (error "jump"))
                  (values n 0))))
        ((node-root? parent)
         (values parent #f))
        (else
           (let ([n2 (node-suffix-link parent)])
             (unless n2 (error "jump"))
             (values n2
                 (label-length (node-up-label node)))))))


(provide try-to-set-suffix-edge!)
;; try-to-set-suffix-edge!: node node -> void
;;
;; Sets the suffix edge of from-node directed to to-node if it
;; hasn't been set yet.
(: try-to-set-suffix-edge! (-> Node Node Void))
(define (try-to-set-suffix-edge! from-node to-node)
  (when (not (node-suffix-link from-node))
    (debug "setting suffix link from ~a to ~a"
           ((current-node->id) from-node)
           ((current-node->id) to-node))
    (set-node-suffix-link! from-node to-node)))

;; find-next-extension-point/add-suffix-link!: node label number number ->
;;     (values node number number)
;;
;; Given the last active node where an extension was last made,
;; looks for the next position for extension.  Returns that
;; extension point's node and label offset, as well as the new phase
;; number i.  (Postcondition: (>= i initial-i))
;;
;; The first pass through the loop is a special case: we set the
;; suffix link from node to suffix-node unless we expect it to be
;; done from a splicing extension.
;;
;; If we run off the label (implicit tree), returns (values #f #f #f).
(provide find-next-extension-point/add-suffix-link!)
(: find-next-extension-point/add-suffix-link! (-> Node Label Index Index (values (U #f Node) (U #f Index) (U #f Index))))
(define (find-next-extension-point/add-suffix-link! node label initial-i j)
  (: fixed-start (-> Integer Index))
  (define (fixed-start suffix-offset)
    (let ([i
            (if suffix-offset (- initial-i suffix-offset) j)])
      (unless (index? i) (error "find-next")) i))
  (define-values (suffix-node suffix-offset)
    (jump-to-suffix node))
  (define K
    (fixed-start (cond [(integer? suffix-offset) suffix-offset]
                       [(eq? #t suffix-offset) 1]
                       [(eq? #f suffix-offset) 0]
                       [else (error "find-next")])))
  (define N
    (let ([i (label-length label)])
      (unless (index? i) (error "find-next")) i))
   (: loop-first (-> Index (values (U #f Node) (U #f Index) (U #f Index))))
   (define (loop-first i)
     (loop-general i (lambda ([skipped-node : Node]
                              [skip-offset  : Index])
                             (when (node-position-at-end? skipped-node skip-offset) (try-to-set-suffix-edge! node skipped-node)))))
   (: loop-rest (-> Index (values (U #f Node) (U #f Index) (U #f Index))))
   (define (loop-rest i)
     (loop-general i (lambda ([skipped-node : Node] [skip-offset : Index])
                       (void))))
   (: loop-general (-> Index (-> Node Index Void) (values (U #f Node) (U #f Index) (U #f Index))))
   (define (loop-general i first-shot)
     (cond [(>= i N) (values #f #f #f)]
           [else 
             (define-values (skipped-node skipped-offset) (skip-count-helper suffix-node label K i))
             (first-shot skipped-node skipped-offset)
             (if (node-position-at-end? skipped-node skipped-offset)
                 (find-extension-at-end! skipped-node skipped-offset i)
                 (find-extension-in-edge skipped-node skipped-offset i))]))
   (: find-extension-in-edge (-> Node Index Index (values (U #f Node) (U #f Index) (U #f Index))))
   (define (find-extension-in-edge skipped-node skip-offset i)
     (cond [(label-element-equal? (label-ref label i) (label-ref (node-up-label skipped-node) skip-offset))
            (let ([n (add1 i)]) (unless (index? n) (error "find-next")) (loop-rest n))]
           [else (values skipped-node skip-offset i)]))
   (: find-extension-at-end! (-> Node Index Index (values (U #f Node) (U #f Index) (U #f Index))))
   (define (find-extension-at-end! skipped-node skip-offset i)
     (cond [(node-find-child skipped-node (label-ref label i))
            (let ([n (add1 i)]) (unless (index? n) (error "find-next")) (loop-rest n))]
           [else (values skipped-node skip-offset i)]))
   (loop-first initial-i))


;; (provide extend-at-point!)
;; ;; extend-at-point!: node number label number -> node
;; (define extend-at-point!
;;   (letrec [
;;            (main-logic
;;             (lambda (node offset label i)
;;               (if (should-extend-as-leaf? node offset)
;;                   (attach-as-leaf! node label i)
;;                   (splice-with-internal-node! node offset label i))))
;; 
;;            (should-extend-as-leaf?
;;             (lambda (node offset)
;;               (node-position-at-end? node offset)))
;; 
;;            (attach-as-leaf!
;;             (lambda (node label i)
;;               (debug "adding ~S as leaf off of ~A"
;;                      (label->string (sublabel label i))
;;                      ((current-node->id) node))
;;               (let ((leaf (node-add-leaf! node (sublabel label i))))
;;                 (debug "leaf ~A added" ((current-node->id) leaf))
;;                 node)))
;; 
;;            (splice-with-internal-node!
;;             (lambda (node offset label i)
;;               (debug "adding ~S within edge above ~A between ~S and ~S"
;;                      (label->string (sublabel label i))
;;                      ((current-node->id) node)
;;                      (label->string (sublabel (node-up-label node) 0 offset))
;;                      (label->string (sublabel (node-up-label node) offset)))
;;               ;; otherwise, extend by splicing
;;               (let-values (((split-node leaf)
;;                             (node-up-splice-leaf!
;;                              node offset (sublabel label i))))
;;                 (debug "spliced ~A with leaf ~A"
;;                        ((current-node->id) split-node)
;;                        ((current-node->id) leaf))
;;                 split-node)))
;;            ]
;;     main-logic))
;; 
;; 
;; 
;; 
;; 
;; (provide suffix-tree-add!)
;; ;; suffix-tree-add!: tree label -> void
;; ;; Adds a new label and its suffixes to the suffix tree.
;; ;; Precondition: label is nonempty.
;; (define suffix-tree-add!
;;   (letrec
;;       [
;;        (do-construction!
;;         (lambda (tree label)
;;           (debug "Starting construction for ~S" (label->string label))
;;           (debug "Root node is ~A"
;;                  ((current-node->id) (suffix-tree-root tree)))
;;           (let-values (((starting-node starting-offset)
;;                         (add-first-suffix! tree label)))
;;             (add-rest-suffixes! label starting-node starting-offset)
;;             (debug "finished construction"))))
;;        
;;        (add-first-suffix!
;;         (let
;;             [
;;              (matched-at-node
;;               (lambda (node)
;;                 (report-implicit-tree-constructed)))
;;              (matched-in-node
;;               (lambda (node offset)
;;                 (report-implicit-tree-constructed)))
;;              (mismatched-at-node
;;               (lambda (node label label-offset)
;;                 (let ((leaf (node-add-leaf!
;;                              node (sublabel label label-offset))))
;;                   (debug "adding leaf ~A with label ~S"
;;                          ((current-node->id) leaf)
;;                          (label->string (node-up-label leaf)))
;;                   (values node label-offset))))
;;              (mismatched-in-node
;;               (lambda (node offset label label-offset)
;;                 (let-values (((joint leaf)
;;                               (node-up-splice-leaf!
;;                                node offset
;;                                (sublabel label label-offset))))
;;                   (debug "spliced leaf ~A with label ~S"
;;                          ((current-node->id) leaf)
;;                          (label->string (node-up-label leaf)))
;;                   (values joint label-offset))))
;;              ]
;;           (lambda (tree label)
;;             (node-follow/k
;;              (suffix-tree-root tree) label
;;              matched-at-node
;;              matched-in-node
;;              mismatched-at-node
;;              mismatched-in-node))))
;; 
;;        (add-rest-suffixes!
;;         (lambda (label starting-node starting-offset)
;;           (add-rest-suffixes-loop!
;;            label
;;            (label-length label)
;;            (max starting-offset 1)
;;            1
;;            starting-node)))
;;        
;;        (add-rest-suffixes-loop!
;;         (lambda (label N i j active-node)
;;           (when (< j N)
;;             (debug "At node ~a (i=~a, j=~a)"
;;                    ((current-node->id) active-node) i j)
;;             (let-values (((next-extension-node next-extension-offset i*)
;;                           (find-next-extension-point/add-suffix-link!
;;                            active-node label i j)))
;;               (if i*
;;                   (begin
;;                     (let ((new-active-node
;;                            (extend-at-point! next-extension-node
;;                                              next-extension-offset
;;                                              label i*)))
;;                       (try-to-set-suffix-edge! active-node new-active-node)
;;                       (add-rest-suffixes-loop!
;;                        label N
;;                        (max i* (add1 j)) (add1 j) new-active-node)))
;;                   (begin
;;                     (report-implicit-tree-constructed)))))))
;; 
;;        
;;        (report-implicit-tree-constructed
;;         (lambda ()
;;           (debug "Implicit tree constructed")
;;           (void)))
;;        ]
;;     
;;     do-construction!))
;; 
;; ;; -- from suffixtree.rkt
;; 
;; (provide tree-add!)
;; (define tree-add! suffix-tree-add!)
;; 
;; 
