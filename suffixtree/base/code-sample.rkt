#lang racket/base

(provide
  (struct-out label)
  (struct-out suffix-tree)
  (struct-out node))


(define-struct label (datum i j) #:mutable)

;; A suffix tree consists of a root node.
(define-struct suffix-tree (root))

;; up-label: label
;; parent: (union #f node)
;; children: (listof node)
;; suffix-link: (union #f node)
(define-struct node (up-label parent children suffix-link) #:mutable)
#lang racket/base

(require
  "data.rkt")
;; Label implementation.  Labels are like strings, but also allow for
;; efficient shared slicing.
;;
;; TODO later: see if we can generalize labels to be more than just
;; text strings.  It might be useful to have them as arbitrary
;; elements, perhaps as vectors?

;; FIXME: set custom writer for labels to be displayed for debugging
;; purposes.
;; (http://download.plt-scheme.org/doc/299.400/html/mzscheme/mzscheme-Z-H-11.html#node_sec_11.2.10)


;; label-element? object -> true
;; Every value is considered to be a possible label-element.
(define (label-element? obj) #t)

;; When comparing label elements, we use equal?.
;;
(define label-element-equal? equal?)


(provide
         (rename-out [ext:make-label make-label])
         label-element?
         label-element-equal?
         string->label
         string->label/with-sentinel
         vector->label
         vector->label/with-sentinel
         label->string
         label->string/removing-sentinel
         label->vector
         label-length
         label-ref
         sublabel
         sublabel!
         label-prefix?
         label-equal?
         label-empty?
         label-copy
         label-ref-at-end?
         label-source-id
         label-same-source?)


;; make-label: label-element -> label
;; Constructs a new label from either a string or a vector of things.
(define (ext:make-label label-element)
  (cond ((string? label-element) (string->label label-element))
        ((vector? label-element) (vector->label label-element))
        (else
         (error 'make-label "Don't know how to make label from ~S" label-element))))


(define (make-sentinel)
  (gensym 'sentinel))

(define (sentinel? datum)
  (symbol? datum))

;; vector->label vector
;; Constructs a new label from the input vector.
(define (vector->label vector)
  (make-label (vector->immutable-vector vector)
              0 (vector-length vector)))


;; vector->label vector
;; Constructs a new label from the input vector, with a sentinel
;; symbol at the end.
(define (vector->label/with-sentinel vector)
  (let* ((N (vector-length vector))
         (V (make-vector (add1 N))))
    (vector-set! V N (make-sentinel))
    (let loop ((i 0))
      (if (< i N)
          (begin (vector-set! V i (vector-ref vector i))
                 (loop (add1 i)))
          (vector->label V)))))


;; string->label: string -> label
;; Constructs a new label from the input string.
(define string->label
  (let ((f (compose vector->label list->vector string->list)))
    (lambda (str) (f str))))


;; string->label/with-sentinel: string -> label
;; Constructs a new label from the input string, attaching a unique
;; sentinel symbol at the end of the label.
;;
;; Note: this label can not be converted in whole back to a string:
;; the sentinel character interferes with string concatenation
(define string->label/with-sentinel
  (let ((f (compose vector->label/with-sentinel list->vector string->list)))
    (lambda (str) (f str))))


;; label-length: label -> number?
;; Returns the length of the label.
(define (label-length label)
  (- (label-j label) (label-i label)))


;; label-ref: label number? -> char
;; Returns the kth element in the label.
(define (label-ref label k)
  (vector-ref (label-datum label) (+ k (label-i label))))


;; sublabel: label number number -> label
;; Gets a slice of the label on the half-open interval [i, j)
(define sublabel
  (case-lambda
    ((label i)
     (sublabel label i (label-length label)))
    ((label i j)
     (unless (<= i j)
       (error 'sublabel "illegal sublabel [~a, ~a]" i j))
     (make-label (label-datum label)
                 (+ i (label-i label))
                 (+ j (label-i label))))))


;; sublabel!: label number number -> void
;; destructively sets the input label to sublabel.
(define sublabel!
  (case-lambda
    ((label i)
     (sublabel! label i (label-length label)))
    ((label i j)
     (begin
       ;; order dependent code ahead!
       (set-label-j! label (+ j (label-i label)))
       (set-label-i! label (+ i (label-i label)))
       (void)))))


;; label-prefix?: label label -> boolean
;; Returns true if the first label is a prefix of the second label
(define (label-prefix? prefix other-label)
  (let ((m (label-length prefix))
        (n (label-length other-label)))
    (if (> m n)                       ; <- optimization: prefixes
					; can't be longer.
        #f
        (let loop ((k 0))
          (if (= k m)
              #t
              (and (equal? (label-ref prefix k) (label-ref other-label k))
                   (loop (add1 k))))))))


;; label-equal?: label label -> boolean
;; Returns true if the two labels are equal.
(define (label-equal? l1 l2)
  (and (= (label-length l1) (label-length l2))
       (label-prefix? l1 l2)))


;; label-empty?: label -> boolean
;; Returns true if the label is considered empty
(define (label-empty? label)
  (>= (label-i label) (label-j label)))


;; label->string: label -> string
;; Extracts the string that the label represents.
;; Precondition: the label must have originally come from a string.
;; Note: this operation is expensive: don't use it except for debugging.
(define (label->string label)
  (list->string (vector->list (label->vector label))))


(define (label->string/removing-sentinel label)
  (let* ([ln (label-length label)]
         [N (if (and (> ln 0) (sentinel? (label-ref label (sub1 ln))))
                (sub1 ln)
                ln)])
    (build-string N (lambda (i) (label-ref label i)))))


;; label->vector: label -> vector
;; Extracts the vector that the label represents.
;; Note: this operation is expensive: don't use it except for debugging.
(define (label->vector label)
  (let* ((N (label-length label))
         (buffer (make-vector N)))
    (let loop ((i 0))
      (if (< i N)
          (begin
            (vector-set! buffer i (label-ref label i))
            (loop (add1 i)))
          (vector->immutable-vector buffer)))))


;; label-copy: label->label
;; Returns a copy of the label.
(define (label-copy label)
  (make-label (label-datum label) (label-i label) (label-j label)))


;; label-ref-at-end?: label number -> boolean
(define (label-ref-at-end? label offset)
  (= offset (label-length label)))


;; label-source-id: label -> number
(define (label-source-id label)
  (eq-hash-code (label-datum label)))

;; label-same-source?: label label -> boolean
(define (label-same-source? label-1 label-2)
  (eq? (label-datum label-1) (label-datum label-2)))

;; --- from suffixtree.rkt
(provide label-source-eq?)
(define label-source-eq? label-same-source?)
#lang racket/base
;; Some utilities.

(require
 (except-in "data.rkt" make-label)
 "label.rkt"
 "structs.rkt"
 "ukkonen.rkt")

(define false-thunk (lambda () #f))


;; longest-common-substring: string string -> string
;; Returns the longest common substring between the two strings.
(provide longest-common-substring)
(define (longest-common-substring s1 s2)
  (label->string (longest-common-sublabel (string->label/with-sentinel s1)
                                          (string->label/with-sentinel s2))))


;; longest-common-sublabel: label label -> label
;;
;; Naive use of suffix trees to find longest common sublabel between
;; two labels.  Note that there's a better way to do this with
;; matching statistics: I'll try using matching statistics as soon
;; as I get this version running.
;;
;; This approach simply adds both labels to a common suffix tree,
;; does a postorder traversal to mark up the inner nodes, and then
;; finds the inner node with the deepest string depth.
(provide longest-common-sublabel)
(define (longest-common-sublabel label-1 label-2)
  (let ((label-1-marks (make-hasheq))
        (label-2-marks (make-hasheq))
        (deepest-node (node (make-label "no lcs") #f '() #f))
        (deepest-depth 0))
    (letrec
        [
         (main
          (lambda ()
            (let ((tree (make-tree)))
              (tree-add! tree label-1)
              (tree-add! tree label-2)
              (mark-up-inner-nodes! (tree-root tree) 0)
              (path-label deepest-node))))

         (mark-up-inner-nodes!
          (lambda (node depth)
            (if (null? (node-children node))
                (begin (when (label-source-eq? (node-up-label node) label-1)
                         (mark-with-label-1! node))
                       (when (label-source-eq? (node-up-label node) label-2)
                         (mark-with-label-2! node)))
                (begin (for-each
                        (lambda (child)
                          (mark-up-inner-nodes!
                           child
                           (+ depth (label-length (node-up-label child)))))
                        (node-children node))
                       (absorb-children-marks! node depth)))))

         (mark-with-label-1!
          (lambda (node)
            (hash-set! label-1-marks node #t)))

         (mark-with-label-2!
          (lambda (node)
            (hash-set! label-2-marks node #t)))

         (marked-by-label-1?
          (lambda (node)
            (hash-ref label-1-marks node false-thunk)))

         (marked-by-label-2?
          (lambda (node)
            (hash-ref label-2-marks node false-thunk)))
         
         (marked-by-both?
          (lambda (node)
            (and (marked-by-label-1? node)
                 (marked-by-label-2? node))))
         
         (absorb-children-marks!
          (lambda (node depth)
            (let/ec escape
              (for-each (lambda (child)
                          (when (marked-by-label-1? child)
                            (mark-with-label-1! node))
                          (when (marked-by-label-2? child)
                            (mark-with-label-2! node))
                          (when (marked-by-both? node)
                            (escape)))
                        (node-children node)))
            (when (and (marked-by-both? node)
                       (> depth deepest-depth))
              (set! deepest-depth depth)
              (set! deepest-node node))))
         ]
      
      (if (or (= 0 (label-length label-1))
              (= 0 (label-length label-2)))
          (string->label "")
          (begin
            (main))))))



;; path-label: node -> label
;;
;; Returns a new label that represents the path from the tree root
;; to this node.
;;
;; Fixme: optimize the representation of label to be able to do this
;; without much reallocation.  Maybe another label class that uses a
;; rope data structure might be better...  I need to read Hans
;; Boehm's paper on "Ropes, an alternative to strings" to see how
;; much work this would be.
(provide path-label)
(define path-label
  (letrec
      [(collect-loop
        (lambda (current-node collected-labels total-length)
          (if current-node
              (collect-loop (node-parent current-node)
                            (cons (node-up-label current-node)
                                  collected-labels)
                            (+ total-length
                               (label-length (node-up-label current-node))))
              (build-new-label collected-labels total-length))))

       (vector-blit!
        (lambda (src-label dest-vector dest-offset)
          (let loop ((i 0))
            (when (< i (label-length src-label))
              (vector-set! dest-vector
                           (+ i dest-offset)
                           (label-ref src-label i))
              (loop (add1 i))))))
       
       (build-new-label
        (lambda (labels total-length)
          (let ((vector (make-vector total-length)))
            (let loop ((labels labels)
                       (i 0))
              (if (null? labels)
                  (begin
                    (vector->label vector))
                  (begin
                    (vector-blit! (car labels) vector i)
                    (loop (cdr labels)
                          (+ i (label-length (car labels))))))))))]
    (lambda (node)
      (collect-loop node '() 0))))

#lang racket/base

(require benchmark-util
         (only-in racket/file file->lines file->string))

(require "lcs.rkt")

(define LARGE_TEST "../base/prufock.txt")
(define SMALL_TEST "../base/hunt.txt")

(define (self-test)
  (define txt (file->string "../base/code-sample.rkt"))
  (longest-common-substring txt txt)
  (void))

(define (main testfile)
  (define lines (file->lines testfile))
  (for* ([a lines] [b lines])
    (longest-common-substring a b))
  (void))

;(time (main SMALL_TEST)) ;150ms
;(time (main LARGE_TEST)) ; 2500ms
(time (self-test)) ; 170ms
#lang racket/base
(require
         racket/list)

(require "label.rkt"
  (except-in "data.rkt" make-label))

(provide
 tree?
 make-tree
 tree-root
 new-suffix-tree
 node-find-child
 node-root?
 node-suffix-link
 set-node-suffix-link!
 node-position-at-end?
 node-add-leaf!
 node-up-splice-leaf!
 node-follow/k)


;; new-suffix-tree: void -> suffix-tree
;; Builds a new empty suffix-tree.
(define (new-suffix-tree)
  (make-suffix-tree
   ;; The root node has no label, no parent, an empty list of
   ;; children.  Its suffix link is invalid, but we set it to #f.
   (let ((root (make-node (make-label (make-vector 0)) #f (list) #f)))
     root)))


(define (node-root? node)
  (eq? #f (node-parent node)))


;; node-add-leaf!: node label -> node
;; Attaches a new leaf node to an internal node.  Returns thew new leaf.
(define (node-add-leaf! node label)
  (let ((leaf (make-node label node (list) #f)))
    (node-add-child! node leaf)
    leaf))


;; node-add-child!: node node -> void
;; Adds to the node list.
(define (node-add-child! node child)
  (set-node-children! node (cons child (node-children node))))


;; node-remove-child!: node node -> void
;; Removes child node.
(define (node-remove-child! node child)
  (set-node-children! node (remq child (node-children node))))


;; Constructor for children is a list at the moment.  TODO: change
;; children representation based on density?
(define children-list list)


;; node-leaf?: node -> boolean
;; Returns true if node is a leaf
(define (node-leaf? node)
  (empty? (node-children node)))


;; node-find-child: node label-element -> (union node #f)
;;
;; Finds the first child node whose up-label starts with (label-ref
;; label 0).  If none can be found, returns #f.
(define (node-find-child node label-element)
  (define (loop children)
    (cond ((null? children) #f)
          ((label-element-equal? label-element (label-ref (node-up-label (first children)) 0))
           (first children))
          (else
           (loop (rest children)))))
  (loop (node-children node)))


;; node-up-split!: node number -> node
;; Introduces a new node that goes between this node and its parent.
(define (node-up-split! node offset)
  (let* ((label (node-up-label node))
         (pre-label (sublabel label 0 offset))
         (post-label (sublabel label offset))
         (parent (node-parent node))
         (new-node (make-node pre-label parent (children-list node) #f)))
    (set-node-up-label! node post-label)
    (node-remove-child! parent node)
    (set-node-parent! node new-node)
    (node-add-child! parent new-node)
    new-node))


;; node-up-splice-leaf!: node offset label -> (values node node)
;;
;; Adds a new leaf at a splice joint between the node and its
;; parent.  Returns both the joint and the leaf.
(define (node-up-splice-leaf! node offset leaf-label)
  (let* ((split-node (node-up-split! node offset))
         (leaf (node-add-leaf! split-node leaf-label)))
    (values split-node leaf)))


;; tree-contains?: tree label -> boolean
;; Returns true if the tree contains the given label.
(define (tree-contains? tree label)
  (node-follow/k (suffix-tree-root tree)
                 label
                 (lambda args #t)
                 (lambda args #t)
                 (lambda args #f)
                 (lambda args #f)))

;; node-follow/k: node label (node -> A)
;;                           (node number -> B)
;;                           (node label number -> C)
;;                           (node number label number -> D)
;;                    -> (union A B C D)
;; 
;; Traverses the node's edges along the elements of the input label.
;; Written in continuation-passing-style for leakage containment.
;; One of the four continuation arguments will be executed.
(define node-follow/k
  (lambda (node original-label
                matched-at-node/k
                matched-in-edge/k
                mismatched-at-node/k
                mismatched-in-edge/k)
    (letrec
        ((EDGE/k
          ;; follows an edge
          (lambda (node label label-offset)
            (let ((up-label (node-up-label node)))
              (let loop ((k 0))
                (cond
                 ((= k (label-length up-label))
                  (NODE/k node label (+ label-offset k)))
                 ((= (+ label-offset k) (label-length label))
                  (matched-in-edge/k node k))
                 ((label-element-equal? (label-ref up-label k)
                                        (label-ref label (+ k label-offset)))
                  (loop (add1 k)))
                 (else
                  (mismatched-in-edge/k node k label
                                        (+ k label-offset))))))))
         ;; follows a node
         (NODE/k
          (lambda (node label label-offset)
            (if (= (label-length label) label-offset)
                (matched-at-node/k node)
                (let ((child (node-find-child
                              node
                              (label-ref label label-offset))))
                  (if child
                      (EDGE/k child label label-offset)
                      (mismatched-at-node/k node label label-offset)))))))
      (NODE/k node (label-copy original-label) 0))))


;; node-position-at-end?: node number -> boolean
;;
;; Returns true if the position defined by node and the up-label
;; offset are pointing at the end of the node.
(define (node-position-at-end? node offset)
  (label-ref-at-end? (node-up-label node) offset))

;; --- from suffixtree.rkt

(define tree? suffix-tree?)

(define make-tree new-suffix-tree)

(define tree-root suffix-tree-root)

#lang racket/base

;; Much of this comes from reading Dan Gusfield's Algorithms on
;; strings, trees, and sequences: computer science and computational
;; biology.

(require "structs.rkt"
  (except-in "data.rkt" make-label)
  "label.rkt")

(define dummy-node (node (make-label "dummy") #f '() #f))


(provide skip-count)
;; skip-count: node label -> (values node number)
;;
;; Follows down the node using the skip-count rule until we exhaust
;; the label.  Assumes that there does exist a labeled path starting
;; from the node that exactly matches label.
(define skip-count
  (lambda (node label)
    (skip-count-helper node label 0 (label-length label))))

;; Utility function for skip count, but also visible for those in
;; the know to skip-count from an arbitrary position in label.
(define (skip-count-helper node label k N)
  (define (loop node k)
    (let* ((child (node-find-child node (label-ref label k)))
           (child-label (node-up-label child))
           (child-label-length (label-length child-label))
           (rest-of-chars-left-to-skip (- N k)))
      (if (> rest-of-chars-left-to-skip child-label-length)
          (loop child
                (+ k child-label-length))
          (values child rest-of-chars-left-to-skip))))
  (if (>= k N)
      (values node (label-length (node-up-label node)))
      (loop node k)))






(provide jump-to-suffix)
;; jump-to-suffix: node -> (values node (union boolean number))
;;
;; Given an internal node, jumps to the suffix from that node.
;; According to the theory of suffix trees, such a node will exist
;; in the tree if we follow the Ukkonen construction.  If we had to
;; go up a few characters, returns the number of chars at the suffix
;; end that need to be compared to get the real suffix.

;; If we hit the root, that offset is #f to indicate that we have to
;; start searching the suffix from scratch.
(define (jump-to-suffix node)
  (define PARENT (node-parent node))
  (cond ((node-root? node)
         (values node #f))
        ((node-suffix-link node)
         (begin 
                (let ([node2 (node-suffix-link node)])
                  (unless node2 (error "jump to suffix"))
                  (values node2 0))))
        ((and PARENT (node-root? PARENT))
           (values PARENT #f))
        (else
         (let* ([parent (node-parent node)]
                [sl (begin (unless parent (error "j2s"))
                           (node-suffix-link parent))])
           (unless sl (error "j2s whoahao"))
           (values sl
                   (label-length (node-up-label node)))))))

(provide try-to-set-suffix-edge!)
;; try-to-set-suffix-edge!: node node -> void
;;
;; Sets the suffix edge of from-node directed to to-node if it
;; hasn't been set yet.
(define (try-to-set-suffix-edge! from-node to-node)
  (when (not (node-suffix-link from-node))
    
    (set-node-suffix-link! from-node to-node)))



(provide find-next-extension-point/add-suffix-link!)
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
(define (find-next-extension-point/add-suffix-link! node label initial-i j)
  (define (fixed-start suffix-offset)
    (if suffix-offset (- initial-i suffix-offset) j))

  (let*-values
      (((suffix-node suffix-offset) (jump-to-suffix node))
       ((K N) (values (fixed-start suffix-offset)
                      (label-length label))))
    (letrec
        [
         (loop-first
          (lambda (i)
            (loop-general i (lambda (skipped-node skip-offset)
                              (when (node-position-at-end?
                                     skipped-node skip-offset)
                                (try-to-set-suffix-edge!
                                 node skipped-node))))))
         
         (loop-rest
          (lambda (i)
            (loop-general i (lambda (skipped-node skip-offset)
                              (void)))))

         (loop-general
          (lambda (i first-shot)
            (if (>= i N)
                (values #f #f #f)
                (let-values
                    (((skipped-node skipped-offset)
                      (skip-count-helper suffix-node label K i)))
                  (first-shot skipped-node skipped-offset)
                  (if (node-position-at-end? skipped-node skipped-offset)
                      (find-extension-at-end!
                       skipped-node skipped-offset i)
                      (find-extension-in-edge
                       skipped-node skipped-offset i))))))           

         (find-extension-in-edge
          (lambda (skipped-node skip-offset i)
            (if (label-element-equal?
                 (label-ref label i)
                 (label-ref (node-up-label skipped-node)
                            skip-offset))
                (loop-rest (add1 i))
                (values skipped-node skip-offset i))))

         (find-extension-at-end!
          (lambda (skipped-node skip-offset i)
            (if (node-find-child skipped-node (label-ref label i))
                (loop-rest (add1 i))
                (values skipped-node skip-offset i))))
         ]
      (loop-first initial-i))))


(provide extend-at-point!)
;; extend-at-point!: node number label number -> node
(define extend-at-point!
  (letrec [
           (main-logic
            (lambda (node offset label i)
              (if (should-extend-as-leaf? node offset)
                  (attach-as-leaf! node label i)
                  (splice-with-internal-node! node offset label i))))

           (should-extend-as-leaf?
            (lambda (node offset)
              (node-position-at-end? node offset)))

           (attach-as-leaf!
            (lambda (node label i)
              (let ((leaf (node-add-leaf! node (sublabel label i))))
                
                node)))

           (splice-with-internal-node!
            (lambda (node offset label i)
              ;; otherwise, extend by splicing
              (let-values (((split-node leaf)
                            (node-up-splice-leaf!
                             node offset (sublabel label i))))
                split-node)))
           ]
    main-logic))





(provide suffix-tree-add!)
;; suffix-tree-add!: tree label -> void
;; Adds a new label and its suffixes to the suffix tree.
;; Precondition: label is nonempty.
(define suffix-tree-add!
  (letrec
      [
       (do-construction!
        (lambda (tree label)
          (let* ((pair (add-first-suffix! tree label))
                 (starting-node (car pair))
                 (starting-offset (cdr pair)))
            (add-rest-suffixes! label starting-node starting-offset)
            )))
       
       (add-first-suffix!
        (let
            [
             (matched-at-node
              (lambda (node)
                (report-implicit-tree-constructed)))
             (matched-in-node
              (lambda (node offset)
                (report-implicit-tree-constructed)))
             (mismatched-at-node
              (lambda (node label label-offset)
                (let ((leaf (node-add-leaf!
                             node (sublabel label label-offset))))
                  (cons node label-offset))))
             (mismatched-in-node
              (lambda (node offset label label-offset)
                (let-values (((joint leaf)
                              (node-up-splice-leaf!
                               node offset
                               (sublabel label label-offset))))
                  (cons joint label-offset))))
             ]
          (lambda (tree label)
            (node-follow/k
             (suffix-tree-root tree) label
             matched-at-node
             matched-in-node
             mismatched-at-node
             mismatched-in-node))))

       (add-rest-suffixes!
        (lambda (label starting-node starting-offset)
          (add-rest-suffixes-loop!
           label
           (label-length label)
           (max starting-offset 1)
           1
           starting-node)))
       
       (add-rest-suffixes-loop!
        (lambda (label N i j active-node)
          (when (< j N)
            (let-values (((next-extension-node next-extension-offset i*)
                          (find-next-extension-point/add-suffix-link!
                           active-node label i j)))
              (if i*
                  (begin
                    (let ((new-active-node
                           (extend-at-point! next-extension-node
                                             next-extension-offset
                                             label i*)))
                      (try-to-set-suffix-edge! active-node new-active-node)
                      (add-rest-suffixes-loop!
                       label N
                       (max i* (add1 j)) (add1 j) new-active-node)))
                  (begin
                    (report-implicit-tree-constructed)
                    (void)))))))

       
       (report-implicit-tree-constructed
        (lambda ()
          (cons dummy-node 0)))
       ]
    
    do-construction!))

;; -- from suffixtree.rkt

(provide tree-add!)
(define tree-add! suffix-tree-add!)


#lang typed/racket/base

(provide
  (struct-out label)
  (struct-out suffix-tree)
  (struct-out node))


(define-struct label ([datum : (Vectorof (U Char Symbol))] [i : Natural] [j : Natural]) #:mutable)

;; A suffix tree consists of a root node.
(define-struct suffix-tree ([root : node]))

;; up-label: label
;; parent: (union #f node)
;; children: (listof node)
;; suffix-link: (union #f node)
(define-struct node ([up-label : label] [parent : (U #f node)] [children : (Listof node)] [suffix-link : (U #f node)]) #:mutable)
#lang typed/racket/base

(require "typed-data.rkt")

;; Label implementation.  Labels are like strings, but also allow for
;; efficient shared slicing.
;;
;; TODO later: see if we can generalize labels to be more than just
;; text strings.  It might be useful to have them as arbitrary
;; elements, perhaps as vectors?

;; label-element? object -> true
;; Every value is considered to be a possible label-element.
(: label-element? (-> Any Boolean))
(define (label-element? obj) #t)

;; When comparing label elements, we use equal?.
(: label-element-equal? (-> Any Any Boolean))
(define label-element-equal? equal?)

(provide
        (rename-out [ext:make-label make-label])
        label-element?
        label-element-equal?
        string->label
        string->label/with-sentinel
        vector->label
        vector->label/with-sentinel
        label->string
        label->string/removing-sentinel
        label->vector
        label-length
        label-ref
        sublabel
        sublabel!
        label-prefix?
        label-equal?
        label-empty?
        label-copy
        label-ref-at-end?
        label-source-id
        label-same-source?
        label-source-eq?)


;;make-label: label-element -> label
;;Constructs a new label from either a string or a vector of things.
(: ext:make-label (-> (U String (Vectorof (U Char Symbol))) Label))
(define (ext:make-label label-element)
 (cond ((string? label-element) (string->label label-element))
       ((vector? label-element) (vector->label label-element))
       (else
        (error 'make-label "Don't know how to make label from ~S" label-element))))

(: make-sentinel (-> Symbol))
(define (make-sentinel)
 (gensym 'sentinel))

(: sentinel? (-> Any Boolean))
(define (sentinel? datum)
 (symbol? datum))

;; vector->label vector
;; Constructs a new label from the input vector.
(: vector->label (-> (Vectorof (U Char Symbol)) label))
(define (vector->label vector)
  (make-label (vector->immutable-vector vector)
              0 (vector-length vector)))


;; vector->label vector
;; Constructs a new label from the input vector, with a sentinel
;; symbol at the end.
(: vector->label/with-sentinel (-> (Vectorof Char) label))
(define (vector->label/with-sentinel vector)
  (: N Index)
  (define N (vector-length vector))
  (: V (Vectorof (U Char Symbol)))
  (define V (make-vector (add1 N) (make-sentinel)))
  (let loop ((i 0))
    (if (< i N)
        (begin (vector-set! V i (vector-ref vector i))
               (loop (add1 i)))
        (vector->label V))))


;;string->label: string -> label
;;Constructs a new label from the input string.
(: string->label (-> String label))
(define (string->label str)
  (vector->label (list->vector (string->list str))))


;; string->label/with-sentinel: string -> label
;; Constructs a new label from the input string, attaching a unique
;; sentinel symbol at the end of the label.
;;
;; Note: this label can not be converted in whole back to a string:
;; the sentinel character interferes with string concatenation
(: string->label/with-sentinel (-> String label))
(define (string->label/with-sentinel str)
  (vector->label/with-sentinel (list->vector (string->list str))))

;; label-length: label -> number?
;; Returns the length of the label.
(: label-length (-> label Index))
(define (label-length label)
  (define len (- (label-j label) (label-i label)))
  (unless (index? len) (error "label-length"))
  len)


; label-ref: label number? -> char
; Returns the kth element in the label.
(: label-ref (-> label Integer (U Symbol Char)))
(define (label-ref label k)
  (unless (index? k) (error "label ref INDEX"))
  (vector-ref (label-datum label) (+ k (label-i label))))

;; sublabel: label number number -> label
;; Gets a slice of the label on the half-open interval [i, j)
(: sublabel (case-> (-> label Index label)
                    (-> label Index Index label)))
(define sublabel
  (case-lambda
    ((label i)
     (sublabel label i (label-length label)))
    ((label i j)
     (unless (<= i j)
       (error 'sublabel "illegal sublabel [~a, ~a]" i j))
     (make-label (label-datum label)
                 (+ i (label-i label))
                 (+ j (label-i label))))))

;; sublabel!: label number number -> void
;; destructively sets the input label to sublabel.
(: sublabel! (case-> (-> label Index Void)
                     (-> label Index Index Void)))
(define sublabel!
  (case-lambda
    ((label i)
     (sublabel! label i (label-length label)))
    ((label i j)
     (begin
       ;; order dependent code ahead!
       (set-label-j! label (+ j (label-i label)))
       (set-label-i! label (+ i (label-i label)))
       (void)))))


;; label-prefix?: label label -> boolean
;; Returns true if the first label is a prefix of the second label
(: label-prefix? (-> label label Boolean))
(define (label-prefix? prefix other-label)
  (let ((m (label-length prefix))
        (n (label-length other-label)))
    (if (> m n)                       ; <- optimization: prefixes
					; can't be longer.
        #f
        (let loop ((k 0))
          (if (= k m)
              #t
              (and (index? k)
                   (equal? (label-ref prefix k) (label-ref other-label k))
                   (loop (add1 k))))))))


;; label-equal?: label label -> boolean
;; Returns true if the two labels are equal.
(: label-equal? (-> label label Boolean))
(define (label-equal? l1 l2)
  (and (= (label-length l1) (label-length l2))
       (label-prefix? l1 l2)))


;; label-empty?: label -> boolean
;; Returns true if the label is considered empty
(: label-empty? (-> label Boolean))
(define (label-empty? label)
  (>= (label-i label) (label-j label)))


;; label->string: label -> string
;; Extracts the string that the label represents.
;; Precondition: the label must have originally come from a string.
;; Note: this operation is expensive: don't use it except for debugging.
(: label->string (-> label String))
(define (label->string label)
  (: V (Vectorof (U Char Symbol)))
  (define V (label->vector label))
  (: L (Listof Char))
  (define L (for/list : (Listof Char)
                      ([c : (U Char Symbol) (in-vector V)])
              (unless (char? c) (error "label->string invariant broken"))
              c))
  (list->string L))

(: label->string/removing-sentinel (-> label String))
(define (label->string/removing-sentinel label)
  (let* ([ln (label-length label)]
         [N (if (and (> ln 0) (sentinel? (label-ref label (sub1 ln))))
                (sub1 ln)
                ln)])
    (build-string N (lambda ([i : Integer])
                      (unless (index? i) (error "label->string 1"))
                      (let ([val (label-ref label i)])
                        (unless (char? val) (error "label->string 2"))
                        val)))))

;; label->vector: label -> vector
;; Extracts the vector that the label represents.
;; Note: this operation is expensive: don't use it except for debugging.
(: label->vector (-> label (Vectorof (U Char Symbol))))
(define (label->vector label)
  (: N Integer)
  (define N (label-length label))
  (: buffer (Vectorof (U Char Symbol)))
  (define buffer (make-vector N 'X));;'X is a placeholder
    (let loop ((i 0))
      (if (and (< i N) (index? i))
          (begin
            (vector-set! buffer i (label-ref label i))
           (loop (add1 i)))
          (vector->immutable-vector buffer))))


;; label-copy: label->label
;; Returns a copy of the label.
(: label-copy (-> label label))
(define (label-copy label)
  (make-label (label-datum label) (label-i label) (label-j label)))


;; label-ref-at-end?: label number -> boolean
(: label-ref-at-end? (-> label Integer Boolean))
(define (label-ref-at-end? label offset)
  (= offset (label-length label)))


;; label-source-id: label -> number
(: label-source-id (-> label Integer))
(define (label-source-id label)
  (eq-hash-code (label-datum label)))

;; label-same-source?: label label -> boolean
(: label-same-source? (-> label label Boolean))
(define (label-same-source? label-1 label-2)
  (eq? (label-datum label-1) (label-datum label-2)))

;; --- from suffixtree.rkt
(define label-source-eq? label-same-source?)
#lang typed/racket/base
;; Some utilities.

(require benchmark-util
         (except-in "typed-data.rkt" make-label)
         racket/list)

(require/typed/check "label.rkt"
 [label->string (-> Label String)]
 [string->label (-> String Label)]
 [string->label/with-sentinel (-> String Label)]
 [make-label (-> (U String (Vectorof (U Char Symbol))) Label)]
 [label-source-eq? (-> Label Label Boolean)]
 [label-length (-> Label Index)]
 [vector->label (-> (Vectorof (U Char Symbol)) Label)]
 [label-ref (-> Label Integer (U Symbol Char))])

(require/typed/check
 "structs.rkt"
 [make-tree (-> Tree)]
 [tree-root (-> Tree Node)]
 )

(require/typed/check "ukkonen.rkt"
 [tree-add! (-> Tree Label Void)])

(: false-thunk (-> #f))
(define false-thunk (lambda () #f))


;; longest-common-substring: string string -> string
;; Returns the longest common substring between the two strings.
(provide longest-common-substring)
(: longest-common-substring (-> String String String))
(define (longest-common-substring s1 s2)
  (label->string (longest-common-sublabel (string->label/with-sentinel s1)
                                          (string->label/with-sentinel s2))))

;; longest-common-sublabel: label label -> label
;;
;; Naive use of suffix trees to find longest common sublabel between
;; two labels.  Note that there's a better way to do this with
;; matching statistics: I'll try using matching statistics as soon
;; as I get this version running.
;;
;; This approach simply adds both labels to a common suffix tree,
;; does a postorder traversal to mark up the inner nodes, and then
;; finds the inner node with the deepest string depth.
(provide longest-common-sublabel)
(: longest-common-sublabel (-> Label Label Label))
(define (longest-common-sublabel label-1 label-2)
  (: label-1-marks (HashTable Node Boolean))
  (define label-1-marks (make-hasheq))
  (: label-2-marks (HashTable Node Boolean))
  (define label-2-marks (make-hasheq))
  (: deepest-node Node)
  (define deepest-node (node (make-label "no lcs") #f '() #f))
  (: deepest-depth Index)
  (define deepest-depth 0)
  (: main (-> Label))
  (define (main)
    (define tree (make-tree))
    (tree-add! tree label-1)
    (tree-add! tree label-2)
    (mark-up-inner-nodes! (tree-root tree) 0)
    (path-label deepest-node))
  (: mark-up-inner-nodes! (-> Node Index Void))
  (define (mark-up-inner-nodes! node depth)
    (cond [(null? (node-children node))
           (when (label-source-eq? (node-up-label node) label-1)
             (mark-with-label-1! node))
           (when (label-source-eq? (node-up-label node) label-2)
             (mark-with-label-2! node))]
          [else
            (for ([child (node-children node)])
              (let ([i (+ depth (label-length (node-up-label child)))])
                (unless (index? i) (error "NOOOOO"))
                (mark-up-inner-nodes! child i)))
            (absorb-children-marks! node depth)]))
  (: mark-with-label-1! (-> Node Void))
  (define (mark-with-label-1! node)
    (hash-set! label-1-marks node #t))
  (: mark-with-label-2! (-> Node Void))
  (define (mark-with-label-2! node)
    (hash-set! label-2-marks node #t))
  (: marked-by-label-1? (-> Node Boolean))
  (define (marked-by-label-1? node)
    (hash-ref label-1-marks node false-thunk))
  (: marked-by-label-2? (-> Node Boolean))
  (define (marked-by-label-2? node)
    (hash-ref label-2-marks node false-thunk))
  (: marked-by-both? (-> Node Boolean))
  (define (marked-by-both? node)
    (and (marked-by-label-1? node)
         (marked-by-label-2? node)))
  (: absorb-children-marks! (-> Node Index Void))
  (define (absorb-children-marks! node depth)
    ;(let/ec escape
      (for ([child (node-children node)])
        (when (marked-by-label-1? child)
          (mark-with-label-1! node))
        (when (marked-by-label-2? child)
          (mark-with-label-2! node)))
        ;(when (marked-by-both? node)
        ;  (escape))))
    (when (and (marked-by-both? node)
               (> depth deepest-depth))
      (set! deepest-depth depth)
      (set! deepest-node node)))
  (if (or (= 0 (label-length label-1))
          (= 0 (label-length label-2)))
      (string->label "")
      (main)))



;; path-label: node -> label
;;
;; Returns a new label that represents the path from the tree root
;; to this node.
;;
;; Fixme: optimize the representation of label to be able to do this
;; without much reallocation.  Maybe another label class that uses a
;; rope data structure might be better...  I need to read Hans
;; Boehm's paper on "Ropes, an alternative to strings" to see how
;; much work this would be.
(provide path-label)
(: path-label (-> Node Label))
(define (path-label node)
  (: collect-loop (-> (U Node #f) (Listof Label) Integer Label))
  (define (collect-loop current-node collected-labels total-length)
    (if current-node
      (collect-loop (node-parent current-node)
                    (cons (node-up-label current-node) collected-labels)
                    (+ total-length
                       (label-length (node-up-label current-node))))
      (build-new-label collected-labels total-length)))
  (: vector-blit! (-> Label (Vectorof (U Char Symbol)) Index Void))
  (define (vector-blit! src-label dest-vector dest-offset)
    (let loop ((i 0))
      (let ([index (+ i dest-offset)])
      (when (and (< i (label-length src-label)) (index? i) (index? index))
        (vector-set! dest-vector
                     index
                     (label-ref src-label i))
        (loop (add1 i))))))
  (: build-new-label (-> (Listof Label) Integer Label))
  (define (build-new-label labels total-length)
    (: vector (Vectorof (U Char Symbol)))
    (define vector (make-vector total-length 'X))
    (let loop ((labels labels) (i 0))
      (cond [(null? labels)
             (vector->label vector)]
            [(index? i)
              (vector-blit! (car labels) vector i)
              (loop (cdr labels)
                    (+ i (label-length (car labels))))]
            [else (error "not an index")])))
  (collect-loop node '() 0))

#lang typed/racket/base

(require benchmark-util
 (only-in racket/file file->lines))

(require/typed/check "lcs.rkt"
  [longest-common-substring (-> String String String)])

(define LARGE_TEST "../base/prufock.txt")
(define SMALL_TEST "../base/hunt.txt")

(define (main)
  (define lines (file->lines LARGE_TEST))
  (for* ([a lines] [b lines])
    (longest-common-substring a b))
  (void))

(time (main))
#lang typed/racket/base
(require benchmark-util
         (except-in "typed-data.rkt" make-label)
         racket/list)

(require/typed/check
 "label.rkt"
 [make-label (-> (U String (Vectorof (U Char Symbol))) Label)]
 [label-element-equal? (-> Any Any Boolean)]
 [label-length (-> Label Index)]
 [label-ref (-> Label Integer (U Symbol Char))]
 [sublabel (case-> (-> Label Index Label)
                    (-> Label Index Index Label))]
 [label-copy (-> Label Label)]
 [label-ref-at-end? (-> Label Integer Boolean)]
 [label->string (-> Label String)]
 [label-source-eq? (-> Label Label Boolean)]
 [string->label (-> String Label)]
 [string->label/with-sentinel (-> String Label)]
 [vector->label (-> (Vectorof (U Char Symbol)) Label)]
 [vector->label/with-sentinel (-> (Vectorof (U Char Symbol)) Label)]
 [label-same-source? (-> Label Label Boolean)]
 )

(provide
 tree?
 tree-root
 make-tree
 new-suffix-tree
 node-find-child
 node-root?
 node-position-at-end?
 node-add-leaf!
 node-up-splice-leaf!
 node-follow/k)


;; new-suffix-tree: void -> suffix-tree
;; Builds a new empty suffix-tree.
(: new-suffix-tree (-> Tree))
(define (new-suffix-tree)
  (make-suffix-tree
   ;; The root node has no label, no parent, an empty list of
   ;; children.  Its suffix link is invalid, but we set it to #f.
   (let ((root (make-node (make-label (make-vector 0 'X)) #f (list) #f)))
     root)))

(: node-root? (-> Node Boolean))
(define (node-root? node)
  (eq? #f (node-parent node)))


;; node-add-leaf!: node label -> node
;; Attaches a new leaf node to an internal node.  Returns thew new leaf.
(: node-add-leaf! (-> Node Label Node))
(define (node-add-leaf! node label)
  (let ((leaf (make-node label node (list) #f)))
    (node-add-child! node leaf)
    leaf))


;; node-add-child!: node node -> void
;; Adds to the node list.
(: node-add-child! (-> Node Node Void))
(define (node-add-child! node child)
  (set-node-children! node (cons child (node-children node))))


;; node-remove-child!: node node -> void
;; Removes child node.
(: node-remove-child!(-> Node Node Void))
(define (node-remove-child! node child)
  (set-node-children! node (remq child (node-children node))))


;; Constructor for children is a list at the moment.  TODO: change
;; children representation based on density?
(define children-list list)


;; node-leaf?: node -> boolean
;; Returns true if node is a leaf
(: node-leaf? (-> Node Boolean))
(define (node-leaf? node)
  (empty? (node-children node)))


;; node-find-child: node label-element -> (union node #f)
;;
;; Finds the first child node whose up-label starts with (label-ref
;; label 0).  If none can be found, returns #f.
(: node-find-child (-> Node Any (U Node #f)))
(define (node-find-child node label-element)
  (: loop (-> (Listof Node) (U Node #f)))
  (define (loop children)
    (cond ((null? children) #f)
          ((label-element-equal? label-element (label-ref (node-up-label (first children)) 0))
           (first children))
          (else
           (loop (rest children)))))
  (loop (node-children node)))


;; node-up-split!: node number -> node
;; Introduces a new node that goes between this node and its parent.
(: node-up-split! (-> Node Index Node))
(define (node-up-split! node offset)
  (let* ((label (node-up-label node))
         (pre-label (sublabel label 0 offset))
         (post-label (sublabel label offset))
         (parent (node-parent node))
         (new-node (make-node pre-label parent (children-list node) #f)))
    (set-node-up-label! node post-label)
    (unless parent (error "node-up-split!"))
    (node-remove-child! parent node)
    (set-node-parent! node new-node)
    (node-add-child! parent new-node)
    new-node))


;; node-up-splice-leaf!: node offset label -> (values node node)
;;
;; Adds a new leaf at a splice joint between the node and its
;; parent.  Returns both the joint and the leaf.
(: node-up-splice-leaf! (-> Node Index Label (values Node Node)))
(define (node-up-splice-leaf! node offset leaf-label)
  (let* ((split-node (node-up-split! node offset))
         (leaf (node-add-leaf! split-node leaf-label)))
    (values split-node leaf)))


;; tree-contains?: tree label -> boolean
;; Returns true if the tree contains the given label.
(: tree-contains? (-> Tree Label Boolean))
(define (tree-contains? tree label)
  (node-follow/k (suffix-tree-root tree)
                 label
                 (lambda args #t)
                 (lambda args #t)
                 (lambda args #f)
                 (lambda args #f)))


;; node-follow/k: node label (node -> A)
;;                           (node number -> B)
;;                           (node label number -> C)
;;                           (node number label number -> D)
;;                    -> (union A B C D)
;; 
;; Traverses the node's edges along the elements of the input label.
;; Written in continuation-passing-style for leakage containment.
;; One of the four continuation arguments will be executed.
(: node-follow/k (All (A B C D)
                      (-> Node
                          Label
                          (-> Node A)
                          (-> Node Index B)
                          (-> Node Label Index C)
                          (-> Node Index Label Index D)
                          (U A B C D))))
(define (node-follow/k node
                       original-label
                       matched-at-node/k
                       matched-in-edge/k
                       mismatched-at-node/k
                       mismatched-in-edge/k)
  (: EDGE/k (-> Node Label Index (U A B C D)))
  (define (EDGE/k node label label-offset)
    (: up-label Label)
    (define up-label (node-up-label node))
    (let loop ((k 0))
      (define k+label-offset (+ k label-offset))
      (cond
       ((= k (label-length up-label))
        (unless (index? k+label-offset) (error "node/folllowd"))
        (NODE/k node label k+label-offset))
       ((= k+label-offset (label-length label))
        (unless (index? k) (error "node/followk"))
        (matched-in-edge/k node k))
       ((label-element-equal? (label-ref up-label k)
                              (label-ref label k+label-offset))
        (loop (add1 k)))
       (else
        (unless (and (index? k)
                     (index? k+label-offset)) (error "node-follow/k mismatched fail"))
        (mismatched-in-edge/k node k label
                              k+label-offset)))))
  (: NODE/k (-> Node Label Index (U A B C D)))
  (define (NODE/k node label label-offset)
    (if (= (label-length label) label-offset)
        (matched-at-node/k node)
        (let ([child (node-find-child node (label-ref label label-offset))])
          (if child
              (EDGE/k child label label-offset)
              (mismatched-at-node/k node label label-offset)))))
  (NODE/k node (label-copy original-label) 0))


;; node-position-at-end?: node number -> boolean
;;
;; Returns true if the position defined by node and the up-label
;; offset are pointing at the end of the node.
(: node-position-at-end? (-> Node Index Boolean))
(define (node-position-at-end? node offset)
  (label-ref-at-end? (node-up-label node) offset))

;; --- from suffixtree.rkt

(define tree? suffix-tree?)

(define make-tree new-suffix-tree)

(define tree-root suffix-tree-root)

#lang typed/racket/base

;; Much of this comes from reading Dan Gusfield's Algorithms on
;; strings, trees, and sequences: computer science and computational
;; biology.

(require
 (except-in "typed-data.rkt" make-label)
 benchmark-util
)
(require/typed/check "label.rkt"
  [label-length (-> Label Index)]
  [label-ref (-> Label Integer (U Symbol Char))]
  [label->string (-> Label String)]
  [string->label (-> String Label)]
  [string->label/with-sentinel (-> String Label)]
  [label-element-equal? (-> Any Any Boolean)]
  [label-source-eq? (-> Label Label Boolean)]
  [vector->label (-> (Vectorof (U Char Symbol)) Label)]
  [make-label (-> (U String (Vectorof (U Char Symbol))) Label)]
  [sublabel (case-> (-> Label Index Label)
                    (-> Label Index Index Label))])

(require/typed/check "structs.rkt"
  [new-suffix-tree (-> Tree)]
  [node-find-child (-> Node Any (U Node #f))]
  [node-root? (-> Node Boolean)]
  [node-position-at-end? (-> Node Index Boolean)]
  [node-add-leaf! (-> Node Label Node)]
  [node-up-splice-leaf! (-> Node Index Label (values Node Node))]
  [node-follow/k (-> Node
                     Label
                     (-> Node (Pairof Node Index))
                     (-> Node Index (Pairof Node Index))
                     (-> Node Label Index (Pairof Node Index))
                     (-> Node Index Label Index (Pairof Node Index))
                     (Pairof Node Index))]
  )

(define dummy-node (node (make-label "dummy") #f '() #f))


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
  (: loop (-> Node Integer (values Node Index)))
  (define (loop node k)
    (let* ((child (node-find-child node (label-ref label k)))
           (child-label (begin
                          (unless child (error "skip-count-hlper"))
                          (node-up-label child)))
           (child-label-length (label-length child-label))
           (rest-of-chars-left-to-skip (- N k)))
      (if (> rest-of-chars-left-to-skip child-label-length)
          (loop child
                (+ k child-label-length))
          (begin (unless (index? rest-of-chars-left-to-skip) (error "skip-count=hlper !!!"))
                 (values child rest-of-chars-left-to-skip)))))
  (if (>= k N)
      (values node (label-length (node-up-label node)))
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
  (define PARENT (node-parent node))
  (cond ((node-root? node)
         (values node #f))
        ((node-suffix-link node)
         (begin 
                (let ([node2 (node-suffix-link node)])
                  (unless node2 (error "jump to suffix"))
                  (values node2 0))))
        ((and PARENT (node-root? PARENT))
           (values PARENT #f))
        (else
         (let* ([parent (node-parent node)]
                [sl (begin (unless parent (error "j2s"))
                           (node-suffix-link parent))])
           (unless sl (error "j2s whoahao"))
           (values sl
                   (label-length (node-up-label node)))))))

(provide try-to-set-suffix-edge!)
;; try-to-set-suffix-edge!: node node -> void
;;
;; Sets the suffix edge of from-node directed to to-node if it
;; hasn't been set yet.
(: try-to-set-suffix-edge! (-> Node Node Void))
(define (try-to-set-suffix-edge! from-node to-node)
  (when (not (node-suffix-link from-node))
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
  (: fixed-start (-> (U Integer #f) Index))
  (define (fixed-start suffix-offset)
    (let ([i (if suffix-offset (- initial-i suffix-offset) j)])
      (unless (index? i) (error "find-next")) i))
  (define-values (suffix-node suffix-offset)
    (jump-to-suffix node))
  (define K
    (fixed-start (cond [(integer? suffix-offset) suffix-offset]
                       [(eq? #t suffix-offset) 1]
                       [(eq? #f suffix-offset) #f]
                       [else (error "find-next")])))
  (define N
    (let ([i (label-length label)])
      (unless (index? i) (error "find-next")) i))
   (: loop-first (-> Index (values (U #f Node) (U #f Index) (U #f Index))))
   (define (loop-first i)
     (loop-general i (lambda ([skipped-node : Node]
                              [skip-offset  : Index])
                       (when (node-position-at-end? skipped-node skip-offset)
                         (try-to-set-suffix-edge! node skipped-node)))))
   (: loop-rest (-> Index (values (U #f Node) (U #f Index) (U #f Index))))
   (define (loop-rest i)
     (loop-general i (lambda ([skipped-node : Node] [skip-offset : Index])
                       (void))))
   (: loop-general (-> Index (-> Node Index Void) (values (U #f Node) (U #f Index) (U #f Index))))
   (define (loop-general i first-shot)
     (cond [(>= i N) (values #f #f #f)]
           [else 
            (define-values (skipped-node skipped-offset)
              (skip-count-helper suffix-node label K i))
            (first-shot skipped-node skipped-offset)
             (if (node-position-at-end? skipped-node skipped-offset)
                 (find-extension-at-end! skipped-node skipped-offset i)
                 (find-extension-in-edge skipped-node skipped-offset i))]))
   (: find-extension-in-edge (-> Node Index Index (values (U #f Node) (U #f Index) (U #f Index))))
   (define (find-extension-in-edge skipped-node skip-offset i)
     (cond [(label-element-equal?
             (label-ref label i)
             (label-ref (node-up-label skipped-node) skip-offset))
            (let ([n (add1 i)]) (unless (index? n) (error "find-next")) (loop-rest n))]
           [else (values skipped-node skip-offset i)]))
   (: find-extension-at-end! (-> Node Index Index (values (U #f Node) (U #f Index) (U #f Index))))
   (define (find-extension-at-end! skipped-node skip-offset i)
     (cond [(node-find-child skipped-node (label-ref label i))
            (let ([n (add1 i)]) (unless (index? n) (error "find-next")) (loop-rest n))]
           [else (values skipped-node skip-offset i)]))
   (loop-first initial-i))


(provide extend-at-point!)
;; extend-at-point!: node number label number -> node
(: extend-at-point! (-> Node Index Label Index Node))
(define (extend-at-point! node offset label i)
  (: main-logic (-> Node Index Label Index Node))
  (define (main-logic node offset label i)
    (if (should-extend-as-leaf? node offset)
        (attach-as-leaf! node label i)
        (splice-with-internal-node! node offset label i)))
  (: should-extend-as-leaf? (-> Node Index Boolean))
  (define (should-extend-as-leaf? node offset)
    (node-position-at-end? node offset))
  (: attach-as-leaf! (-> Node Label Index Node))
  (define (attach-as-leaf! node label i)
    (: leaf Node)
    (define leaf (node-add-leaf! node (sublabel label i)))
    node)
  (: splice-with-internal-node! (-> Node Index Label Index Node))
  (define (splice-with-internal-node! node offset label i)
    ;; otherwise, extend by splicing
    (define-values (split-node leaf)
      (node-up-splice-leaf!
       node offset (sublabel label i)))
    split-node)
  (main-logic node offset label i))

(provide suffix-tree-add!)
;; suffix-tree-add!: tree label -> void
;; Adds a new label and its suffixes to the suffix tree.
;; Precondition: label is nonempty.
(: suffix-tree-add! (-> Tree Label Void))
(define (suffix-tree-add! tree label)
  (: do-construction! (-> Tree Label Void))
  (define (do-construction! tree label)
    (define pr (add-first-suffix! tree label))
    (define starting-node (car pr))
    (define starting-offset (cdr pr))
    (add-rest-suffixes! label starting-node starting-offset))
  (: add-first-suffix! (-> Tree Label (Pairof Node Index)))
  (define (add-first-suffix! tree label)
    (: matched-at-node (-> Node (Pairof Node Index)))
    (define (matched-at-node node)
      (report-implicit-tree-constructed))
    (: matched-in-node (-> Node Index (Pairof Node Index)))
    (define (matched-in-node node offset)
      (report-implicit-tree-constructed))
    (: mismatched-at-node (-> Node Label Index (Pairof Node Index)))
    (define (mismatched-at-node node label label-offset)
      (define leaf
        (node-add-leaf! node (sublabel label label-offset)))
      (cons node label-offset))
    (: mismatched-in-node (-> Node Index Label Index (Pairof Node Index)))
    (define (mismatched-in-node node offset label label-offset)
      (define-values (joint leaf)
        (node-up-splice-leaf! node offset (sublabel label label-offset)))
      (cons joint label-offset))
    (define res (node-follow/k
                 (suffix-tree-root tree)
                 label
                 matched-at-node
                 matched-in-node
                 mismatched-at-node
                 mismatched-in-node))
    ;(when (void? res) (error "foo"))
    res)
  (: add-rest-suffixes! (-> Label Node Index Void))
  (define (add-rest-suffixes! label starting-node starting-offset)
    (add-rest-suffixes-loop!
     label
     (let ([i (label-length label)]) (unless (index? i) (error "ars")) i)
     (max starting-offset 1)
     1
     starting-node))
  (: add-rest-suffixes-loop! (-> Label Index Index Index Node Void))
  (define (add-rest-suffixes-loop! label N i j active-node)
    (when (< j N)
      (define-values (next-extension-node next-extension-offset i*)
        (find-next-extension-point/add-suffix-link! active-node label i j))
      (if i*
          (begin
            (let ([new-active-node
                   (extend-at-point! (begin (unless next-extension-node (error "bar bar bar")) next-extension-node)
                                     (begin (unless next-extension-offset (error "fofodofdof")) next-extension-offset)
                                     label i*)])
                  (try-to-set-suffix-edge! active-node new-active-node)
                  (add-rest-suffixes-loop!
                   label
                   N
                   (let ([num (max i* (add1 j))]) (unless (index? num) (error "foo")) num)
                   (let ([num (add1 j)]) (unless (index? num) (error "foo")) num)
                   new-active-node)))
          (begin (report-implicit-tree-constructed)
                 (void)))))
  (: report-implicit-tree-constructed (-> (Pairof Node Index)))
  (define (report-implicit-tree-constructed)
    (cons dummy-node 0))
  (do-construction! tree label))

;; -- from suffixtree.rkt

(provide tree-add!)
(define tree-add! suffix-tree-add!)
