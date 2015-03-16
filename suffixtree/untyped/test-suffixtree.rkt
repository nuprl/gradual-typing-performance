#lang racket/base
(require rackunit
         rackunit/text-ui
         "label.rkt" "structs.rkt" "ukkonen2.rkt"
         "suffixtree.rkt"
         (for-syntax racket/base))




;; simple counter function to trace sequence order of actions.
(define (make-counter)
  (let ((n -1))
    (lambda ()
      (set! n (add1 n))
      n)))

;; Returns a trace of a tree walk
(define (trace-tree-walk tree label)
  (let* ((events '())
         (add-event! (lambda args (set! events (cons args events)))))
    (tree-walk tree label
               (lambda (node up-label-offset)
                 (add-event! 'success node up-label-offset))
               (lambda (node up-label-offset input-label-offset)
                 (add-event! 'fail node up-label-offset input-label-offset)))
    (reverse events)))

;; Checks to see if two tree traces look the same.
(define (trace-equal? trace-1 trace-2)
  (if (or (null? trace-1) (null? trace-2))
      (and (null? trace-1) (null? trace-2))
      (let-values (((head-1 head-2) (values (car trace-1) (car trace-2))))
        (and (eq? (car head-1) (car head-2))
             (equal? (cdr head-1) (cdr head-2))
             (trace-equal? (cdr trace-1) (cdr trace-2))))))


(define-simple-check (check-trace-equal? a b)
  (trace-equal? a b))


(define suffixtree-tests
  (let ((tree #f)
        (root #f)
        (count-f #f))
    (let-syntax
        ((make-test-case
            ;;; Just to make things easier to type. This definition
            ;;; shadows the existing make-test-case macro. Modifies
            ;;; make-test-case to automatically reset tree and root to
            ;;; initial values.
            ;;;
            ;;; WARNING: it reuses it! Make sure the above does not
            ;;; become a lecrec-syntax, or else things will get ugly
            ;;; fast.
            ;;;
            ;;; TODO: see if we can modify schemeunit to define test
            ;;; suites with recurrent setup/teardown fixtures.
          (lambda (stx)
            (syntax-case stx ()
              ((_ name e1 e2 ...)
               (syntax/loc stx
                 (test-case
                  name
                  (set! tree (make-tree))
                  (set! root (tree-root tree))
                  (set! count-f (make-counter))
                  e1 e2 ...)))))))
      
      
      (test-suite
       "suffixtree.rkt"
       
       (make-test-case
        "tree? #t"
        (check-true (tree? tree)))
       
       
       (make-test-case
        "tree? #f"
        (check-false (tree? "arbor")))
       
       
       (make-test-case
        "tree-root"
        (check-true (node? root)))
       
       
       (make-test-case
        "node? #f"
        (check-false (node? tree))
        (check-false (node? "bulge")))
       
       
       (make-test-case
        "tree-add! \"\" should fail"
        (check-exn exn:fail?
                   (lambda () (tree-add! tree (string->label "")))))
       
       
       (make-test-case
        "tree-add! \"x\" should not fail"
        (check-not-exn (lambda () (tree-add! tree (string->label "x")))))
       
       
       (make-test-case
        "tree-add! on duplicate strings should raise implicit tree exception"
        ;; fixme: make the exception more explicit
        (check-exn exn:fail? (lambda ()
                               (tree-add! tree (string->label "a"))
                               (tree-add! tree (string->label "a")))))
       
       (make-test-case
        "node-up-label on root"
        (check label-equal? (string->label "") (node-up-label root)))
       
       
       (make-test-case
        "node-up-label on leaves"
        (tree-add! tree (string->label "abc"))
        (let* ((nodes (node-children root))
               (strings (map (lambda (node)
                               (label->string (node-up-label node)))
                             nodes)))
          (check-not-false (member "abc" strings))
          (check-not-false (member "bc" strings))
          (check-not-false (member "c" strings))
          (check-equal? 3 (length strings))))
       
       
       (make-test-case
        "node-parent of root is #f"
        (check-eq? #f (node-parent root)))
       
       
       (make-test-case
        "node-parent of single-edged tree should be root"
        (tree-add! tree (string->label "a"))
        (check-equal? 1 (length (node-children root)))
        (let ((leaf (car (node-children root))))
          (check-equal? root (node-parent leaf))))
       
       
       (make-test-case
        "node-parent with joint in the middle"
        (tree-add! tree (string->label "aah"))
        (let ((joint (node-find-child root #\a)))
          (check-eq? root (node-parent joint))
          (check label-equal? (string->label "a") (node-up-label joint))
          (let ((leaf (node-find-child joint #\h)))
            (check-eq? joint (node-parent leaf))
            (check label-equal? (string->label "h") (node-up-label leaf)))))
       
       
       (make-test-case
        "node-find-child on empty returns #f"
        (check-false (node-find-child root (string->label "z"))))
       
       
       (make-test-case
        "node-find-child on single leafed tree"
        (tree-add! tree (string->label "a"))
        (check-false (node-find-child root #\z))
        (check-not-false (node-find-child root #\a))
        (check-eq? (car (node-children root))
                   (node-find-child root #\a)))
       
       
       (make-test-case
        "node-find-child with choices"
        (tree-add! tree (string->label "a"))
        (tree-add! tree (string->label "b"))
        (tree-add! tree (string->label "c"))
        (check-equal? 3 (length (node-children root)))
        (let ((a (node-find-child root #\a))
              (b (node-find-child root #\b))
              (c (node-find-child root #\c)))
          (check-not-false a)
          (check-not-false b)
          (check-not-false c)
          (check-not-eq? a b)
          (check-not-eq? a c)
          (check-not-eq? b c)
          (check-eq? root (node-parent a))
          (check-eq? root (node-parent b))
          (check-eq? root (node-parent c))
          (check-equal? "a" (label->string (node-up-label a)))
          (check-equal? "b" (label->string (node-up-label b)))
          (check-equal? "c" (label->string (node-up-label c)))))
       
       
       (make-test-case
        "structure tests + suffix-link test on 1011$

The tree should have the following structure:

(root) +-- $ (a)
|
+-- 011$ (b)
|
+-- 1 (c) -- $ (d)
|
+------- 011$ (e)
|
+------- 1$ (f)

with the only suffix link directed from (c) to the (root).
"
          (tree-add! tree (string->label "1011$"))
          (let* ((a (node-find-child root #\$))
                 (b (node-find-child root #\0))
                 (c (node-find-child root #\1))
                 (d (node-find-child c #\$))
                 (e (node-find-child c #\0))
                 (f (node-find-child c #\1)))
            (for-each (lambda (n) (check-not-false n)) (list a b c d e f))
            (check-eq? root (node-suffix-link c))
            (for-each (lambda (n) (check-eq? root (node-parent n)))
                      (list a b c))
            (for-each (lambda (n) (check-eq? c (node-parent n)))
                      (list d e f))
            (for-each (lambda (node s)
                        (check string=? s
                                (label->string (node-up-label node))))
                      (list a b c d e f)
                      (list "$" "011$" "1" "$" "011$" "1$"))))
         
         
         (make-test-case
          "string->label/with-sentinel"
          (check-equal? 2 (label-length (string->label/with-sentinel "1"))))
         
         
         (make-test-case
          "string->label/with-sentinel: sentinels are unique"
          (let ((l1 (string->label/with-sentinel "1"))
                (l2 (string->label/with-sentinel "1")))
            (check-not-eq? (label-ref l1 1) (label-ref l2 1))
            (check char=? (label-ref l1 0) (label-ref l2 0))))
         
         
         (make-test-case
          "label->string on empty is empty"
          (check-equal? "" (label->string (string->label ""))))
         
         
         (make-test-case
          "label->string on whole label"
          (check-equal? "chocolate" (label->string (string->label "chocolate"))))
         
         
         (make-test-case
          "label->string on sublabel"
          (check-equal? "oh" (label->string (sublabel (string->label "pooh bear") 2 4))))
         
         
         (make-test-case
          "vector->label"
          (let ((label (vector->label '#(1 two 3.0))))
            (check-equal? 3 (label-length label))
            (check-equal? 1 (label-ref label 0))
            (check-equal? 'two (label-ref label 1))
            (check-equal? 3.0 (label-ref label 2))))
         
         
         (make-test-case
          "vector->label on empty case"
          (let ((label (vector->label #())))
            (check-equal? 0 (label-length label))))
         
         
         (make-test-case
          "vector->label/with-sentinel"
          (check-equal? 1 (label-length (vector->label/with-sentinel #())))
          (let ((label-1 (vector->label/with-sentinel #()))
                (label-2 (vector->label/with-sentinel #())))
            (check-not-eq? (label-ref label-1 0)
                            (label-ref label-2 0))))
         
         
         (make-test-case
          "label->vector"
          (let ((label (string->label "hello world")))
            (check-equal? (list->vector (string->list "hello world"))
                           (label->vector label))
            (check-true (immutable? (label->vector label)))))
         
         
         (make-test-case
          "label->vector with slicing"
          (let* ((label-1 (string->label "tom and jerry"))
                 (label-2 (sublabel label-1 8)))
            (check-equal? (list->vector (string->list "jerry"))
                           (label->vector label-2))
            (check-true (immutable? (label->vector label-1)))
            (check-true (immutable? (label->vector label-2)))))
         
         
         (make-test-case
          "label-equal? on same label"
          (let ((label (string->label "foo")))
            (check-true (label-equal? label label))))
         
         
         (make-test-case
          "label-equal? on different labels obviously false"
          (let ((label-1 (string->label "foo"))
                (label-2 (string->label "bar")))
            (check-false (label-equal? label-1 label-2))))
         
         
         (make-test-case
          "label-equal? on different labels true"
          (let ((label-1 (string->label "blocks"))
                (label-2 (string->label "locks")))
            (check-true (label-equal? (sublabel label-1 1) label-2))))
         
         
         (make-test-case
          "label-source-id"
          (let* ((label-1 (string->label "hello"))
                 (label-2 (string->label "world")))
            (check-false (equal? (label-source-id label-1) (label-source-id label-2)))))
         
         
         (make-test-case
          "label-source-id on empty string"
          (let* ((label (string->label ""))
                 (subl (sublabel label 0)))
            (check-equal? (label-source-id label) (label-source-id subl))))
         
         
         (make-test-case
          "label-source-id on another string"
          (let* ((label (string->label "terminator"))
                 (subl (sublabel label 3 5)))
            (check-equal? (label-source-id label) (label-source-id subl))))
         
         
         (make-test-case
          "label-source-eq?"
          (let* ((label-1 (string->label "jump"))
                 (label-2 (string->label "jump")))
            (check-true (label-source-eq? (sublabel label-1 0 2)
                                           (sublabel label-1 2))
                         (check-false (label-source-eq? label-1 label-2)))))
         
         
         (make-test-case
          "null tree-walk"
          (check-trace-equal? `((success ,(tree-root tree) 0))
                               (trace-tree-walk tree (string->label ""))))
         
         
         (make-test-case
          "unsuccessful tree-walk from empty tree"
          (check-trace-equal? `((fail ,(tree-root tree) 0 0))
                               (trace-tree-walk tree (string->label "the truth is out there"))))
         
         
         (make-test-case
          "tree walks from aa$, whose tree looks like:

v
+---- $ (1)
|
+---- a (2) ---- $ (3)
|
+ ------- a$ (4)

I'll try to do this as exhaustively as I can.
"
          (tree-add! tree (string->label "aa$"))
          (let* ((n1 (node-find-child root #\$))
                 (n2 (node-find-child root #\a))
                 (n3 (node-find-child n2 #\$))
                 (n4 (node-find-child n2 #\a)))
            (check-trace-equal? `((success ,n1 1))
                                 (trace-tree-walk tree (string->label "$")))
            (check-trace-equal? `((fail ,root 0 0))
                                 (trace-tree-walk tree (string->label "z")))
            (check-trace-equal? `((fail ,n1 1 1))
                                 (trace-tree-walk tree (string->label "$z")))
            (check-trace-equal? `((success ,n2 1))
                                 (trace-tree-walk tree (string->label "a")))
            (check-trace-equal? `((fail ,n2 1 1))
                                 (trace-tree-walk tree (string->label "az")))
            (check-trace-equal? `((success ,n3 1))
                                 (trace-tree-walk tree (string->label "a$")))
            (check-trace-equal? `((fail ,n3 1 2))
                                 (trace-tree-walk tree (string->label "a$z")))
            (check-trace-equal? `((success ,n4 1))
                                 (trace-tree-walk tree (string->label "aa")))
            (check-trace-equal? `((fail ,n4 1 2))
                                 (trace-tree-walk tree (string->label "aaz")))
            (check-trace-equal? `((success ,n4 2))
                                 (trace-tree-walk tree (string->label "aa$")))
            (check-trace-equal? `((fail ,n4 2 3))
                                 (trace-tree-walk tree (string->label "aa$z")))))
         
         
         (make-test-case
          "single-character successful tree-walk"
          (let* ((label-x (string->label "x"))
                 (_ (tree-add! tree label-x))
                 (leaf (node-find-child root #\x)))
            (check trace-equal? `((success ,leaf 1))
                    (trace-tree-walk tree label-x))))
         
         (make-test-case
          "missisippi$ with tree-contains?"
          (tree-add! tree (string->label "mississippi$"))
          (check-false (tree-contains? tree (string->label "california$")))
          (for-each (lambda (s)
                      (check-true (tree-contains? tree (string->label s))))
                    '("mississippi$"
                      "ississippi$"
                      "ssissippi$"
                      "sissippi$"
                      "issippi$"
                      "ssippi$"
                      "sippi$"
                      "ippi$"
                      "ppi$"
                      "pi$"
                      "i$"
                      "$"
                      "")))))))
  
  
  (error-print-width 800)
  (printf "suffixtree.rkt tests~%")
  (void (run-tests suffixtree-tests))
