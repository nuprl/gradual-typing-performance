#lang racket/base
(require "test-common-definitions.rkt"
         "ukkonen2.rkt")


(define ukkonen-test-suite
  (test-suite
   "ukkonen test suite"
   
   (test-case
    "empty construction"
    (let-values (((root tree) (root-and-tree)))
      (check-equal? (list) (node-children root))
      (check-label-equal? (make-label "") (node-up-label root))
      (check-eq? #f (node-suffix-link root))))
   
   
   (test-case
    "adding a leaf."
    (let-values (((root tree) (root-and-tree)))
      (node-add-leaf! root (make-label "bookkeeper"))
      (check-equal? 1 (length (node-children root)))))
   
   
   (test-case
    "tree-contains? #t on empty tree"
    (let-values (((root tree) (root-and-tree)))
      (check-true (tree-contains? tree (make-label "")))))
   
   
   (test-case
    "tree-contains? #f on empty tree"
    (let-values (((root tree) (root-and-tree)))
      (check-false (tree-contains? tree (make-label "munging")))))
   
   
   (test-case
    "tree-contains? on whole tree"
    (let-values (((root tree) (root-and-tree)))
      (node-add-leaf! root (make-label "fezzick"))
      (check-true (tree-contains? tree (make-label "fezzick")))))
   
   
   (test-case
    "tree-contains? on prefix"
    (let-values (((root tree) (root-and-tree)))
      (node-add-leaf! root (make-label "wesley"))
      (check-true (tree-contains? tree (make-label "wes")))))
   
   
   (test-case
    "tree-contains? failing"
    (let-values (((root tree) (root-and-tree)))
      (node-add-leaf! root (make-label "inigo"))
      (check-false (tree-contains? tree (make-label "montoya")))))
   
   
   (test-case
    "tree-contains? passing through two nodes"
    (let-values (((root tree) (root-and-tree)))
      (node-up-split!
       (node-add-leaf! root (make-label "inconcievable")) 5)
      (check-true (tree-contains? tree (make-label "inconcievable")))))
   
   
   (test-case
    "adding two leaves."
    (let-values (((root tree) (root-and-tree)))
      (node-add-leaf! root (make-label "bookkeeper"))
      (node-add-leaf! root (make-label "ookkeeper"))
      (check-equal? 2 (length (node-children root)))))
   
   
   (test-case
    "finding children"
    (let-values (((root tree) (root-and-tree)))
      (let ((leaf-1 (node-add-leaf! root (make-label "blah")))
            (leaf-2 (node-add-leaf! root (make-label "lah"))))
        (check-eq? leaf-1 (node-find-child root #\b))
        (check-eq? leaf-2 (node-find-child root #\l))
        (check-false (node-find-child root #\a)))))
   
   
   (test-case
    "test splitting"
    (let-values (((root tree) (root-and-tree)))
      (let* ((leaf (node-add-leaf! root (make-label "ssi")))
             (joint (node-up-split! leaf 1)))
        (check-equal? 1 (length (node-children root)))
        (check-eq? root (node-parent joint))
        (check-eq? joint (node-parent leaf))
        (check-eq? joint (node-find-child root #\s))
        (check-eq? leaf (node-find-child joint #\s)))))
   
   
   (test-case
    "node-up-splice-leaf!"
    (let-values (((root tree) (root-and-tree)))
      (let ((leaf (node-add-leaf! root (make-label "aaz"))))
        (let-values (((joint split-leaf)
                      (node-up-splice-leaf! leaf 1 (make-label "z"))))
          (check-label-equal? (make-label "z") (node-up-label split-leaf))
          (check-label-equal? (make-label "az") (node-up-label leaf))
          (check-label-equal? (make-label "a")
                              (node-up-label
                               (node-parent split-leaf)))))))
   
   
   (test-case
    "following"
    (let-values (((root tree) (root-and-tree)))
      (let ((leaf (node-add-leaf! root (make-label "delicious"))))
        (node-follow/k root (make-label "delicious")
                       (lambda (node) #t)
                       (lambda (node offset) (fail))
                       (lambda (node label label-offset) (fail))
                       (lambda (node offset label label-offset) (fail)))
        (node-follow/k root (make-label "deli")
                       (lambda (node) (fail))
                       (lambda (node offset)
                         (check-eq? node leaf)
                         (check-equal? 4 offset))
                       (lambda (node label label-offset) (fail))
                       (lambda (node offset label label-offset) (fail)))
        (node-follow/k root (make-label "yummy")
                       (lambda (node) (fail))
                       (lambda (node offset) (fail))
                       (lambda (node label label-offset)
                         (check-eq? root node)
                         (check-label-equal? (make-label "yummy")
                                             (sublabel label label-offset)))
                       (lambda (node offset label label-offset) (fail)))
        (node-follow/k root (make-label "done")
                       (lambda (node) (fail))
                       (lambda (node offset) (fail))
                       (lambda (node label label-offset) (fail))
                       (lambda (node offset label label-offset)
                         (check-eq? leaf node)
                         (check-equal? 1 offset)
                         (check-label-equal?
                          (make-label "one")
                          (sublabel label label-offset)))))))
   
   
   (test-case
    "test skip-counting"
    (let-values (((root tree) (root-and-tree)))
      (let* ((some-node (node-add-leaf! root (make-label "arabidopsis")))
             (another-leaf (node-add-leaf! some-node (make-label "thaliana"))))
        (let-values (((node offset)
                      (skip-count root (make-label "arabidopsist"))))
          (check-eq? another-leaf node)
          (check-equal? 1 offset)))))
   
   
   (test-case
    "test skip-counting on end"
    (let-values (((root tree) (root-and-tree)))
      (let* ((some-node (node-add-leaf! root (make-label "crunchy")))
             (another-leaf (node-add-leaf! some-node (make-label "bacon"))))
        (let-values (((node offset)
                      (skip-count root (make-label "crunchy"))))
          (check-equal? 7 offset)
          (check-eq? some-node node)))))
   
   
   (test-case
    "test skip-counting on empty string"
    (let-values (((root tree) (root-and-tree)))
      (let* ((some-node (node-add-leaf! root (make-label "power")))
             (another-leaf (node-add-leaf! some-node (make-label "rangers"))))
        (let-values (((node offset)
                      (skip-count root (make-label ""))))
          (check-equal? 0 offset)
          (check-eq? root node)))))
   
   
   (test-case
    "skip-counting within the tree on empty string"
    (let-values (((root tree) (root-and-tree)))
      (let ((leaf (node-add-leaf! root (make-label "blah"))))
        (let-values (((node offset)
                      (skip-count leaf (make-label ""))))
          (check-eq? leaf node)
          (check-equal? 4 offset)))))
   
   
   (test-case
    "simple suffix tree adding with single character"
    (let-values (((root tree) (root-and-tree)))
      (suffix-tree-add! tree (make-label "a"))
      (check-not-false (node-find-child root #\a))))
   
   
   (test-case
    "simple suffix tree adding: aa --- implicit tree"
    (let-values (((root tree) (root-and-tree)))
      (suffix-tree-add! tree (make-label "aa"))
      (let ((child (node-find-child root #\a)))
        (check-not-false child)
        (check-label-equal? (make-label "aa") (node-up-label child)))))
   
   
   (test-case
    "simple suffix tree adding: aa with sentinel --- explicit tree"
    (let*-values
        (((root tree) (root-and-tree))
         ((label) (string->label/with-sentinel "aa"))
         ((a-label) (make-label "a"))
         ((sentinel-label) (sublabel label (sub1 (label-length label)))))
      (suffix-tree-add! tree label)
      (let ((child (node-find-child root (label-ref a-label 0))))
        (check-not-false child)
        (check-label-equal? a-label (node-up-label child))
        (check-not-false (node-find-child child (label-ref a-label 0)))
        (check-label-equal?
         (sublabel label 1)
         (node-up-label (node-find-child child (label-ref a-label 0)))))))
   
   
   
   (test-case
    "simple suffix tree adding: a$"
    (let-values (((root tree) (root-and-tree)))
      (suffix-tree-add! tree (make-label "a$"))
      (check-not-false (node-find-child root #\a))
      (check-label-equal? (make-label "a$")
                          (node-up-label
                           (node-find-child root #\a)))
      (check-not-false (node-find-child root #\$))
      (check-label-equal? (make-label "$")
                          (node-up-label
                           (node-find-child root #\$)))))
   
   
   (test-case
    "missisippi example: missisippi$"
    (let-values (((root tree) (root-and-tree)))
      (suffix-tree-add! tree (make-label "mississippi$"))
      (check tree-contains? tree (make-label "mississippi$"))
      (check tree-contains? tree (make-label "ississippi$"))
      (check tree-contains? tree (make-label "ssissippi$"))
      (check tree-contains? tree (make-label "sissippi$"))
      (check tree-contains? tree (make-label "issippi$"))
      (check tree-contains? tree (make-label "ssippi$"))
      (check tree-contains? tree (make-label "sippi$"))
      (check tree-contains? tree (make-label "ippi$"))
      (check tree-contains? tree (make-label "ppi$"))
      (check tree-contains? tree (make-label "pi$"))
      (check tree-contains? tree (make-label "i$"))
      (check tree-contains? tree (make-label "$"))
      (check tree-contains? tree (make-label ""))))
   
   
   (test-case
    "missisippi$ example as a vector of symbols"
    (let* ((v (list->vector '(m i s s i s s i p p i $)))
           (label (vector->label v)))
      (let-values (((root tree) (root-and-tree)))
        (suffix-tree-add! tree label)
        (for-each-sublabel (lambda (s)
                             (check tree-contains? tree s))
                           label))))
   
   
   (test-case
    "test all substrings of binary strings of length 8"
    (let loop ((n 0))
      (when (< n 256)
        (let-values (((root tree) (root-and-tree)))
          (let ((binary-label (make-label
                               (string-append
                                (zfill (number->string n 2) 8)
                                "$"))))
            (suffix-tree-add! tree binary-label)
            (for-each-sublabel (lambda (s)
                                 (check tree-contains? tree s
                                        (string-append
                                         "couldn't find "
                                         (label->string s)
                                         " in "
                                         (label->string binary-label))))
                               binary-label)
            (loop (add1 n)))))))
   
   
   
   (test-case
    "testing jump-to-suffix on node with suffix link"
    (let*-values (((tree nodes) (tree-structure-for-00000100$))
                  ((node offset) (jump-to-suffix (list-ref nodes 8))))
      (check-eq? (list-ref nodes 0) node)
      (check-equal? 0 offset)))
   
   
   (test-case
    "another jump-to-suffix test."
    (let-values (((root tree) (root-and-tree)))
      (let* ((leaf (node-add-leaf! root (make-label "00000100$")))
             (joint (node-up-split! leaf 4)))
        (let-values (((n o) (jump-to-suffix joint)))
          (check-eq? root n)
          (check-false o)))))
   
   
   (test-case
    "jumping-to-suffix on root should return (values root #f)"
    (let*-values (((root tree) (root-and-tree))
                  ((n o) (jump-to-suffix root)))
      (check-eq? root n)
      (check-false o)))
   
   
   (test-case
    "find-next-extension-point at root should point to the root"
    (let-values (((root tree) (root-and-tree)))
      (let-values (((node offset i*)
                    (find-next-extension-point/add-suffix-link! root
                                                                (make-label "hercules") 0 0)))
        (check-eq? node root)
        (check-equal? 0 offset)
        (check-equal? 0 i*))))
   
   
   (test-case
    "looking for the next extension point"
    (let-values (((tree nodes) (tree-structure-for-00000100$)))
      (let-values (((node offset i*) (find-next-extension-point/add-suffix-link!
                                      (list-ref nodes 6)
                                      (make-label "00000100$")
                                      8 7)))
        (check-eq? node (list-ref nodes 8))
        (check-equal? 1 offset)
        (check-equal? 8 i*))))
   
   
   (test-case
    "find-next-extension on beginning of 00$ construction"
    (let-values (((root tree) (root-and-tree)))
      (let ((leaf
             (node-add-leaf! root (make-label "00$"))))
        (let-values (((extension-node extension-offset i*)
                      (find-next-extension-point/add-suffix-link! root
                                                                  (make-label "001$")
                                                                  1 1)))
          (check-eq? leaf extension-node)
          (check-equal? 1 extension-offset)
          (check-equal? 2 i*)))))
   
   
   (test-case
    "find-next-extension intensional failure"
    (let-values (((root tree) (root-and-tree)))
      (let ((leaf
             (node-add-leaf! root (make-label "000000"))))
        (let-values (((extension-node extension-offset i*)
                      (find-next-extension-point/add-suffix-link! root
                                                                  (make-label "000000")
                                                                  1 1)))
          (check-false extension-node)
          (check-false extension-offset)
          (check-false i*)))))
   
   
   (test-case
    "extend-at-point! at the root"
    (let*-values (((root tree) (root-and-tree))
                  ((label) (make-label "foo$"))
                  ((first-char) #\f))
      (check-false (node-find-child root first-char))
      (let ((last-active-node
             (extend-at-point! root 0 label 0)))
        (check-eq? last-active-node root)
        (check-not-false (node-find-child root first-char)))))
   
   
   (test-case
    "extend-at-point with splice"
    (let-values (((root tree) (root-and-tree)))
      (let ((leaf (node-add-leaf! root (make-label "001"))))
        (let ((new-active-point (extend-at-point! leaf 1
                                                  (make-label "001")
                                                  2)))
          (check-label-equal? (make-label "0")
                              (node-up-label new-active-point))
          (check-label-equal? (make-label "01")
                              (node-up-label leaf))
          (check-not-false (node-find-child new-active-point
                                            #\1))))))
   
   
   
   (test-case
    "suffix-tree-add! on two single elements"
    (let-values (((root tree) (root-and-tree))
                 ((label-1 label-2) (values (make-label "$")
                                            (make-label "!"))))
      (suffix-tree-add! tree label-1)
      (suffix-tree-add! tree label-2)
      (for-each-sublabel (lambda (s) (check tree-contains? tree s))
                         label-1)
      (for-each-sublabel (lambda (s) (check tree-contains? tree s))
                         label-2)))
   
   
   (test-case
    "suffix-tree-add! on two short strings"
    (let-values (((root tree) (root-and-tree))
                 ((label-1 label-2) (values (make-label "a$")
                                            (make-label "a!"))))
      (suffix-tree-add! tree label-1)
      (suffix-tree-add! tree label-2)
      (check tree-contains? tree (make-label "a"))
      (check tree-contains? tree (make-label "a$"))
      (check tree-contains? tree (make-label "$"))
      (check tree-contains? tree (make-label "a!"))
      (check tree-contains? tree (make-label "!"))
      (check tree-contains? tree (make-label ""))
      ))
   
   
   ;; FIXME: Refactor pairwise comparison of these strings.
   (test-case
    "suffix-tree-add! on two strings"
    (let-values (((root tree) (root-and-tree))
                 ((label-1 label-2) (values (make-label "aaazzz$")
                                            (make-label "aaazza!"))))
      (suffix-tree-add! tree label-1)
      (suffix-tree-add! tree label-2)
      (for-each-sublabel (lambda (s) (check tree-contains? tree s))
                         label-1)
      (for-each-sublabel (lambda (s) (check tree-contains? tree s))
                         label-2)))
   
   ))

(printf "ukkonen tests~%")
(void (run-tests ukkonen-test-suite))
