#lang racket/base
(require rackunit
         rackunit/text-ui
         "suffixtree.rkt"
         "util.rkt")


(define-simple-check (check-label-equal? a b)
  (label-equal? a b))

(define util-tests
  (test-suite
   "test-util.rkt"
   
   (test-case
    "longest common sublabel of empty strings is empty strings 1"
    (check-label-equal? (string->label "")
                        (longest-common-sublabel (string->label "")
                                                 (string->label ""))))
   
   (test-case
    "longest common sublabel of empty strings is empty strings 2"
    (check-label-equal? (string->label "")
                        (longest-common-sublabel (string->label "aha!")
                                                 (string->label ""))))
   
   (test-case
    "longest common sublabel of empty strings is empty strings 2"
    (check-label-equal? (string->label "")
                        (longest-common-sublabel (string->label "")
                                                 (string->label "bah!"))))
   
   (test-case
    "longest common sublabel with trivial answer"
    (check-label-equal? (string->label "a")
                        (longest-common-sublabel (string->label "a?")
                                                 (string->label "a!"))))
   (test-case
    "longest-common-sublabel with less trivial answer"
    (check-label-equal?
     (string->label "alive")
     (longest-common-sublabel (string->label "superiorcalifornialives?")
                              (string->label "sealiver!"))))
   
   (test-case
    "longest-common-sublabel with less trivial answer and sentinels"
    (check-label-equal?
     (string->label "alive")
     (longest-common-sublabel (string->label/with-sentinel "superiorcalifornialives")
                              (string->label/with-sentinel "sealiver"))))
   
   (test-case
    "path-label on empty tree"
    (check-label-equal? (string->label "")
                        (path-label (tree-root (make-tree)))))
   
   (test-case
    "path-label on leaf with no joints"
    (let ((tree (make-tree)))
      (tree-add! tree (string->label "abcd"))
      (let ((leaf (node-find-child (tree-root tree) #\a)))
        (check-label-equal? (string->label "abcd")
                            (path-label leaf)))))
   (test-case
    "path-label on the joints of 'abac'
+--- a ---- bac
|    |
|    + ---- c
|
+--- bac
|
+--- c
"
    (let* ((tree (make-tree))
           (root (tree-root tree)))
      (tree-add! tree (string->label "abac"))
      (check-label-equal? (string->label "a")
                          (path-label (node-find-child root #\a)))
      (check-label-equal? (string->label "abac")
                          (path-label (node-find-child
                                       (node-find-child root #\a)
                                       #\b)))
      (check-label-equal? (string->label "ac")
                          (path-label (node-find-child
                                       (node-find-child root #\a)
                                       #\c)))
      (check-label-equal? (string->label "bac")
                          (path-label (node-find-child root #\b)))
      (check-label-equal? (string->label "c")
                          (path-label (node-find-child root #\c)))))))



(error-print-width 800)
(printf "util.rkt tests~%")
(void (run-tests util-tests))
