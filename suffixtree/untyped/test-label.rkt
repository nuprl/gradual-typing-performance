#lang racket/base
(require "test-common-definitions.rkt")


(define label-tests
  (test-suite
   "test-label.rkt"
   
   
   (test-case
    "label construction"
    (let ((label (make-label "hello")))
      (check-equal? 5 (label-length label))))
   
   
   (test-case
    "test simple referencing"
    (let ((label (make-label "foobar")))
      (check-equal? #\f (label-ref label 0))
      (check-equal? #\o (label-ref label 1))
      (check-equal? #\o (label-ref label 2))
      (check-equal? #\b (label-ref label 3))
      (check-equal? #\a (label-ref label 4))
      (check-equal? #\r (label-ref label 5))))
   
   (test-case
    "test simple sublabeling and referencing"
    (let* ((label (make-label "supercalifragilisticexpialidocious"))
           (l2 (sublabel label 5 9))
           (l3 (sublabel l2 1 2)))
      
      (check-equal? 5 (label-length (sublabel label 0 5)))
      (check-equal? 4 (label-length l2))
      (check-equal? 1 (label-length l3))
      
      (check-equal? #\c (label-ref l2 0))
      (check-equal? #\a (label-ref l2 1))
      (check-equal? #\l (label-ref l2 2))
      (check-equal? #\i (label-ref l2 3))
      
      (check-equal? #\a (label-ref l3 0))))
   
   
   (test-case
    "prefix: empty-case"
    (check-true (label-prefix? (make-label "") (make-label ""))))
   
   
   (test-case
    "prefix: equal case"
    (check-true (label-prefix? (make-label "abra") (make-label "abra"))))
   
   (test-case
    "prefix: proper"
    (check-true (label-prefix? (make-label "abra") (make-label "abracadabra"))))
   
   (test-case
    "prefix: empty is the prefix of everything"
    (check-true (label-prefix? (make-label "") (make-label "blah"))))
   
   (test-case
    "prefix: simple failures"
    (check-false (label-prefix? (make-label "hello") (make-label "")))
    (check-false (label-prefix? (make-label "hello") (make-label "hi"))))
   
   (test-case
    "sublabel: single shift"
    (check label-equal? (make-label "ello") (sublabel (make-label "hello") 1)))
   
   (test-case
    "sublabel: shift from back"
    (check label-equal? (make-label "hiy") (sublabel (make-label "hiya") 0 3)))
   
   (test-case
    "sublabel: double shift from back"
    (check label-equal? (make-label "hi") (sublabel
                                           (sublabel (make-label "hiya") 0 3)
                                           0 2)))
   
   (test-case
    "sublabel: shift from back and front"
    (check label-equal? (make-label "i") (sublabel
                                          (sublabel
                                           (sublabel (make-label "hiya") 0 3)
                                           0 2)
                                          1)))
   
   (test-case
    "sublabel: double shift"
    (check label-equal? (make-label "llo")
           (sublabel
            (sublabel (make-label "hello") 1)
            1)))
   
   
   (test-case
    "sublabel: double sublabel"
    (check label-equal? (make-label "cowboy bebop")
           (sublabel (make-label "the anime cowboy bebop feels like firefly") 10 22))
    (check label-equal? (make-label "cowboy")
           (sublabel
            (sublabel (make-label "the anime cowboy bebop feels like firefly") 10 22)
            0 6)))
   
   
   (test-case
    "label-source-id"
    (let* ((label (string->label "antigone"))
           (subl (sublabel label 4)))
      (check-equal? (label-source-id label)
                    (label-source-id subl))))
   
   (test-case
    "label-same-source?"
    (let* ((label (string->label "hello"))
           (another-label (string->label "hello")))
      (check-true (label-same-source? (sublabel label 0 3) (sublabel label 3)))
      (check-false (label-same-source? label another-label))))))


(printf "label tests~%")
(void (run-tests label-tests))
