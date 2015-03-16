#lang racket/base
;; Program to do some timing of tree building.

;; time-implementation: string string -> number
;;
;; string should be the name of the module that implements a
;; make-suffix-tree function.  s is the string used to construct the
;; tree.  Returns the amount of milliseconds taken to generate the tree.


(require "label.rkt")
(require "structs.rkt")
(require (prefix-in ukkonen: "ukkonen2.rkt"))
(require profile)


(define (time-implementation string)
  (let ((impl (get-implementation)))
    (let ((start-time (current-inexact-milliseconds)))
      (let ((new-tree
             (impl (string->label/with-sentinel string))))
        (let ((end-time (current-inexact-milliseconds)))
          ;; fixme: add some structural tests to make sure this isn't cheating?
          (- end-time start-time))))))

    
(define (get-implementation)
  (lambda (label)
    (let ((tree (new-suffix-tree)))
      (ukkonen:suffix-tree-add! tree label)
      tree)))



;; random-string: number -> string
;; Returns a random string of 0's and 1's of length n.
(define (random-string n)
  (let ((buffer (make-string n #\0)))
    (let loop ((k 0))
      (when (< k n)
        (when (= (random 2) 1)
          (string-set! buffer k #\1))
        (loop (add1 k))))
    buffer))


(define (time-trial)
  (let* ((step 10)
         (start 100)
         (stop 10000)
         (strings (let loop ((k start))
                   (if (< k stop)
                       (cons (random-string k)
                             (loop (+ k step)))
                       (list)))))
    (map (lambda (s)
           (list (string-length s) (time-implementation s)))
         strings)))



(profile-thunk (lambda () (write (time-trial))))
(newline)
