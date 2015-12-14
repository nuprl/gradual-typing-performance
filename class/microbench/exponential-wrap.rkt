#lang racket

;; Exponential slowdown from object contracts.
;;
;; (Class contracts seem fine, so far)

;; -----------------------------------------------------------------------------

(module+ main
  (require "count-chaps.rkt")
  (define lo 1)
  (define hi 6)

  (for ([i (in-list (for/list ([j (in-range lo hi)]) (* 5 j)))])
    (displayln i)
    (time (main i))
    (count-chaps))

  ;(time (main 1))
  ;(time (main 2))
  ;(time (main 3))
  ;(time (main 25))
  ;(count-chaps)
)

;; -----------------------------------------------------------------------------

(module t1 typed/racket

  ;; --- 1. Create a "complicated-enough" object type
  (define-type ComplexEnough%
    (Class
      (lam
        ;(-> (Class) (Class))            ;; No problem
        ;(-> C C)                        ;; Problem
        ;(-> Any ComplexEnough)          ;; Less-serious problem
        ;(-> ComplexEnough Any)          ;; Problem
        (-> ComplexEnough ComplexEnough) ;; Problem
      )))
  (define-type ComplexEnough (Instance ComplexEnough%))

  (: c% ComplexEnough%)
  (define c%
    (class object%
      (super-new)
      (define/public (lam x) x)))

  (: obj ComplexEnough)
  (define obj (new c%))
  (: obj2 ComplexEnough)
  (define obj2 (new c%))

  ;; --- 2. Create a simple function on objects
  (: mymix (-> ComplexEnough% ComplexEnough%))
  (define (mymix x) x)

  (: myid (-> ComplexEnough ComplexEnough))
  (define (myid x) x)

  (provide c% obj obj2 myid mymix))

;; --- 3. Repeatedly apply the function to an object
(require 't1)

(define (check-class M)
  (for/fold
      ([c c%])
      ([i (in-range M)])
    (mymix c))
  (void))

(define (check-object M)
  (for/fold
      ([obj1 obj]
       [obj2 obj2])
      ([i (in-range M)])
    (values (myid obj1) (myid obj2)))
  (void))

(define (main M)
  (check-class M))

