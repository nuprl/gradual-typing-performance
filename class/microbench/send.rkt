#lang racket/base

;; Repeatedly SEND a message to a class
;; Even without contract barriers,
;;  typed is 2x slower

(require (only-in racket/class send new))

;; -----------------------------------------------------------------------------

(define ITERS (expt 20 6))

(module u racket

  (define C
   (class object%
    (super-new)
    (define/public (update-one n)
      (void))))

  (define (main N)
    (define c (new C))
    (for ([n (in-range N)])
      (send c update-one n)))

  (provide main))

(module t typed/racket
  (define-type MyClass
    (Class
      [update-one (-> Natural Void)]))

  (: C MyClass)
  (define C
   (class object%
    (super-new)
    (define/public (update-one n)
      (void))))

  (: main (-> Natural Void))
  (define (main N)
    (define c (new C))
    (for ([n : Natural (in-range N)])
      (send c update-one n)))

  (provide main))


(require
  (prefix-in u: 'u)
  (prefix-in t: 't))

(displayln "Untyped")
(time (u:main ITERS))

(displayln "Typed")
(time (t:main ITERS))

