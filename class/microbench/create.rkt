#lang racket/base

;; Repeatedly CREATE an empty class

;; Typed is faster at 1 mil. classes

(require (only-in racket/class send new))

;; -----------------------------------------------------------------------------

(define ITERS (expt 10 6))

(module u racket

  (define C
   (class object%
    (super-new)
    (define/public (update-one n)
      (void))))

  (define (main N)
    (for ([n (in-range N)])
      (new C)))

  (provide main))

(module t typed/racket
  (define-type MyClass
    (Class [update-one (-> Natural Void)]))

  (: C MyClass)
  (define C
   (class object%
    (super-new)

    (define/public (update-one n)
      (void))))

  (: main (-> Natural Void))
  (define (main N)
    (for ([n : Natural (in-range N)])
      (new C)))

  (provide main))


(require
  (prefix-in u: 'u)
  (prefix-in t: 't))

(displayln "Untyped")
(time (u:main ITERS))
(displayln "Typed")
(time (t:main ITERS))

