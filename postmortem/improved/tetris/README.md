Runtime `before`: 47027ms

Runtime `after`: 70ms


### Overview

After a small change, tetris' runtime is amazingly better.

Maybe the contract profiler should blame lists for the recursive checks they trigger.


### Changelog

Changes:
- Add a typed, wrapping interface to the untyped bset module
  (Remembers that a list is type-checked)

- Change the `tetra` constructor to unwrap output from bset
  (Effected in the adaptor module.)

- Edit the imports of the tetra module to use the new wrapping
  interface instead of directly `(require/typed "bset.rkt" ...)`


### The Code

##### bset-protector.rkt
```
#lang typed/racket/base

(require "base-types.rkt" benchmark-util)

(require/typed/check "bset.rkt"
 [blocks-intersect (-> BSet BSet BSet)]
 [blocks-move (-> Real Real BSet BSet)]
 [blocks-rotate-cw (-> Posn BSet BSet)]
 [blocks-rotate-ccw (-> Posn BSet BSet)]
 [blocks-change-color (-> BSet Color BSet)])

(provide (rename-out
 [blocks-intersect+ blocks-intersect]
 [blocks-move+ blocks-move]
 [blocks-rotate-ccw+ blocks-rotate-ccw]
 [blocks-rotate-cw+ blocks-rotate-cw]
 [blocks-change-color+ blocks-change-color]))

(: blocks-intersect+ (-> BSet+ BSet+ BSet+))
(define (blocks-intersect+ b1 b2)
  (wrap
    (blocks-intersect (unwrap b1)
                      (unwrap b2))))

(: blocks-move+ (-> Real Real BSet+ BSet+))
(define (blocks-move+ r1 r2 b1)
  (wrap (blocks-move r1 r2 (unwrap b1))))

(: blocks-rotate-cw+ (-> Posn BSet+ BSet+))
(define (blocks-rotate-cw+ p b)
  (wrap (blocks-rotate-cw p (unwrap b))))

(: blocks-rotate-ccw+ (-> Posn BSet+ BSet+))
(define (blocks-rotate-ccw+ p b)
  (wrap (blocks-rotate-ccw p (unwrap b))))

(: blocks-change-color+ (-> BSet+ Color BSet+))
(define (blocks-change-color+ b c)
  (wrap (blocks-change-color (unwrap b) c)))
```


##### base-types.rkt (the adaptor)
```
#lang typed/racket

(require benchmark-util)
(require/typed/check "data.rkt"
  ;; ... truncated
(require/typed 2htdp/image 
  [#:opaque Image image?])

(define-type Color Symbol)
(define-type Posn posn)
(define-type Block block)
(define-type Tetra tetra)
(define-type World world)
(define-type BSet  (Listof Block))

;; === NEW =========================
(struct bset+ ([val : BSet]))

(define-type BSet+ (U BSet bset+))

(: unwrap (-> BSet+ BSet))
(define (unwrap b)
  (if (bset+? b) (bset+-val b) b))

(: wrap (-> BSet+ BSet+))
(define (wrap b)
  (if (bset+? b) b (bset+ b)))

(: make-tetra (-> Posn BSet+ Tetra))
(define (make-tetra a b)
  (tetra a (unwrap b)))
;; === NEW =========================

(provide
 ;; ... truncated
```
