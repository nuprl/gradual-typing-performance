#lang racket/base

;; Core:
;; Implementing comparisons

(provide
 (struct-out comparison)
 order?
 build-comparison)
(require (only-in data/order
  order?
  order ;(All (A) (-> Symbol
        ;              (-> Any Boolean)
        ;              (-> Any Any Symbol)
        ;              Order))])
))
;; =============================================================================

(struct comparison
        (=? ;: (-> Any Any Boolean)]
         <? ;: (-> Any Any Boolean)]
         <=? ;: (-> Any Any Boolean)]
         >? ;: (-> Any Any Boolean)]
         >=? ;: (-> Any Any Boolean)]
         comparator ;: (-> Any Any Symbol)]
         order ;: Order]))
         ))

;(: build-comparison (-> Symbol
;                                 (-> Any Boolean)
;                                 (-> Any Exact-Rational)
;                                 comparison))
(define (build-comparison name pred? ->num)
  ;(: comparator (-> Any Any Symbol))
  (define (comparator x y)
    (unless (and (pred? x) (pred? y)) (error "comparison: type error"))
    ;(: diff Exact-Rational)
    (define diff (- (->num x) (->num y)))
    (cond [(> 0 diff) '<]
          [(= 0 diff) '=]
          [else       '>]))

  ;(: =? (-> Any Any Boolean))
  (define (=? x y)
    (unless (and (pred? x) (pred? y)) (error "comparison: type error"))
    (eq? '= (comparator x y)))
  ;(: <? (-> Any Any Boolean))
  (define (<? x y)
    (unless (and (pred? x) (pred? y)) (error "comparison: type error"))
    (eq? '< (comparator x y)))
  ;(: >? (-> Any Any Boolean))
  (define (>? x y)
    (unless (and (pred? x) (pred? y)) (error "comparison: type error"))
    (eq? '> (comparator x y)))
  ;(: <=? (-> Any Any Boolean))
  (define (<=? x y)
    (unless (and (pred? x) (pred? y)) (error "comparison: type error"))
    (case (comparator x y)
      [(< =) #t]
      [else  #f]))
  ;(: >=? (-> Any Any Boolean))
  (define (>=? x y)
    (unless (and (pred? x) (pred? y)) (error "comparison: type error"))
    (case (comparator x y)
      [(> =) #t]
      [else  #f]))
  ;(: the-order Order)
  (define the-order (order name pred? comparator))

  (comparison =? <? <=? >? >=? comparator the-order))
