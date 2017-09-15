#lang typed/racket/base

(provide
 Hotel
 Share
 Shares
 Color
 Cash
 M*ority
 Column
 Row
 SpotType
 Kind
 Content
 )

(define-type Hotel String)
(define-type Share Integer)
(define-type Shares (HashTable Hotel Share))
(define-type Color Symbol)
(define-type Cash Natural)
(define-type M*ority (U 'majority 'minority))
(define-type Column Natural)
(define-type Row Symbol)
(define-type SpotType (U 'FOUNDING 'GROWING 'MERGING 'SINGLETON 'IMPOSSIBLE))
(define-type Kind (U 'FOUNDING 'GROWING 'MERGING 'SINGLETON))
(define-type Content (U Hotel 'UNTAKEN 'taken-no-hotel))
