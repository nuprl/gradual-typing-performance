#lang racket 

;; basic constants and types for the game

(require "../base/utility.rkt")

(provide
 (type Face)
 (type Bulls)
 
 ;; constants 
 FACE
 HAND
 SIXTYSIX
 STACKS
 FIVE
 MAX-BULL
 MIN-BULL
 
 ;; -> [Listof Number]
 configuration)

;; -----------------------------------------------------------------------------

(define-type Face [1,FACE])
(define-type Bulls [MIN-BULL,MAX-BULL])

(define FACE 104)
(define HAND 10)
(define SIXTYSIX 66)
(define STACKS 4)
(define FIVE 5)
(define MAX-BULL 7)
(define MIN-BULL 2)

(define (configuration)
  `((FACE     ,FACE)
    (HAND     ,HAND)
    (SIXTSIX  ,SIXTYSIX)
    (STACKS   ,STACKS)
    (FIVE     ,FIVE)
    (MAX BULL ,MAX-BULL)
    (MIN BULL ,MIN-BULL)))

