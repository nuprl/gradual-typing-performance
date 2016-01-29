#lang racket

;; Object contracts' exponential slowdown, from the ECOOP'14 paper.

;; v6.1.1
;; Running time is 99.23% contracts
;; 124708/125677 ms
;;
;; (instanceof/c (recursive-contract Obj%3036 #:impersonator))
;; obj
;; 249415/2 ms

;; v6.2
;; Running time is 0% contracts
;; 0/3 ms
;;
;; (recursive-contract anonymous-contract #:impersonator)
;; obj

(module u racket
  (provide (contract-out [obj bubble/c]))
  (define bubble/c
    (recursive-contract
      (object/c [m (->m bubble/c bubble/c)])))

  (define obj
    (new (class object%
           (super-new)
           (define/public (m x) x)))))

(module t typed/racket
  (provide obj)

  (define-type Obj%
    (Class (m (-> (Instance Obj%) (Instance Obj%)))))

  (: obj (Instance Obj%))
  (define obj
    (new (class object%
           (super-new)
           (: m (-> (Instance Obj%) (Instance Obj%)))
           (define/public (m x) x)))))

;; -----------------------------------------------------------------------------

(require
  contract-profile
  plot/no-gui
  (prefix-in u: 'u)
  (prefix-in t: 't))

(define LO 99)
(define HI 100)
(define STEP 5)

(define (loop obj N)
  (for/fold ([obj obj])
            ([i (in-range N)])
    (send obj m obj)))

(define (benchmark obj)
  (for/list ([i (in-range LO HI STEP)])
    (define-values (_res ms real gc) (time-apply loop (list obj i)))
    (list i ms)))

(contract-profile (time (benchmark t:obj)))
