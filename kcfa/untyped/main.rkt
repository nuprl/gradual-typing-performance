#lang racket/base

;; Create a few examples and run abstract interpretation

(require
  "structs.rkt"
  "ui.rkt"
)

;; =============================================================================

(define new-label gensym)

;(: make-ref (-> Var Exp))
(define (make-ref var)
  (Ref (new-label) var))

;(: make-lambda (-> (Listof Var) Exp Exp))
(define (make-lambda formals call)
  (Lam (new-label) formals call))

;(: make-call (-> Exp Exp * Exp))
(define (make-call fun . args)
  (Call (new-label) fun args))

;(: make-let (-> Var Exp Exp Exp))
(define (make-let var exp call)
  (make-call (make-lambda (list var) call) exp))

;; -- example

;; (let* ((id (lambda (x) x))
;;        (a  (id (lambda (z) (halt z))))
;;        (b  (id (lambda (y) (halt y)))))
;;   (halt b))
;(: standard-example Exp)
(define standard-example
  (make-let
    'id
    (make-lambda '(x k) (make-call (make-ref 'k) (make-ref 'x)))
    (make-call (make-ref 'id)
                (make-lambda '(z) (make-ref 'z))
                (make-lambda '(a) 
                              (make-call (make-ref 'id)
                                          (make-lambda '(y) (make-ref 'y))
                                          (make-lambda '(b)
                                                        (make-ref 'b)))))))

;; (let ((f1 (lambda (f k) (f k)))
;;       (f2 (lambda (f k) (f k)))
;;       (f3 (lambda (f k) (f k))))
;;   (f1 f2))
;(: loop3 Exp)
(define loop3
  (make-let
    'f1 (make-lambda '(f k) (make-call (make-ref 'f) (make-ref 'k)))
    (make-let
      'f2 (make-lambda '(f k) (make-call (make-ref 'f) (make-ref 'k)))
      (make-let
        'f3 (make-lambda '(f k) (make-call (make-ref 'f) (make-ref 'k)))
        (make-call (make-ref 'f1) (make-ref 'f2))))))

;; (let ((S (lambda (a b c k) (k (a c (b c)))))
;;       (K (lambda (a b k)   (k a)))
;;       (I (lambda (x k)     (k x))))
;;   ((S I I) (S I I)))
(define ski
  (make-let
   'S (make-lambda '(a b c k)
                   ;; TODO issue here
                   (make-call (make-ref 'k)
                              (make-call (make-ref 'a)
                                         (make-ref 'c)
                                         (make-call (make-ref 'b) (make-ref 'c)))))
   (make-let
    'K (make-lambda '(a b k) (make-call (make-ref 'k)
                                        (make-ref 'a)))
     (make-call (make-ref 'K) (make-ref 'K) (make-ref 'K)))))

;; (let ((zero (lambda (f x k) (k x)))
;;       (succ (lambda (n k) (lambda (f x) (k (n f x)))))
;;       (add  (lambda (n1 n2 k) (k (n1 suc n2)))))
;;   (add (succ succ zero) (succ succ zero)))
;(: church Exp)
(define church
  (make-let
    'zero (make-lambda '(f x k) (make-call (make-ref 'k) (make-ref 'x)))
    (make-call (make-ref 'zero) (make-ref 'zero) (make-ref 'zero))))

;; multiplication distributes over addition test: (2 * (1 + 3)) = ((2 * 1) + (2 * 3)
(define ianj-test
  ((lambda (plus)
     ((lambda (mult)
        ((lambda (pred)
           ((lambda (sub)
              ((lambda (church0)
                 ((lambda (church1)
                    ((lambda (church2)
                       ((lambda (church3)
                          ((lambda (true)
                             ((lambda (false)
                                ((lambda (church0?)
                                   ((lambda (Y)
                                      ((lambda (church=?)
                                         ((church=? ((mult church2) ((plus church1) church3)))
                                          ((plus ((mult church2) church1)) ((mult church2) church3))))
                                       (Y
                                        (lambda (church=?)
                                          (lambda (e1)
                                            (lambda (e2)
                                              (((church0? e1) ;; IF
                                                (lambda (thendummy) (church0? e2)))
                                               (lambda (elsedummy1)
                                                 (((church0? e2) ;; IF
                                                   false)
                                                  (lambda (elsedummy2) ((church=? ((sub e1) church1)) ((sub e2) church1))))))))))))
                                    (lambda (yf)
                                      ((lambda (yg) (yg yg))
                                       (lambda (yx) (yf (λ (yv) ((yx yx) yv))))))))
                                 (lambda (z) ((z (lambda (zx) false)) true))))
                              (lambda (fa) (lambda (fb) (fb (lambda (bdummy) bdummy))))))
                           (lambda (ta) (lambda (tb) (ta (lambda (adummy) adummy))))))
                        (lambda (f3) (lambda (x3) (f3 (f3 (f3 x3)))))))
                     (lambda (f2) (lambda (x2) (f2 (f2 x2))))))
                  (lambda (f1) (lambda (x1) (f1 x1)))))
               (lambda (f0) (lambda (x0) x0))))
            (lambda (s1)
              (lambda (s2)
                ((s2 pred) s1)))))
         (lambda (n)
           (lambda (rf)
             (lambda (rx)
               (((n (lambda (g) (lambda (h) (h (g rf)))))
                 (lambda (ignored) rx))
                (lambda (id) id)))))))
      (lambda (m1)
        (lambda (m2)
          (lambda (mf) (m2 (m1 mf)))))))
   (lambda (p1)
     (lambda (p2)
       (lambda (pf)
         (lambda (x) ((p1 pf) ((p2 pf) x))))))))

;; -- main

(define (main)
  (for ([a-k (in-range 6)])
    ;; (analyze ianj-test)
    ;; (analyze standard-example)
    ;; (analyze loop3)
    ;; (analyze church)
    (analyze ski)
  ))
    ;; (printf "K = ~a\n~a\n"
    ;;    a-k (format-mono-store (analyze ianj-test)))))

(time (main))
