#lang racket

;; NOTE: this file is not part of the experiment

;; Compile an expression for k-CFA analysis
;; Source terms are lambda terms.

;; Usage:
;; Directly call `compile` in this script with a symbol representing a lambda term.
;;
;; Output goes directly to stdout; pipe this to a file or something.
;; After producing output, need two "sanitation" steps:
;; - Remove the leading quote character
;; - Remove the symbol 'halt
;;
;; Finally, paste the result into the `main.rkt` file of k-CFA

;; =============================================================================

;; -- From Matt Might
(define (T expr cont)
  (match expr
    [`(λ . ,_)
     `(make-call ,cont ,(M expr))]
    [ (? symbol?)
      `(make-call ,cont ,(M expr))]
    [`(,f ,e)
      ; =>
      (define $f (gensym '$f))
      (define $e (gensym '$e))
      (T f `(make-lambda '(,$f)
              ,(T e `(make-lambda '(,$e)
                       (make-call (make-ref ',$f) (make-ref ',$e) ,cont)))))]))

(define (M expr)
  (match expr
    [`(λ (,var) ,expr)
      ; =>
      (define $k (gensym '$k))
     `(make-lambda '(,var ,$k) ,(T expr `(make-ref ',$k)))]

    [(? symbol?)  #;=>  `(make-ref ',expr)]))

;; -- Front-end

(define (compile sym)
  (T sym 'halt))

;; Examples:

;; (compile '((λ (s)
;;         ((λ (k)
;;            ((s k) k))
;;          (λ (x) (λ (y) x))))
;;       (λ (x) (λ (y) (λ (z) ((x z) (y z)))))))

;; (compile '((λ (s)
;;        ((λ (k)
;;           ((λ (i)
;;              (((s i) i) ((s i) i)))
;;            ((s k) k)))
;;         (λ (x) (λ (y) x))))
;;      (λ (x) (λ (y) (λ (z) ((x z) (y z)))))))

(compile
  '((λ (plus)
     ((λ (mult)
        ((λ (pred)
           ((λ (sub)
              ((λ (church0)
                 ((λ (church1)
                    ((λ (church2)
                       ((λ (church3)
                          ((λ (true)
                             ((λ (false)
                                ((λ (church0?)
                                   ((λ (Y)
                                      ((λ (church=?)
                                         ((church=? ((mult church2) ((plus church1) church3)))
                                          ((plus ((mult church2) church1)) ((mult church2) church3))))
                                       (Y
                                        (λ (church=?)
                                          (λ (e1)
                                            (λ (e2)
                                              (((church0? e1) ;; IF
                                                (λ (thendummy) (church0? e2)))
                                               (λ (elsedummy1)
                                                 (((church0? e2) ;; IF
                                                   false)
                                                  (λ (elsedummy2) ((church=? ((sub e1) church1)) ((sub e2) church1))))))))))))
                                    (λ (yf)
                                      ((λ (yg) (yg yg))
                                       (λ (yx) (yf (λ (yv) ((yx yx) yv))))))))
                                 (λ (z) ((z (λ (zx) false)) true))))
                              (λ (fa) (λ (fb) (fb (λ (bdummy) bdummy))))))
                           (λ (ta) (λ (tb) (ta (λ (adummy) adummy))))))
                        (λ (f3) (λ (x3) (f3 (f3 (f3 x3)))))))
                     (λ (f2) (λ (x2) (f2 (f2 x2))))))
                  (λ (f1) (λ (x1) (f1 x1)))))
               (λ (f0) (λ (x0) x0))))
            (λ (s1)
              (λ (s2)
                ((s2 pred) s1)))))
         (λ (n)
           (λ (rf)
             (λ (rx)
               (((n (λ (g) (λ (h) (h (g rf)))))
                 (λ (ignored) rx))
                (λ (id) id)))))))
      (λ (m1)
        (λ (m2)
          (λ (mf) (m2 (m1 mf)))))))
   (λ (p1)
     (λ (p2)
       (λ (pf)
         (λ (x) ((p1 pf) ((p2 pf) x))))))))
