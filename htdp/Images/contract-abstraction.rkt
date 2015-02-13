#lang racket/gui

(require slideshow/pict)

(dc-for-text-size (new bitmap-dc% [bitmap (make-object bitmap% 1 1)]))

;; -----------------------------------------------------------------------------

(define sp      (blank 3))
(define vsp     (blank 0 40))

(define Number  (text "Number"))
(define IR      (text "IR"))
(define String  (text "String"))
(define t->     (text "->"))


;; -----------------------------------------------------------------------------
(define (List-of c) (hc-append (text "[List-of") sp c (text "]")))
(define (2-> c d r) (hc-append (text "(") c sp d sp t-> sp r (text ")")))
(define (1-> dom r) (hc-append (text "(") dom sp t-> sp r (text ")")))
(define (--> c d r) (hc-append c sp d sp t-> sp r))

(define (stack-em base l1 l2)
  (foldr (lambda (src dst img) (pin-arrow-line 5.0 img src lb-find dst lt-find))
         base l1 l2))

(define (launder* n c) (apply values (build-list n (lambda (_) (launder c)))))


;; -----------------------------------------------------------------------------
;; cf* versus names:

(define-values (cf1 cf2 cf3 cf4) (launder* 4 Number))
(define-values (__nm1 nm2 nm3 nm4) (launder* 4 String))
(define nm1 (launder IR)) ;; <-- Eric's typo report

(define one.0 (hc-append 
               (vl-append (List-of cf1) vsp (List-of nm1))
               (vl-append t-> vsp t->)
               sp
               (vl-append (List-of cf4) vsp (List-of nm4))))

(define (cf*-vs-names-concrete) 
  (stack-em one.0 (list cf1 cf4) (list nm1 nm4)))

; (cf*-vs-names-concrete)

(define two.0 (hc-append
               (vl-append (List-of cf1) vsp (List-of nm1))
               sp
               (vl-append (1-> cf2 cf3) vsp (1-> nm2 nm3))
               sp
               (vl-append t-> vsp t->)
               sp
               (vl-append (List-of cf4) vsp (List-of nm4))))

(define (cf*-vs-names-abstract)
  (stack-em two.0 (list cf1 cf2 cf3 cf4) (list nm1 nm2 nm3 nm4)))

; (cf*-vs-names-abstract)

(provide cf*-vs-names-abstract cf*-vs-names-concrete)

;; -----------------------------------------------------------------------------
;; product vs image*

(define Posn (text "Posn"))
(define Image (text "Image"))

(define-values (pro1 pro2 pro3 pro4 pro5) (launder* 5 Number))
(define-values (img1 img3) (launder* 2 Posn))
(define-values (img2 img4) (launder* 2 Image))
(define-values (img5 img6) (launder* 2 Image))


(define pro.0 (hc-append 
               (vl-append (List-of pro1) vsp (List-of img1))
               sp
               (vl-append t-> vsp t->)
               sp
               (vl-append pro2 vsp img2)))

(define (product-vs-image*-concrete)
  (stack-em pro.0 (list pro1 pro2) (list img1 img2)))

; (product-vs-image*-concrete)

(define pro.1 (hc-append
               (vl-append (List-of pro1) vsp (List-of img1))
               sp
               (vl-append pro2 vsp img2)
               sp
               (vl-append (2-> pro3 pro4 pro5) vsp (2-> img3 img4 img5))
               sp
               (vl-append t-> vsp t->)
               sp
               (vl-append cf1 vsp img6)))


(define (product-vs-image*-abstract)
  (stack-em pro.1 (list pro1 pro2 pro3 pro4 pro5 cf1) (list img1 img2 img3 img4 img5 img6)))

; (product-vs-image*-abstract)

(provide product-vs-image*-abstract product-vs-image*-concrete)
