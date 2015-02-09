#lang racket 

(provide
 ;; Pict
 ;; a picture that illustrates how quick sort works 
 qsort-image)

;; ---------------------------------------------------------------------------------------------------

(require slideshow)

;; [Number] -> Pict 
;; create vertical blank space of size v
(define (vb (v 40)) (blank 1 v))

;; [Number] -> Pict 
;; create horizontal blank space of size v
(define (hb (h 50)) (blank h 1))

;; Number *-> Pict 
;; turn sequence of numbers x ... into framed (list x ...)
(define (alist #:color [color "black"]  . s)
  (define x (string-append "'(" (string-join (map number->string s)) ")"))
  (define y (text x))
  (cc-superimpose (colorize (rectangle (+ 10 (pict-width y)) (+ 10 (pict-height y))) color) y))

;; Pict *-> Pict
;; frame a sequence of images
(define (split . x)
  (define y (apply ht-append x))
  (define r (rectangle (+ 10 (pict-width y)) (+ 20 (pict-height y))))
  (cc-superimpose r y))

;; (add-> P t b [ta] [tb]) 
;; effect: adds a 'curvy' arrow from t to b in I, use ta as start angle and tb as end angle 
(define-syntax add->
  (syntax-rules ()
    [(_ P t b ta ba)
     (set! P (pin-arrow-line 5 P t cb-find b ct-find #:start-angle ta #:end-angle ba #:solid? #f))]
    [(_ P t b ta) (add-> P t b ta (- (* .5 pi)))]
    [(_ P t b) (add-> P t b (* 1.5 pi) (- (* .5 pi)))]))

(define-syntax dash->
  (syntax-rules ()
    [(_ P t b ba)
     (let ((s 'long-dash)
           (a (* 1.5 pi)))
       (set! P (pin-arrow-line 5 P t cb-find b ct-find #:start-angle ba #:end-angle a #:style 'dot)))]
    [(_ P t b) (dash-> P t b (- (* .5 pi)))]))

;; ---------------------------------------------------------------------------------------------------
;; qsort specific definitions 

(define qsort-image (blank 1 1))

;; Number -> Pict
;; circle-frame a QuickSort pivot number 
(define (pivot x)
  (define y (text (number->string x)))
  (define c (colorize (circle (* 2 (max (pict-width y) (pict-height y)))) "orange"))
  (cc-superimpose  c y))

;; Number *-> Pict
;; frame a number as a Quick Sort result 
(define (result . x)
  (keyword-apply alist '(#:color) '("purple") x))

(define l1 (alist 11 8 14 7))
(define l2 (alist 8 7))
(define l3 (alist 7))
(define l4 (alist 14))
(define p11 (pivot 11))
(define p7 (pivot 7))
(define p14 (pivot 14))
(define p8 (pivot 8))

(define e1 (alist))
(define e2 (alist))
(define e3 (alist))
(define e4 (alist))
(define e5 (alist))

(define r7 (result 7))
(define r8 (result 8))
(define r11 (result 11))
(define r14 (result 14))

(define divide
  (vc-append
        l1
        (vb)
        (split (vc-append 
                l2
                (vb)
                (split (vc-append
                        l3
                        (vb)
                        (split e1 (hb) p7 (hb) e2))
                       (hb) p8 (hb) e3))
               (hb)
               p11
               (hb)
               (vc-append
                l4
                (vb) 
                (split e4 (hb) p14 (hb) e5)))
        (vb 60)))

(define r7+8 (result 7 8))
(define r7+8+11+14 (result 7 8 11 14))

(define conquer
  (vl-append 
   (hc-append r7 (hb))
   (vb)
   (hc-append (hb) r7+8 (hb 100) r14)
   (vb 100)
   (hc-append (hb 70) r7+8+11+14)))
  
(set! qsort-image (vc-append divide conquer))

(add-> qsort-image l1 p11)
(add-> qsort-image l1 l2)
(add-> qsort-image l1 l4 (* 1.75 pi))
(add-> qsort-image l4 p14)
(add-> qsort-image l2 p8 (* 1.75 pi))
(add-> qsort-image l2 l3 (* 1.25 pi))
(add-> qsort-image l3 p7)
(add-> qsort-image l3 p7)

(add-> qsort-image l2 e3 (* 1.75 pi))
(add-> qsort-image l3 e1)
(add-> qsort-image l3 e2)
(add-> qsort-image l4 e4)
(add-> qsort-image l4 e5)

(dash-> qsort-image e1 r7) (dash-> qsort-image p7 r7) (dash-> qsort-image e2 r7)

(dash-> qsort-image e4 r14) (dash-> qsort-image p14 r14) (dash-> qsort-image e5 r14) 

(dash-> qsort-image e3 r7+8) (dash-> qsort-image p8 r7+8) (dash-> qsort-image r7 r7+8) 

(dash-> qsort-image r7+8 r7+8+11+14)
(dash-> qsort-image p11  r7+8+11+14)
(dash-> qsort-image r14  r7+8+11+14)


; qsort-image
