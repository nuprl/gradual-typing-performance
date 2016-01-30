#lang racket

(provide
 ;; Boolean -> Pict
 ;; #t show path to occurrence of y, otherwise to \x.y
 le0
 ;; -> Image
 le1
 le2)

;; ---------------------------------------------------------------------- 

(require slideshow "ysupport.rkt")

#;
'((lambda (x)
    [(lambda (y) (y x)) x]) 
  (lambda (z) z))

(define (label name)
  (let* ([texts (t name)]
         [R (+ 6 (max 30 (pict-height texts) (pict-width texts)))])
    (cc-superimpose texts (ellipse R R))))

(define (my-frame p #:delta (delta 5))
  (cc-superimpose
   (rectangle (+ (pict-width p) delta) (+ (pict-height p) delta))
   p))

(define (le0 le-to-y)
  (define (lam x) (label (string-append "λ" x)))
  (define app-top (label "@"))
  (define app-bot (label "@"))
  (define lam-x (lam "x"))
  (define lam-y (lam "y"))
  (define lam-z (lam "z"))
  (define lam-w (lam "w"))
  (define y (label "y"))
  (define z (label "z"))
  (define w (label "w"))
  (define-values (boxed path-width) 
    (if le-to-y
        (values (vc-append 30 lam-x (my-frame y #:delta 20)) 8)
        (values (my-frame (vc-append 30 lam-x y) #:delta 20) 1)))
  
  #|
            @
           / \
          @   \w
         / \   |
        \y  \z w
        |    |
        \x   z
        |
        y
 |#
  (build-scene 
   s 
   (vc-append 30 
              app-top
              (ht-append 
               30
               (vc-append 30
                          app-bot
                          (ht-append 30
                                     (vc-append 30 lam-y boxed)
                                     (vc-append 30 lam-z z)))
               (vc-append 30 lam-w w)))
   (pin-line s app-top cb-find app-bot ct-find #:line-width 8)
   (pin-line s app-top cb-find lam-w ct-find)
   (pin-line s app-bot cb-find lam-y ct-find #:line-width 8)
   (pin-line s app-bot cb-find lam-z ct-find)
   (pin-line s lam-w cb-find w ct-find)
   (pin-line s lam-y cb-find lam-x ct-find #:line-width 8)
   (pin-line s lam-x cb-find y ct-find #:line-width path-width)
   (pin-line s lam-z cb-find z ct-find)
   (scale s .66)))
  
;; (le0 #t)
;; (le0 #f]

;; show: #t says show λs with names and dashed arrows
(define (le x1-in x2-in y-in z-in (show #t))
  (define-syntax-rule 
    (m-pin-arrow-line w s x ...)
    (if show
        (pin-arrow-line w s x ...)
        s))
  (define (lam x) (if show (label (string-append "λ" x)) (label "λ")))
  (define app-x (label "@"))
  (define lam-x (lam "x"))
  (define app-y (label "@"))
  (define lam-y (lam "y"))
  (define lam-z (lam "z"))
  (define app-yx (label "@"))
  (define y (label y-in))
  (define x1 (label x1-in))
  (define x2 (label x2-in))
  (define z (label z-in))
  (build-scene 
   s 
   
   #|
          @
         / \
        \x  \z
        |    |
        @    z
       /\
      /  \
     \y   x2
     |
     @
    /\
   y  x1
 |#
   
   (hc-append y (blank 30) x1)
   (vc-append app-yx (blank 50) s)
   (pin-line s app-yx cb-find y ct-find)
   (pin-line s app-yx cb-find x1 ct-find)
   (vc-append lam-y (blank 30) s)
   (pin-line s lam-y cb-find app-yx ct-find)
   (ht-append s (blank 50) x2)
   (vc-append app-y (blank 50) s)
   (pin-line s app-y cb-find lam-y ct-find)
   (pin-line s app-y cb-find x2 ct-find)
   (vc-append lam-x (blank 30) s)
   (pin-line s lam-x cb-find app-y ct-find)
   (ht-append s (vc-append lam-z (blank 30) z))
   (pin-line s lam-z cb-find z ct-find)
   (vc-append app-x (blank 50) s)
   (pin-line s app-x cb-find lam-x ct-find)
   (pin-line s app-x cb-find lam-z ct-find)
   (hc-append (blank 50) s)
   
   (m-pin-arrow-line 10 s y lc-find lam-y lc-find
                     #:style 'dot-dash
                     #:start-angle (- pi)
                     #:end-angle 0)
   
   (m-pin-arrow-line 10 s x1 rc-find lam-x lc-find
                     #:style 'dot-dash
                     #:start-angle 0
                     #:end-angle 0)
   
   (m-pin-arrow-line 10 s x2 rc-find lam-x lc-find
                     #:start-pull .8
                     #:end-pull .8
                     #:style 'dot-dash
                     #:start-angle 0
                     #:end-angle 0)
   
   (m-pin-arrow-line 10 s z lc-find lam-z lc-find
                     #:start-pull .8
                     #:end-pull .8
                     #:style 'dot-dash
                     #:start-angle (- pi)
                     #:end-angle 0)
   
   (scale s .66)))

(define (le1) (le "x" "x" "y" "z"))
(define (le2) (le "1" "0" "0" "0" #f))
; (le1)
; (le2)
