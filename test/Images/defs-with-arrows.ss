#lang slideshow 

(require "write.ss")

(current-font-size 12)

(define (self-referential)
  (define los (tt "List-of-strings"))
  (define line0 (hb-append (tt ";; A ") los (tt " is one of")))
  (define line1 (hb-append (tt ";; -- empty")))
  (define line2 (hb-append (tt ";; -- (cons String ") los (tt ")")))
  
  (define txt (vl-append line0 line1 line2))
  
  (define the-data-pict
    (pin-arrow-line 5 txt 
                    line2 (lambda _ 
                            (define-values (x _1) (ct-find line2 los))
                            (define-values (_2 y) (ct-find txt line2))
                            (values x y))
                    line0 (lambda _ 
                            (define-values (x _1) (cb-find line0 los))
                            (define-values (_2 y) (cb-find txt line0))
                            (values x y))
                    #:start-angle (* 3/4 pi)
                    #:end-angle (* 1/2 pi)))
  
  ;; ---- 
  
  (define fls (tt "(fun-for-los"))
  (define def-line (hb-append (tt "(define ") fls (tt " a-list-of-strings)")))
  
  (define rst (tt "(rest"))
  (define rst-line (hb-append (tt "     ... ") rst (tt " a-list-of-strings) ...]))")))
  
  (define fun-def
    (vl-append
     def-line
     (tt "  (cond")
     (tt "    [(empty? a-list-of-strings) ...]")
     (tt "    [else ; (cons? a-list-of-strings)")
     (tt "     ... (first a-list-of-strings)")
     rst-line))
  
  (define the-fun-pict
    (pin-arrow-line 5 fun-def
                    rst-line
                    (lambda _
                      (define-values (rst-x _0) (lc-find rst-line rst))
                      (define-values (_1 rst-y) (lc-find fun-def rst-line))
                      (values rst-x rst-y))
                    def-line
                    (lambda _ 
                      (define-values (fls-x _0) (cb-find def-line fls))
                      (define-values (_1 fls-y) (cb-find fun-def def-line))
                      (values fls-x fls-y))
                    #:start-angle (* 3/4 pi)
                    #:end-angle (* 1/2 pi)))
  
  (values 
   (write-pict "def-data-arrows" the-data-pict)
   (write-pict "def-fun-arrows" the-fun-pict)))

;; --- 

(define (mutual)
  (define los (tt "S-expr"))
  (define sl  (tt "SL"))
  (define atm (tt "Atom"))
  
  (define line0 (hb-append (tt ";; A ") los (tt " is one of")))
  (define line1 (hb-append (tt ";; -- ") atm))
  (define line2 (hb-append (tt ";; -- ") sl))
  (define line3 (tt ""))
  (define line4 (hb-append (tt ";; A ") sl (tt " is one of")))
  (define line5 (tt ";; -- empty"))
  (define line6 (hb-append (tt ";; -- (cons ") los (tt " ") sl (tt ")")))
  (define line7 (hb-append (tt "; An ") atm (tt " is one of")))
  
  (define txt 
    (vl-append line0 line1 line2 line3 line4 line5 line6 line3 line7
               (tt ";; -- Number") (tt ";; -- String")  (tt ";; -- Symbol")))
  
  (define atom->atom
    (pin-arrow-line 5 txt 
                    line2 (lambda _ 
                            (define-values (x _0) (lb-find line1 atm))
                            (define-values (_1 y) (lb-find txt line1))
                            (values x y))
                    line0 (lambda _ 
                            (define-values (x _2) (ct-find line7 atm))
                            (define-values (_3 y) (lt-find txt line7))
                            (values x y))
                    #:start-angle (* -7/8 pi)
                    #:start-pull .45
                    #:end-angle (* -1/4 pi)
                    #:end-pull .45))
  
  (define sl->sl
    (pin-arrow-line 5 atom->atom
                    line2 (lambda _ 
                            (define-values (sl-x1 _4) (lb-find line2 sl))
                            (define-values (_5 sl-y1) (lb-find txt line2))
                            (values sl-x1 sl-y1))
                    line0 (lambda _ 
                            (define-values (sl-x2 _6) (ct-find line4 sl))
                            (define-values (_7 sl-y2) (lt-find txt line4))
                            (values sl-x2 sl-y2))))
  
  (define sl->self 
    (pin-arrow-line 5 sl->sl
                    line2 (lambda _ 
                            (define-values (sl-x3 _8) (lt-find line6 sl))
                            (define-values (_9 sl-y3) (lt-find txt line6))
                            (values sl-x3 sl-y3))
                    line0 (lambda _ 
                            (define-values (sl-xb _a) (cb-find line4 sl))
                            (define-values (_b sl-yb) (lb-find txt line4))
                            (values sl-xb sl-yb))
                    #:start-angle (* 1/4 pi)
                    #:start-pull .3
                    #:end-angle (* 3/4 pi)
                    #:end-pull .5))
  
  (define sexpr->back
    (pin-arrow-line 5 sl->self
                    line2 (lambda _
                            (define-values (sx-x1 _f) (cb-find line6 los))
                            (define-values (_e sx-y1) (cc-find txt line6))
                            (values sx-x1 sx-y1))
                    line0 (lambda _ 
                            (define-values (sx-xb _c) (cb-find line0 los))
                            (define-values (_d sx-yb) (lb-find txt line0))
                            (values sx-xb sx-yb))
                    #:start-angle (* 1/4 pi)
                    #:start-pull .3
                    #:end-angle (* 3/4 pi)
                    #:end-pull .2))
  
  ;; -- IN --
  
  ;; --- 
  (define fsx (tt "(fun-for-sexpr")) (define sexpr (tt " an-sexpr)"))
  
  (define fsl (tt "(fun-for-sl"))
  (define fst (tt "(first "))
  (define rst (tt "(rest "))
  
  (define fat (tt "(fun-for-atom"))
  
  (define fsx-line (hb-append (tt "(define ") fsx sexpr))
  (define fat-call (hb-append (tt "   [(atom? an-sexpr) ") fat sexpr (text "]")))
  (define fsl-call (hb-append (tt "   [else ") fsl sexpr (tt "]))")))
  
  (define fsl-line (hb-append (tt "(define ") fsl (tt " a-sl)")))
  (define fst-line (hb-append (tt "     ... ") fst (tt " a-sl)")))
  (define rst-line (hb-append (tt "     ... ") rst (tt "a-sl) ...]))")))
  
  (define fat-line (hb-append (tt "(define ") fat (tt " an-atom)")))
  
  (define fun-def
    (vl-append
     fsx-line
     (tt " (cond")
     fat-call 
     fsl-call
     (tt "")
     fsl-line
     (tt "  (cond")
     (tt "    [(empty? a-sl) ...]")
     (tt "    [else ; (cons? a-sl)")
     fst-line
     rst-line
     (tt "")
     fat-line 
     (tt "  (cond")
     (tt "    [(number? an-atom) ...]")
     (tt "    [(string? an-atom) ...]")
     (tt "    [(symbol? an-atom) ...]))")))
  
  (define-values (fat-x1 __0) (lc-find fat-call fat))
  (define-values (__1 fat-y1) (lc-find fun-def fat-call))
  (define-values (fat-xb __2) (ct-find fat-line fat))
  (define-values (__3 fat-yb) (lt-find fun-def fat-line))
  
  (define fun-for-sx1 
    (pin-arrow-line 5 fun-def
                    fsl-line (lambda _ (values fat-x1 fat-y1))
                    fsl-line (lambda _ (values fat-xb fat-yb))
                    #:start-angle (* -1/2 pi)
                    #:end-angle (* -1/2 pi)
                    #:end-pull .1))
  
  (define-values (fsl-x1 __4) (cc-find fsl-call fsl))
  (define-values (__5 fsl-y1) (cc-find fun-def fsl-call))
  (define-values (fsl-xt __6) (ct-find fsl-line fsl))
  (define-values (__7 fsl-yt) (ct-find fun-def fsl-line))
  
  (define fun-for-sx2
    (pin-arrow-line 5 fun-for-sx1
                    fsl-line (lambda _ (values fsl-x1 fsl-y1))
                    fsl-line (lambda _ (values fsl-xt fsl-yt))
                    #:start-angle (* -1/2 pi)
                    #:end-angle (* -1/2 pi)
                    #:end-pull .1))
  
  (define-values (fsx-x1 _i) (lt-find rst-line rst))
  (define-values (_j fsx-y1) (cc-find fun-def rst-line))
  (define-values (fsx-xb _k) (cb-find fsl-line fsl))
  (define-values (_l fsx-yb) (lb-find fun-def fsl-line))
  
  (define fun-for-sl
    (pin-arrow-line 5 fun-for-sx2
                    fsl-line (lambda _ (values fsx-x1 fsx-y1))
                    fsl-line (lambda _ (values fsx-xb fsx-yb))
                    #:start-angle (* 3/4 pi)
                    #:end-angle (* 1/2 pi)
                    #:end-pull .7))
  
  (define-values (fst-x1 __a) (lc-find fst-line fst))
  (define-values (__b fst-y1) (lc-find fun-def fst-line))
  (define-values (fsx-xbt __c) (cb-find fsx-line fsx))
  (define-values (__d fsx-ybt) (cb-find fun-def fsx-line))
  
  (define fun-mutual
    (pin-arrow-line 5 fun-for-sl
                    fsl-line (lambda _ (values fst-x1 fst-y1))
                    fsl-line (lambda _ (values fsx-xbt fsx-ybt))
                    #:start-angle (* 3/4 pi)
                    #:end-angle (* 1/2 pi)
                    #:end-pull .1))
  ;; -- IN -- 
  (values 
   (write-pict "def-data-mutual-arrows" sexpr->back)
   (write-pict "def-fun-mutual-arrows" fun-mutual)))

; (self-referential)
(mutual)