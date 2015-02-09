#lang slideshow

(require slideshow/pict "write.ss" "pict-aux.ss")

;                                                          
;                                                          
;                   ;                                      
;                                                          
;  ;;  ;; ;; ;;   ;;;   ;;; ;;;  ;;;   ;; ;;   ;;;;   ;;;  
;   ;   ;  ;;  ;    ;    ;   ;  ;   ;   ;;    ;   ;  ;   ; 
;   ;   ;  ;   ;    ;    ;   ;  ;;;;;   ;      ;;;   ;;;;; 
;   ;   ;  ;   ;    ;     ; ;   ;       ;         ;  ;     
;   ;  ;;  ;   ;    ;     ; ;   ;       ;     ;   ;  ;     
;    ;; ;;;;; ;;; ;;;;;    ;     ;;;;  ;;;;;  ;;;;    ;;;; 
;                                                          
;                                                          
;                                                          
;                                                          

(define base (ellipse 100 150))
(define *uni base)

;; String Nat Nat -> Void
;; effect: add text of data to universe at (x,y)
(define (u! data x y) (set! *uni (pin-over *uni x y (t data))))

;; String Pict -> Pict
;; effect: create labeled picture from universe
(define (u+label label universe)
  (vl-append (text label) (blank 1 10) (hc-append (blank 30 1) universe)))

;; +numbers
(for-each u!
          (append (build-list 4 number->string) (list "..."))
          (build-list 5 (lambda (i) (+ 25 (* 8 i))))
          (build-list 5 (lambda _ 9)))
(u! "π" 66 57)
(u! "1+1i" 29 130)

;; +booleans 
(u! "false" 33 29) (u! "true" 10 29)

;; +strings 
(for-each u! 
          '("\"hello\"" "\"world\"" "\"good\"" "\"bye\"" "...")
          (build-list 5 (lambda (i) (+ 5 (* 18 i))))
          (build-list 5 (lambda (i) (+ 60 (* 14 i)))))

(write-pict "dd0" *uni)

;                                                          
;                                                          
;      ;;                                 ;;           ;;; 
;       ;          ;                       ;          ;    
;    ;; ;   ;;;   ;;;;;   ;;;           ;; ;   ;;;   ;;;;; 
;   ;  ;;  ;   ;   ;     ;   ;         ;  ;;  ;   ;   ;    
;   ;   ;   ;;;;   ;      ;;;;         ;   ;  ;;;;;   ;    
;   ;   ;  ;   ;   ;     ;   ;         ;   ;  ;       ;    
;   ;   ;  ;   ;   ;   ; ;   ;         ;   ;  ;       ;    
;    ;;;;;  ;;;;;   ;;;   ;;;;;         ;;;;;  ;;;;  ;;;;; 
;                                                          
;                                                          
;                                                          
;                                                          

(define dd1
  (let ([width 70]
        [height 30]
        [delta 3])
    (cc-superimpose
     (clip-to-ellipse
      (apply vl-append (build-list (quotient height delta)
                                   (lambda (i) 
                                     (define r (filled-rectangle width delta))
                                     (if (odd? i) 
                                         (colorize r "white")
                                         (colorize r "gray")))))
      width height)
     (blank (pict-width base) (pict-height base)))))

(write-pict "dd1" (pin-over dd1 0 0 *uni))

;                                            
;                                            
;                                            
;           ;                           ;    
;    ;;;;  ;;;;;  ;; ;; ;;  ;;   ;;;;  ;;;;; 
;   ;   ;   ;      ;;    ;   ;  ;   ;   ;    
;    ;;;    ;      ;     ;   ;  ;       ;    
;       ;   ;      ;     ;   ;  ;       ;    
;   ;   ;   ;   ;  ;     ;  ;;  ;   ;   ;   ;
;   ;;;;     ;;;  ;;;;;   ;; ;;  ;;;     ;;; 
;                                            
;                                            
;                                            
;                                            

(define *posn (ellipse 180 150))

;; String Nat Nat -> Void
;; effect: add text of data to universe at (x,y)
(define (p! data x y) (set! *posn (pin-over *posn x y (t data))))

(define N 3)

(for-each p! 
          (build-list N (curry format "(make-posn \"hello\" ~a)"))
          (build-list N (lambda (i) (+ 20 (* i 5))))
          (build-list N (lambda (i) (+ 30 (* i 10)))))

(p! "(make-posn (make-posn 0 1) 2)" 10 70)

(set! N 4)

(for-each p! 
          (build-list N (curry format "(make-posn ~a 3)"))
          (build-list N (lambda (i) (- 50 (* i 5))))
          (build-list N (lambda (i) (+ 90 (* i 10)))))


(define *ball (ellipse 180 150))

;; String Nat Nat -> Void
;; effect: add text of data to universe at (x,y)
(define (b! data x y) (set! *ball (pin-over *ball x y (t data))))

(set! N 3)
(for-each b! 
          (build-list N (curry format "(make-ball -1 ~a)"))
          (build-list N (lambda (i) (+ 50 (* i 5))))
          (build-list N (lambda (i) (+ 10 (* i 10)))))

(b! "(make-ball \"hello\" (make-posn 1 1))" 5 50)

(for-each b! 
          (build-list N (curry format "(make-posn (make-ball ~a 1) 3)"))
          (build-list N (lambda (i) (- 30 (* i 5))))
          (build-list N (lambda (i) (+ 80 (* i 10)))))


(write-pict "dd-posn" (hc-append *uni *posn *ball))

;                                                                                             
;                                                                                             
;      ;;                                 ;;           ;;;                                    
;       ;          ;                       ;          ;                                       
;    ;; ;   ;;;   ;;;;;   ;;;           ;; ;   ;;;   ;;;;;        ;; ;;    ;;;    ;;;; ;; ;;  
;   ;  ;;  ;   ;   ;     ;   ;         ;  ;;  ;   ;   ;            ;;  ;  ;   ;  ;   ;  ;;  ; 
;   ;   ;   ;;;;   ;      ;;;;         ;   ;  ;;;;;   ;            ;   ;  ;   ;   ;;;   ;   ; 
;   ;   ;  ;   ;   ;     ;   ;         ;   ;  ;       ;            ;   ;  ;   ;      ;  ;   ; 
;   ;   ;  ;   ;   ;   ; ;   ;         ;   ;  ;       ;            ;   ;  ;   ;  ;   ;  ;   ; 
;    ;;;;;  ;;;;;   ;;;   ;;;;;         ;;;;;  ;;;;  ;;;;;         ;;;;    ;;;   ;;;;  ;;; ;;;
;                                                                  ;                          
;                                                                 ;;;                         
;                                                                                             
;                                                                                             

(define dd2
  (let ([width 120]
        [height 50]
        [delta 3])
    (pin-over 
     (blank (pict-width *posn) (pict-height *posn))
     20 90
     (clip-to-ellipse
      (apply vl-append (build-list (quotient height delta)
                                   (lambda (i) 
                                     (define r (filled-rectangle width delta))
                                     (if (odd? i) 
                                         (colorize r "white")
                                         (colorize r "gray")))))
      width height))))

(write-pict "dd2-posn" (hc-append *uni (cc-superimpose dd2 *posn) *ball))