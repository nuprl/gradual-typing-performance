#lang slideshow

(require slideshow/pict "write.ss" "pict-aux.ss")

;                                            
;                                            
;                                 ;          
;                  ;                         
;  ;;; ;    ;;;   ;;;;;  ;; ;;  ;;;     ;;;; 
;   ; ; ;  ;   ;   ;      ;;      ;    ;   ; 
;   ; ; ;  ;;;;;   ;      ;       ;    ;     
;   ; ; ;  ;       ;      ;       ;    ;     
;   ; ; ;  ;       ;   ;  ;       ;    ;   ; 
;  ;;;;;;;  ;;;;    ;;;  ;;;;;  ;;;;;   ;;;  
;                                            
;                                            
;                                            
;                                            

(define rect (rectangle 10 10))
(define grid 
  (apply vc-append (make-list 8 (apply hc-append (make-list 10 rect)))))

(define R 5)
(define (dot blue) (colorize (filled-ellipse R R) blue))

(define d  (dot "red"))
(define +dot (pin-over grid 27.5 37.5 d))
(define +arrow (pin-arrow-line 7.0 +dot d cc-find d (lambda (p q) (values 0 0))))

(write-pict "distance0" +arrow)

;                                                                 
;                                                                 
;                       ;;                                        
;                        ;              ;      ;                  
;  ;;; ;    ;;;  ;; ;;   ; ;;    ;;;   ;;;;;  ;;;;;   ;;;  ;; ;;  
;   ; ; ;  ;   ;  ;;  ;  ;;  ;  ;   ;   ;      ;     ;   ;  ;;  ; 
;   ; ; ;   ;;;;  ;   ;  ;   ;   ;;;;   ;      ;      ;;;;  ;   ; 
;   ; ; ;  ;   ;  ;   ;  ;   ;  ;   ;   ;      ;     ;   ;  ;   ; 
;   ; ; ;  ;   ;  ;   ;  ;   ;  ;   ;   ;   ;  ;   ; ;   ;  ;   ; 
;  ;;;;;;;  ;;;;;;;; ;;;;;; ;;;  ;;;;;   ;;;    ;;;   ;;;;;;;; ;;;
;                                                                 
;                                                                 
;                                                                 
;                                                                 

(define o3 (dot "white"))
(define +o3 (pin-over +dot 30 0 o3))
(define +up (pin-arrow-line 7.0 +o3 
                            d (lambda (p q) (values 29 38))
                            d (lambda (p q) (values 00 38))))
(define +left 
  (pin-arrow-line 7.0 (cc-superimpose 
                       +up 
                       (blank (+ 20 (pict-width +up)) (+ 20 (pict-height +up))))
                  d (lambda (p q) (values 10 39))
                  d (lambda (p q) (values 10 10))))

(write-pict "distanceM" +left)