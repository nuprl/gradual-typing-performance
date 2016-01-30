#lang slideshow

(require slideshow/pict "write.ss" "pict-aux.ss")

(provide 
 struct-pict ;; String [Listof (list String String)] -> Pict 
 ;; (struct-pict s (list (list f1 c1) ...))
 ;; render struct-pict for struct of type s with fields f1 ... filled with 
 ;;   values v1 ... (possibly diagrams)
 
 draw-box ;; String String [Listof (list String String)] -> Pict 
 ;; (draw-box fl s (list (list f1 c1) ...))
 ;; create struct-pict for struct of type s with fields f1 ... filled with 
 ;;   values v1 ... (possibly diagrams)
 ;; effect: write it to specified file fl 
 
 )


(define (struct-pict name l #:color (c "black"))
  ;; String String -> Pict 
  ;; a field of a structure 
  (define (compartment label content)
    (define l (it-small label))
    (define c (if (pict? content) content (t content)))
    (define b (padded-frame (vl-append l c)))
    (pin-line b c lt-find c rt-find))  
  (define base
    (vr-append
     (padded-frame (it-small name))
     (frame (apply ht-append (map (curry apply compartment) l)))))
  (colorize (padded-blank base 3) c))

(define (draw-box filename struct-name field-values #:color (c "black"))
  (write-pict filename (struct-pict #:color c struct-name field-values)))