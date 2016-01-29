#lang slideshow

(require slideshow/pict "write.ss" "pict-aux.ss")

(define DELTA 40)
(define FT 12)

;; (U String Pict) (U String Pict) (U String Pict) String -> Pict
;; create a 3-state fsa (red, yellow, green) with transisitions labeled with lbl
(define (fsa rd yl gr txt)
  ;; String String -> Pict
  ;; surround txt with rounded rectangle and colorize
  (define (make-state txt)
    (define s (if (pict? txt) txt (t txt)))
    ;; the DELTA here is a good guess
    (define e (rounded-rectangle (if (pict? rd) (+ 5 (pict-width rd)) DELTA)
                                 (+ 10 (pict-height s))))
    (cc-superimpose s e))
  
  (define red (make-state rd))
  (define yel (make-state yl))
  (define grn (make-state gr))
  
  (define base (rectangle (+ (pict-width red) (* 2 DELTA))
                          (+ (pict-height red)
                             (pict-height yel)
                             (pict-height grn)
                             (* 3 DELTA))))
  
  (define +red (center base red (/ DELTA 2)))
  (define yel+pad (hc-append yel (blank (* 3/2 DELTA) (pict-height yel))))
  (define +yel 
    (center +red yel+pad (+ (/ DELTA 2) (pict-height red) DELTA)))
  (define +grn 
    (center +yel grn (+ (/ DELTA 2) 
                        (pict-height red) DELTA
                        (pict-height yel) DELTA)))
  
  (define lbl* (t-small txt))
  
  (define l1 (add-labeled-arrow +grn yel ct-find red lb-find lbl*))
  (define l2 (add-labeled-arrow l1 grn lt-find yel cb-find lbl*))
  (define l3 (add-labeled-arrow l2 red rb-find grn rt-find lbl*))
  
  l3)

;; Pict Pict Nat -> Pict
;; center p in base, at height y
(define (center base p y)
  (define width (pict-width base))
  (define w (pict-width p))
  (define d (quotient (- width w) 2))
  (pin-over base d y p))

;; {'red, 'yellow, 'green } -> Pict
;; turn on the given light 
(define (traffic-light c)
  (define fe filled-ellipse)
  (define ee ellipse)
  (define BULB 10)
  (define red (colorize ((if (eq? c 'red) fe ee) BULB BULB) "red"))
  (define yel (colorize ((if (eq? c 'yellow) fe ee) BULB BULB) "yellow"))
  (define grn (colorize ((if (eq? c 'green) fe ee) BULB BULB) "green"))
  (define spacer (blank (/ BULB 2) BULB))
  (define base (hc-append red spacer yel spacer grn))
  (frame 
   (cc-superimpose (blank (+ 5 (pict-width base)) (+ 5 (pict-height base)))
                   base)))

(define red (write-pict "traffic-red" (traffic-light 'red)))
(define yel (write-pict "traffic-yellow" (traffic-light 'yellow)))
(define grn (write-pict "traffic-green" (traffic-light 'green)))

(write-pict "traffic-real" (fsa red yel grn "time"))
; (write-pict "traffic-real" (fsa red yel grn "time"))
(write-pict "traffic-rep"  (fsa "\"red\"" "\"yellow\"" "\"green\"" "tick"))

(provide fsa) 
