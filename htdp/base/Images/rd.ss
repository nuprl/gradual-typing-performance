#lang scheme/gui

(require slideshow/pict "write.ss" "pict-aux.ss")

(define-syntax-rule
  (def-color color) (define color (send the-color-database find-color (format "~a" 'color))))

; (def-color white)
; (define yellow (send the-color-database find-color "yellow"))
; (define black (send the-color-database find-color "black"))

;; draw the body of a russian doll (a cut of triangle]
(define (tri base height color)
  (dc (lambda (dc x y)
        (send dc draw-polygon 
                (list (make-object point% (* (- 1/2 1/9) base) 0)
                      (make-object point% (* (+ 1/2 1/9) base) 0)
                      (make-object point% base height)
                      (make-object point% 0 height))
                x y))
      base height))

;; [2,10) Color -> Pict
;; one instance of the russian doll
(define (rd s color)
  (define head (disk 10))
  (define body (tri 30 30 color))
  (define lft-foot (hb-append (filled-rectangle 5 2) (filled-rectangle 2 5)))
  (define rgt-foot (hb-append (filled-rectangle 2 5) (filled-rectangle 5 2)))
  (define feet (hb-append lft-foot (blank 5 2) rgt-foot))
  (define head+body (vc-append head body feet))
  (scale (colorize head+body color) s))

(write-pict "rd-red" (rd 1 "red"))

(write-pict "rd" (cc-superimpose (rd 3 "yellow") (rd 2 "green") (rd 1 "red")))
