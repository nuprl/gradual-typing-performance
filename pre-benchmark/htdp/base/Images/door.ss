#lang slideshow

(require slideshow/pict "write.ss" "pict-aux.ss")

(define DELTA 40)

;; String String String String String String String -> Pict 
(define (fsa L C O tick unlock lock push)
  ;; String -> Pict
  (define (make-state txt)
    (define s (t txt))
    (define e (rounded-rectangle (+ 10 (pict-width s)) (+ 10 (pict-height s))))
    (cc-superimpose s e))
  
  ;; Pict Pict Nat -> Pict 
  (define (center base state y)
    (define w (pict-width state))
    (define d (quotient (- width w) 2))
    (pin-over base d y state))
  
  (define locked (make-state L))
  (define closed (make-state C))
  (define open   (make-state O))
  
  (define base (rectangle (+ DELTA (pict-width locked) DELTA)
                          (+ (pict-height locked) DELTA 
                             (pict-height closed) DELTA 
                             (pict-height open) DELTA)))
  
  (define width (pict-width base))
  
  (define +locked (center base locked (/ DELTA 2)))
  (define +closed (center +locked closed (+ (/ DELTA 2) (pict-height locked) DELTA)))
  (define +open   (center +closed open (+ (/ DELTA 2) DELTA (pict-height locked) DELTA (pict-height closed))))
  
  (define l1 (add-labeled-arrow +open locked lb-find closed lt-find unlock))
  (define l2 (add-labeled-arrow l1 closed lb-find open lt-find push))
  (define l3 (add-labeled-arrow l2 open rt-find closed rb-find tick))
  (define l4 (add-labeled-arrow l3 closed rt-find locked rb-find lock))
  l4)

(define f (curry map (lambda (x) (t x))))

(write-pict "door-real"
            (apply fsa "locked" "closed" "open" (f '("*time*" "unlock" "lock" "push"))))

(define g (curry map (curry format "~s")))

(write-pict "door-simu" 
            (apply fsa 
                   (append 
                    (g '("locked" "closed" "open"))
                    (map (curry hc-append (blank 20 0))
                         (f (cons "*tick*" (g '("u" "l" " "))))))))
