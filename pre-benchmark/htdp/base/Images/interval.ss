#lang scheme

#|
I've finally found an approach to curve drawing for picts that I think
works well, so I've added it to `slideshow'.

The `pin-line' function now takes an angle for the exit and entry
points (defaulting to the angle of a straight line between the points),
and it also accepts a "pull" for each point (defaulting to 1/4 the
distance between the points).

So, if you have a `pin-line' call, but you want the line to start out
going straight to the right, then you just and add `#:start-angle 0' to
the call. The line will start straight to the right, then curve around
to hit the end point. Similarly, if you want the line to end going
straight down, add `#:end-angle (* pi -1/2)'.

Each angle and pull is really a polar-coordinate vector specifying the
displacement to a Bezier control point from each end point. In
retrospect, this is a completely obvious interface (at least now that
we have a drawing operation for Bezier curves).
|#

(require slideshow mred/mred "write.ss" "pict-aux.ss")

(define h1 20)
(define w1 20)

(define-values (LP RP LB RB)
  (apply values
         (map (lambda (x) (cc-superimpose (blank w1 h1) (bt-large x)))
              '("(" ")" "[" "]")))) 

;; Nat -> Pict 
;; create a numberline with n dashes 
(define (numberline n open L close R)
  (define X (blank w1 h1))
  (define Y (blank w1 h1))
  (define mmm (apply hc-append 
                     (append 
                      (list Y)
                      (build-list n
                                  (lambda (i)
                                    (cond
                                      [(= i open)  L]
                                      [(= i close) R]
                                      [else (vline w1 h1)])))
                      (list X))))
  (define (one i) (cc-superimpose (blank w1 h1) (t (number->string i))))
  (define kkk (apply hc-append (build-list n one)))
  (define lnn (hline (* w1 n) h1))
  (define img (vc-append (cc-superimpose mmm lnn) kkk))
  (pin-arrow-line 4.0 img Y lc-find X rc-find))

(map
 (lambda (lft rgt) 
   (write-png 
    (string-append (symbol->string lft) "-" (symbol->string rgt))
    (numberline 12 3 (if (eq? lft 'open) LP LB) 5 (if (eq? rgt 'open) RP RB))))
 '(closed closed open open)
 '(closed open closed open))
