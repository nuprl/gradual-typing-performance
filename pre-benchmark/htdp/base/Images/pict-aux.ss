#lang scheme/gui

(require #; scheme/contract slideshow)

(provide t-small it-small bt-large)

(define NORMAL 12)
(define SMALL  10)
(define LARGE  14)

(current-font-size NORMAL)

(define (t-small x) (parameterize ((current-font-size SMALL)) (t x)))
(define (it-small x) (parameterize ((current-font-size SMALL)) (it x)))
(define (bt-large x) (parameterize ((current-font-size LARGE)) (bt x)))


;; a general find contract
(define find/c [->* (pict? pict?) () (values number? number?)])

(provide/contract
 [clip-to-ellipse (-> pict? number? number? pict?)
  ;; clip given picture (usually an ellispe) to ellispe (w,h)
  ])

(define (clip-to-ellipse p w h)
  (let ([drawer (make-pict-drawer p)]
        [δx (/ (- (pict-width p) w) 2)]
        [δy (/ (- (pict-height p) h) 2)])
    (cc-superimpose
     (dc
      (λ (dc dx dy)
        (let ([old-clip (send dc get-clipping-region)]
              [new-clip (new region% [dc dc])])
          (send new-clip set-ellipse (+ dx δx) (+ dy δy) w h)
          (send dc set-clipping-region new-clip)
          (drawer dc dx dy)
          (send dc set-clipping-region old-clip)))
      (pict-width p)
      (pict-height p)
      (pict-ascent p)
      (pict-descent p))
     (ghost p))))

(provide/contract
 [add-labeled-arrow 
  (-> pict?
      pict? find/c
      pict? find/c 
      pict?
      pict?)
  ;; add arrow from f* in nx to t* in base, label it at "center" with txt
  ]
 )

(define (add-labeled-arrow base f* f*-find t* t*-find lbl)
  (define-values (x0 y0) (f*-find base f*))
  (define-values (x1 y1) (t*-find base t*))
  (define wlbl (pict-width lbl))
  (define hlbl (pict-height lbl))
  (define x (+ x0 (/ (- x1 x0 wlbl) 2)))
  (define y (+ y0 (/ (- y1 y0 hlbl) 2)))
  (pin-over (pin-arrow-line 4.0 base f* f*-find t* t*-find) x y lbl))

(provide/contract
 [padded-frame 
  (-> pict? pict?)
  ;; add some white space around the picture and frame it 
  ])

(define (padded-frame p)
  (define DELTA-Y 3)
  (define DELTA-X (* 2 DELTA-Y))
  (define w (pict-width p))
  (define h (pict-height p))
  (frame (cc-superimpose (blank (+ DELTA-X w) (+ DELTA-Y h)) p)))

(provide/contract
 [padded-blank
  (-> pict? number? pict?)
  ;; add some white space around the picture 
  ])

(define (padded-blank p n)
  (define w (pict-width p))
  (define h (pict-height p))
  (cc-superimpose (blank (+ n w) (+ n h)) p))
