#lang racket/base

(provide
  (all-defined-out))

(require
  images/icons/arrow images/icons/control images/icons/misc images/icons/symbol images/icons/style
  racket/class
  racket/draw
  pict
  pict-abbrevs
  ppict/2
  (only-in racket/math pi)
  (only-in racket/string string-replace)
  (only-in plot/utils ->pen-color)
  (only-in slideshow current-font-size current-main-font margin client-w client-h t para))

(module+ test
  (require rackunit))

;; -----------------------------------------------------------------------------

(define HIGHLIGHT-COLOR (string->color% "RoyalBlue"))
(define WHITE (string->color% "white"))
(define BLACK (string->color% "black"))
(define GREY (string->color% "gray"))
(define GRAY GREY)
(define DYN-COLOR (string->color% "Firebrick"))
(define DYN-TEXT-COLOR (string->color% light-metal-icon-color))
(define STAT-COLOR (string->color% "PaleTurquoise"))
(define TAU-COLOR "DarkGoldenrod")
(define DEEP-COLOR (string->color% "DarkMagenta"))
(define SHALLOW-COLOR (string->color% "MediumSeaGreen"))
(define ERASURE-COLOR (string->color% "DarkOrange"))
(define TYPE-BOUNDARY-COLOR (string->color% "Fuchsia"))
(define CLOUD-COLOR (string->color% "Thistle"))
(define SURVEY-COLOR (string->color% "Gainsboro"))
(define BLANK-COLOR (string->color% "LightGoldenrodYellow"))

(define ALL-CAPS-FONT "Triplicate C3" #;"Bebas Neue")
(define MONO-FONT "Triplicate T4")
(define TITLE-FONT "Triplicate C4" #;"Fira Sans, Heavy")

(define NORMAL-FONT-SIZE 32)
(define SMALL-FONT-SIZE 28)
(define FOOTNOTE-FONT-SIZE 22)
(define TITLE-FONT-SIZE 44)
(define SUBTITLE-FONT-SIZE 40)
(define SUBSUBTITLE-FONT-SIZE 36)

(define SLIDE-TOP 1/10)
(define SLIDE-LEFT 1/50)
(define SLIDE-BOTTOM 4/5)
(define SLIDE-RIGHT (- 1 SLIDE-LEFT))
(define BODY-TOP 15/100)

(define HEADING-COORD (coord SLIDE-LEFT SLIDE-TOP 'lb))
(define -HEADING-COORD (coord SLIDE-RIGHT SLIDE-TOP 'rb))
(define CENTER-COORD (coord 1/2 1/2 'cc))
(define QUESTION-COORD (coord 98/100 1/2 'rc)) 
(define SMALL-ROUND -0.08)
(define SUBTITLE-MARGIN 20)
(define COMPONENT-MARGIN 40)
(define LINE-MARGIN 4)
(define INDENT-MARGIN 30)
(define COLUMN-MARGIN 70)

(define COMPONENT-ARROW-SIZE 11)
(define COMPONENT-ARROW-WIDTH 3)
(define TYPE-BOUNDARY-ARROW-SIZE 13)
(define TYPE-BOUNDARY-ARROW-WIDTH 4)
(define TYPE-BOUNDARY-TAG 'the-type-boundary)

(define STA-TAG 'sta-component)
(define DYN-TAG 'dyn-component)

(define (string*->text str* #:y-sep [y-sep 0])
  (if (string? str*)
    (t str*)
    (apply vl-append y-sep (map string*->text str*))))

(define (right-arrow [size 30] #:color [c #f])
  (maybe-colorize (arrow size 0) c))

(define (up-arrow [size 30] #:color [c #f])
  (maybe-colorize (arrow size (/ pi 2)) c))

(define (maybe-colorize p c)
  (if c (colorize p c) p))

(define (-pct num-yes num-no)
  (round (* 100 (/ num-yes (+ num-yes num-no)))))

(define (pixels->w% x)
  (/ x (+ (* 2 margin) client-w)))

(define (pixels->h% x)
  (/ x (+ (* 2 margin) client-h)))

(define (erase-pict p [alpha 0.2])
  (cellophane p alpha))

(define (snoc x* x)
  (let loop ((x* x*))
    (if (null? x*) (list x) (cons (car x*) (loop (cdr x*))))))

(module+ test
  (test-case "snoc"
    (check-equal? (snoc '() 'x) '(x))
    (check-equal? (snoc '(A B) 'C) '(A B C))))

(define (string->tag str)
  (string->symbol (string-downcase (string-replace str " " "-"))))

(module+ test
  (test-case "string->tag"
    (check-equal? (string->tag "hello") 'hello)
    (check-equal? (string->tag "hello world") 'hello-world)
    (check-equal? (string->tag "What hits?") 'what-hits?)))

(define (lines-append . arg*)
  (lines-append* arg*))

(define (lines-append* arg*)
  (apply vl-append (* 1/2 (current-font-size)) arg*))

(define (columns-append . arg*)
  (columns-append* arg*))

(define (columns-append* arg*)
  (apply ht-append (* 11/6 (current-font-size)) arg*))

(define (text/color str c)
  (text str (cons c (current-main-font)) (current-font-size)))

(define (dynt str)
  (text/color str DYN-TEXT-COLOR))

(define (string->title str #:size [size 55] #:color [color BLACK])
  (colorize (text str TITLE-FONT size) color))

(define (large-check-icon)
  (make-icon check-icon #:height 80))

(define (small-check-icon)
  (make-icon check-icon #:height 30))

(define (large-x-icon)
  (make-icon x-icon #:height 80))

(define (small-x-icon)
  (make-icon x-icon #:height 30))

(define (small-tau-icon)
  (make-icon tau-icon #:height 40))

(define (large-tau-icon)
  (make-icon tau-icon #:height 90))

(define tau-font
  (make-font #:smoothing 'unsmoothed #:family 'roman #:weight 'semibold))

(define (tau-icon #:color [c TAU-COLOR] #:height [h (default-icon-height)] #:material [m (default-icon-material)])
  (text-icon "τ" tau-font #:color c #:height h #:material m))

(define (small-lambda-icon)
  (make-icon lambda-icon #:height 40))

(define (large-lambda-icon)
  (make-icon lambda-icon #:height 90))

(define (make-large-focus-icon)
  (make-icon magnifying-glass-icon #:height 90))

(define (make-small-focus-icon)
  (make-icon magnifying-glass-icon #:height 50))

(define (make-icon i #:height h)
  (bitmap (i #:material plastic-icon-material #:height h)))

(define (title-text str [pre-size #f])
  (define size (or pre-size TITLE-FONT-SIZE))
  (text str TITLE-FONT size))

(define (subtitle-text str)
  (title-text str SUBTITLE-FONT-SIZE))

(define (subsubtitle-text str)
  (title-text str SUBSUBTITLE-FONT-SIZE))

(define (scale-q p)
  (scale-to-fit p (* 4/5 client-w) (* 4/5 client-h)))

(define (bigger-program p)
  (scale p 2))

(define (scale-logo p)
  (scale-to-fit p 180 80))

(define (add-tax v pct)
  (+ v (* v pct)))

(define (add-rectangle-background p #:radius [the-radius 10] #:x-margin [x-margin 0] #:y-margin [y-margin 0])
  (define-values [w h] (values (pict-width p) (pict-height p)))
  (define bg
    (filled-rounded-rectangle (add-tax w x-margin) (add-tax h y-margin) the-radius #:color WHITE #:draw-border? #false))
  (cc-superimpose bg p))

(define (add-rounded-border pp)
  (define-values [w h] (values (pict-width pp) (pict-height pp)))
  (define the-radius 10)
  (define frame
    (rounded-rectangle w h the-radius #:border-width 1 #:border-color BLACK))
  (cc-superimpose (add-rectangle-background pp #:radius the-radius) frame))

(define neu-logo (bitmap "src/neu-logo.png"))
(define nwu-logo (bitmap "src/nwu-logo.png"))
(define ctu-logo (bitmap "src/ctu-logo.png"))
(define igalia-logo (add-rectangle-background (bitmap "src/igalia-logo.png") #:x-margin 1/10 #:y-margin 1/10))

(define all-logo* (pict-bbox-sup* (map scale-logo (list neu-logo igalia-logo nwu-logo ctu-logo))))
