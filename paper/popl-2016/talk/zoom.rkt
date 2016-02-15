#lang racket/base

;; A library for zooming in and moving around on big picts on a slide

(require pict
         ppict)

(provide zoom-to
         tag-to-tag
         tag-to-full)

(define (zoom-to pict tag width height)
  (define-values (ltx lty)
    (lt-find pict (find-tag pict tag)))
  (define-values (rbx rby)
    (rb-find pict (find-tag pict tag)))
  (define pwidth (- rbx ltx))
  (define pheight (- lty rby))
  ;; attempt to fit to width by zooming
  (define factor (/ width pwidth))
  (scale (inset pict
                ltx lty
                (- (pict-width pict) rbx)
                (- (pict-height pict) rby))
         factor))

;; animate from tag to tag
(define ((tag-to-tag pict tag1 tag2 width height) n)
  (define-values (ltx1 lty1)
    (lt-find pict (find-tag pict tag1)))
  (define-values (rbx1 rby1)
    (rb-find pict (find-tag pict tag1)))
  (define-values (ltx2 lty2)
    (lt-find pict (find-tag pict tag2)))
  (define-values (rbx2 rby2)
    (rb-find pict (find-tag pict tag2)))
  (define pwidth1 (- rbx1 ltx1))
  (define pheight1 (- lty1 rby1))
  (define pwidth2 (- rbx2 ltx2))
  (define pheight2 (- lty2 rby2))
  ;; for now assume all sub-picts are the same size but
  ;; in general the scale has to change
  (define factor (/ width pwidth1))
  (scale (inset pict
                (+ (* (- 1 n) ltx1)
                   (* n ltx2))
                (+ (* (- 1 n) lty1)
                   (* n lty2))
                (- (pict-width pict)
                   (+ (* (- 1 n) rbx1)
                      (* n rbx2)))
                (- (pict-height pict)
                   (+ (* (- 1 n) rby1)
                      (* n rby2))))
         factor))

(define ((tag-to-full pict tag width height) n)
  (define-values (ltx lty)
    (lt-find pict (find-tag pict tag)))
  (define-values (rbx rby)
    (rb-find pict (find-tag pict tag)))
  (define pwidth (- rbx ltx))
  (define pheight (- lty rby))
  (define factor (/ width pwidth))
  (define target-factor (/ width (pict-width pict)))
  (scale (inset pict
                (* (- 1 n) ltx)
                (* (- 1 n) lty)
                (* (- 1 n)
                   (- (pict-width pict) (* rbx)))
                (* (- 1 n)
                   (- (pict-height pict) rby)))
         (+ (* (- 1 n) factor)
            (* n target-factor))))
