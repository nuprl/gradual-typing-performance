#lang scheme

(require slideshow mred/mred scheme/contract)

(define write/c (-> string? pict? pict?))

(provide/contract
 [write-ps write/c]
 [write-png write/c]
 [write-pict write/c
 ;; write the ps and png file for pict
             ])

(define (write-pict f p)
  (write-png f (write-ps f p)))

;; String Pict -> Pict 
;; effect: write the given pict to the specified file, return pict
(define (write-png file-name the-image)
  (define image-bm
    (make-platform-bitmap ; make-object bitmap% 
      (inexact->exact (round (pict-width the-image)))
      (inexact->exact (round (pict-height the-image)))))
  (define _ (unless (send image-bm ok?) (error 'write-and-return "bad image")))
  (define image-dc
    (new bitmap-dc% [bitmap image-bm]))
  (send image-dc clear)
  (send image-dc set-smoothing 'aligned)
  (draw-pict the-image image-dc 0.0 0.0)
  (send image-bm save-file (string-append file-name ".png") 'png)
  the-image)

;; String[filename] Pict -> Pict
(define (write-ps filename the-pict)
  (let ([ps-setup (new ps-setup%)])
    (send ps-setup copy-from (current-ps-setup))
    (send ps-setup set-file (string-append filename ".ps"))
    (let ([ps-dc (parameterize ((current-ps-setup ps-setup))
                   (make-object post-script-dc% #f #f #f #t))])
      (send ps-dc start-doc "")
      (send ps-dc start-page)
      (draw-pict the-pict ps-dc 0 0)
      (send ps-dc set-smoothing 'aligned)
      (send ps-dc end-page)
      (send ps-dc end-doc)
      the-pict)))