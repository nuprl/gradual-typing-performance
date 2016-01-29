;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mrtable) (read-case-sensitive #t) (teachpacks ((lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.ss" "teachpack" "2htdp")))))

(big-bang 0
          (on-tick add1)
          (record? true)
          (on-draw (lambda (x) 
                     (place-image (circle 3 'solid 'red)
                                  25 (sqr x)
                                  (empty-scene 50 50))))
          (stop-when (lambda (x) (>= (sqr x) 50))))