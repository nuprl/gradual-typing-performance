#lang typed/racket/base

(require/typed "from.rkt"
  [nums (Listof Integer)]
  [funs (Listof (-> Boolean Boolean Boolean))]
  [idks (List Symbol String Integer (List))]
  [lam (-> (Listof Natural) (Listof Natural))])

(define (main)
  (apply + nums)
  ((car funs) #t #f)
  ;((cadr funs) (car funs) (car funs))
  (string-append
    (symbol->string (car idks))
    (cadr idks)
    (number->string (caddr idks))
    (if (eq? '() (cadddr idks)) "yolo" "no"))
  (lam '(8 6 7 5 3 0 9))
  (void))

(main)

;; -----------------------------------------------------------------------------
;; --- output
;; [BG:CREATE]	0	main.rkt	nums	from.rkt
;; [BG:APPLY]	0	main.rkt	nums	from.rkt
;; [BG:APPLY]	0	main.rkt	nums	from.rkt
;; [BG:APPLY]	0	main.rkt	nums	from.rkt
;; [BG:CREATE]	1	main.rkt	funs	from.rkt
;; [BG:CREATE]	2	main.rkt	idks	from.rkt
;; [BG:APPLY]	2	main.rkt	idks	from.rkt
;; [BG:APPLY]	2	main.rkt	idks	from.rkt
;; [BG:APPLY]	2	main.rkt	idks	from.rkt
;; [BG:APPLY]	2	main.rkt	idks	from.rkt
;; [BG:APPLY]	2	main.rkt	idks	from.rkt
;; [BG:CREATE]	3	main.rkt	lam	from.rkt
;; [BG:APPLY]	3	main.rkt	lam	from.rkt
;; [BG:APPLY]	3	main.rkt	lam	from.rkt
;; [BG:APPLY]	3	main.rkt	lam	from.rkt
;; [BG:APPLY]	3	main.rkt	lam	from.rkt
;; [BG:APPLY]	3	main.rkt	lam	from.rkt
;; [BG:APPLY]	3	main.rkt	lam	from.rkt
;; [BG:APPLY]	3	main.rkt	lam	from.rkt
