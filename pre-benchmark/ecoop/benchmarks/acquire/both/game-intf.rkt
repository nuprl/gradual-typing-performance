#lang racket

;; ---------------------------------------------------------------------------------------------------
;; interface specification for integration testers 

(require 
 "admin.rkt" "basics.rkt" "board.rkt" (only-in "state.rkt" state?) "player.rkt" "Lib/contract.rkt")
 
(interface game&
  (run 
   ;; (run lp n) 
   ;; run n rounds of a game of Aquire with the external players in lp
   ;; (run lp n #:show show)
   ;; .. like above, but use show to display the current board 
   (->i ((lp (and/c (listof player?) (lambda (lp) (<= MIN-PLAYER# (length lp) MAX-PLAYER#))))
         (n natural-number/c))
        (#:show (show (-> natural-number/c board? any)))
        (values (status (or/c IMPOSSIBLE DONE EXHAUSTED SCORE)) 
                (los (listof state?))))))
       
