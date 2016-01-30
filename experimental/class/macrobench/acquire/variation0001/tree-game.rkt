#! /bin/sh
#|
exec racket -tm "$0" ${1+"$@"}
|#

;#lang racket/gui
#lang racket

;; ---------------------------------------------------------------------------------------------------
;; run tests with local games and infinite loop player, test runs even more 

(provide
  ;; $ ./tree-game n
  ;; runs n random games with one player that goes into an infinite loop 
  main) 

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION

(require "admin.rkt" "state.rkt" "player-factory.rkt" "Lib/auxiliaries.rkt")

#;(module+ test (require rackunit))
(require contract-profile)

(define (main n num-turns)
  (for ((i (in-range (string->number n))))
    (go num-turns (first (ordered-players 1)) #;(inf-loop-player 0))))

(define (go num-turns extra)
  ;(define p1 (random-players 5))
  ;(define p1 (random-players 1))
  ; use ordered-player no randomness
  (define p1 (ordered-players 5))
  (define p (cons extra p1))
  ; make game deterministic always pick first tile
  (define-values (two-status _score two-run) (run p num-turns #;(#:choice randomly-pick)))
  (displayln `(,(length two-run) ,two-status)))

(define (run players turns# #:show (show void #;(show)) #:choice (choose-next-tile first))
  (define a (new administrator% (next-tile choose-next-tile)))
  (for ((p players)) (send p go a))
  (send a run turns# #:show show))

;; -> (Nat Board -> Void)
(define (show)
  ;;bg;
  (lambda (n state) (void))
  #;(parameterize ((current-eventspace (make-eventspace)))
    (define frame  (new frame% [label "Acquire Game"][width 1000][height 1000]))
    (define paste  (new pasteboard%))
    (define canvas (new editor-canvas% [parent frame][editor paste]))
    (send frame show #t)
    (lambda (n state)
      (send paste begin-edit-sequence)
      (send paste select-all)
      (send paste clear)
      (send paste insert (state-draw state) 0 0)
      (send paste end-edit-sequence)
      (sleep 1))))

