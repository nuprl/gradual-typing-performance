#! /bin/sh
#|
exec racket -tm "$0" ${1+"$@"}
|#

#lang racket/gui

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


(define (main n)
  (for ((i (in-range (string->number n))))
    (go (first (ordered-players 1)) #;(inf-loop-player 0))))

(define (go extra)
  ;(define p1 (random-players 5))
  ;(define p1 (random-players 1))
  ; use ordered-player no randomness
  (define p1 (ordered-players 5))
  (define p (cons extra p1))
  ; make game deterministic always pick first tile
  (define-values (two-status _score two-run) (run p 99 #;(#:choice randomly-pick)))
  (displayln `(,(length two-run) ,two-status)))

(define (run players turns# #:show (show void #;(show)) #:choice (choose-next-tile first))
  (define a (new administrator% (next-tile choose-next-tile)))
  (for ((p players)) (send p go a))
  (send a run turns# #:show show))

;; -> (Nat Board -> Void)
(define (show)
  (parameterize ((current-eventspace (make-eventspace)))
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

#;(module+ test
  (define-values (status _score0 test) (run (ordered-players 3) 4))
  (check-equal? (length test) 5)
 
  (define-values (one-status _score1 one-run) (run (ordered-players 6) 99))
  (check-equal? (length one-run) 25) ; 23
  (check-equal? one-status 'score)
  
  (define-values (two-status _score2 two-run) (run (random-players 6) 99 #:choice randomly-pick))
  (length two-run)
  two-status

)

#;(module+ test 
  (go (merge-bad-player))
  (go (keep-bad-player))
  (go (end-bad-player))
  (go (receive-bad-player))
  (go (inform-bad-player))
  (go (setup-bad-player))
  (go (inf-loop-player 1)))
