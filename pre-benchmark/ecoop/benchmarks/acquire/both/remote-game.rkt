#! /bin/sh
#|
exec racket -tm "$0" ${1+"$@"}
|#
#lang racket

;; ---------------------------------------------------------------------------------------------------
;; run many tests on remote server; starts remote server and then runs a bunch of plain/bad samples 

(provide
 ;; $ ./remote-game n
 ;; runs n random games via localhost, if n > 1 it also tests some really bad 'interactors'
 (rename-out (run main)))

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION 

(require "server-start.rkt" "player-factory.rkt" "remote-admin.rkt")

(define HOST "127.0.0.1")
(define PORT "51239")
(define PLAYERS# "4")

;; Nat -> Void 
;; run n rounds of random players with one ordered one and then some games with ill-behaving players
(define (run n:string)
  (define n (string->number n:string))
  ;; launch server 
  (define c (make-custodian))
  (parameterize ((current-custodian c))
    (void (thread (lambda () (main PORT PLAYERS#))))
    (sleep 1)
    ;; launch specified number of games 
    (for ((i (in-range n)))
      (go (first (random-players 1))))
    ;; launch bad games 
    (when (> n 1)
      (go (merge-bad-player))
      (go (keep-bad-player))
      (go (end-bad-player))
      (go (receive-bad-player))
      (go (inform-bad-player))
      (go (setup-bad-player))
      (go (inf-loop-player 1))))
  ;; tear down the server 
  (sleep 1)
  (custodian-shutdown-all c))

(define (go extra)
  (define c (make-custodian))
  (define all-threads
    (parameterize ((current-custodian c))
      (for/list ((p (cons extra (random-players 3))))
        (thread 
         (lambda ()
           (define-values (status score _state) (make-player HOST PORT p))
           score)))))
  ;; wait for a thread to shut down 
  (let wait ((l all-threads))
    (when (boolean? (apply sync/timeout 60 l)) (wait l)))
  ;; then clean up 
  (sleep 1)
  (custodian-shutdown-all c))

(define (make-player host port player)
  (define-values (in out) (tcp-connect host (string->number port)))
  (define remote-administrator (new remote-administrator% [in in][out out]))
  (send player go remote-administrator)
  (send remote-administrator run 0))

(module+ test 
  (run "1"))
