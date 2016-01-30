#! /bin/sh
#|
exec racket -tm "$0" ${1+"$@"}
|#
#lang racket

;; ---------------------------------------------------------------------------------------------------
;; script for starting up a named player via 
;;   $ ./client-start HOST PORT NAME-of-player 

(provide
  ;; $ ./client-start.rkt ip-or-hostname port# name 
  ;; create random player (name) and connect to Acquire server at ip-or-hostname on port#
  (rename-out (make-player main)))

;; ---------------------------------------------------------------------------------------------------
(require "player.rkt" "strategy.rkt" "remote-admin.rkt")

(define (make-player host port name)
  (define player (create name random-s))
  (define-values (in out) (tcp-connect host (string->number port)))
  (define remote-administrator (new remote-administrator% [in in][out out]))
  (send player go remote-administrator)
  (send remote-administrator show-players)
  (displayln `(signed up ,name))
  (send remote-administrator run 0)) 

(define HOST "192.162.28.178") ; "127.0.0.1"
(define PORT "51234")

;; --- test run 
(define (go)
  (define c (make-custodian))
  (define all-threads
    (parameterize ((current-custodian c))
      (for/list ((name '("a" "b" "c" "d")))
        (thread 
         (lambda ()
           (define-values (status score _state) (make-player HOST PORT name)) 
           score)))))
  (let wait ((l all-threads))
    (when (cons? l) (wait (remove (apply sync l) l))))
  (custodian-shutdown-all c))
