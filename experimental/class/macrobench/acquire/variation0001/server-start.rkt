#! /bin/sh
#|
exec racket -tm "$0" ${1+"$@"}
|#
#lang racket/gui

;; ---------------------------------------------------------------------------------------------------
;; server start up script 

(provide
  ;; $ ./server-start.rkt port# player#
  ;; accept player# players at port# port and then run an Acquire game 
  main)

(require "admin.rkt" "basics.rkt" "state.rkt" "remote-player.rkt" 
         "Lib/auxiliaries.rkt" "Lib/sandbox.rkt")

;; Port#String NatString -> Score 
;; use port to run Acquire server that signs up p# of players, runs a game, and repeats 
(define (main port#:string p#:string)
  (define port# (string->number port#:string))
  (define p# (string->number p#:string))
  (unless (and (integer? port#) (<= 0 port# 65535) (integer? p#) (<= MIN-PLAYER# p# MAX-PLAYER#))
    (error 'server-start "expected usage $ ./server-start port# player#"))
  (define tcp-listener (tcp-listen port# p# #t #f))
  (displayln `(listening on port ,port# for ,p# players))
  (let accept-players-and-run-games ()
    (define admin (new administrator% [next-tile randomly-pick]))
    (define c (make-custodian))
    (parameterize ((current-custodian c))
      (accept-players p# tcp-listener admin)
      (run-one-game admin))
    (custodian-shutdown-all c)
    (accept-players-and-run-games)))

;; Administrator -> Void 
;; run one game and print scores to stdout 
(define (run-one-game admin)
  (pretty-print `(ready to go with ,(send admin show-players)))
  (define-values (_1 score _2) (send admin run 99 #:show values #;(show)))
  (pretty-print score)
  (displayln (make-string 80 #\*)))

;; Nat TcpListener Administrator -> Void 
;; effect: accept tcp connections for p# players and have their proxies register with admin
(define (accept-players p# tcp-listener admin)
  (unless (= p# 0)
    (define-values (in out) (tcp-accept tcp-listener))
    (define remote-proxy-player (new remote-player% [in in][out out]))
    (in-sandbox (lambda () (send remote-proxy-player go admin))
                (lambda (_) (accept-players (- p# 1) tcp-listener admin))
                (lambda (status) (accept-players (- p# 1) tcp-listener admin)))))

;; -> (Nat Board -> Void)
;; display current state in a canvas 
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

;; --- run baby run 
(module+ run
  (main "51234" "4"))
