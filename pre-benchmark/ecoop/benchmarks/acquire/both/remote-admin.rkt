#lang racket

;; ---------------------------------------------------------------------------------------------------
;; remote proxy administrator and remote proxy turn
;; -- implement interface of administrator and player turn, respectively
;; -- communicates with remote proxy player on server side to implement admin/turn functionality 

(require "admin-intf.rkt") 

(admin& remote-administrator% remote-turn% DONE EXHAUSTED SCORE)

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION

(require (only-in "admin.rkt" DONE EXHAUSTED SCORE) "remote-actor.rkt" "state.rkt" "Lib/io.rkt")

(module+ test
  (require rackunit 
           "admin.rkt" "state.rkt" (only-in "player.rkt" create) "strategy.rkt"
           (submod "state.rkt" sample-states) (submod "board.rkt" tiles+spots)))

;; for the interaction diagram, see protocols.rkt -- the below are remote
;; proxies that simulate full-fledged administrators and turns on the
;; client computer so that the player does not need to change 

(define remote-administrator%
  (class/remote
   (init-field (next-tile void))

   (field (*player #f))
   
   ;; signup the client player and obtain unique name 
   (define/public (sign-up local-name player)
     (set! *player player)
     (handle (sign up)
             (xsend (signup-writer local-name))
             (set! name (signup-parser (xreceive)))
             (if (boolean? name) (error 'sign-up "server assigned bad name") name)))
   
   ;; produce the unique names of the players signed up so far
   (define/public (show-players)
     (list name))
   
   ;; run a game of Acquire with the currently client player 
   (define/public (run _t #:show (show values))
     (handle (run)
             ;; setup step
             (send *player setup (okay 'setup-step (state-parser (xreceive))))
             (xsend (void-writer))
             
             ;; accept turns, tiles, informational updates and keep queries until end message shows up
             (let loop ()
               (define msg (xreceive))
               (cond
                 [(keeps-parser msg)
                  => (lambda (keeps-msg) 
                       (xsend (booleans-writer (send *player keep keeps-msg)))
                       (loop))]
                 [(turn-parser msg)
                  => (lambda (turn-msg)
                       (define turn 
                         (new remote-turn% [in in][out out][current-state turn-msg][player *player]))
                       (define-values (t h o) (send *player take-turn turn))
                       (if (send turn place-called)
                           (xsend (order-writer o))
                           (xsend (turn-plain-writer t h o)))
                       ;; end of turn -- common to both 
                       ;; receive new tile 
                       (send *player receive-tile (okay 'receive-tile (tile-parser (xreceive))))
                       (xsend (void-writer))
                       (loop))]
                 [(state-parser msg)
                  => (lambda (inform-msg)
                       ;; get inform message 
                       (send *player inform inform-msg)
                       (xsend (void-writer))
                       (loop))]
                 [(end-parser msg) 
                  => (lambda (end)
                       (xsend (void-writer))
                       (values 'exhausted (rest end) `(,(first end))))]
                 [else (error 'run "remote proxy admin not prepared for this msg: ~e" msg)]))))))

(define remote-turn-administrator/c
  (class/c 
   ;; ------------------------------------------------------------------------------------------------
   ;; temporal contract
   (place-called
    ;; how often was the place method in this turn called
    (->m boolean?))))

(define/contract remote-turn%
  remote-turn-administrator/c
  (class/remote
   (init-field current-state player)
   
   (field 
    [board   (state-board current-state)]
    [current (state-current-player current-state)]
    [cash    (player-money current)]
    [tiles   (player-tiles current)]
    [shares  (state-shares current-state)]
    [hotels  (state-hotels current-state)]
    [players (state-players current-state)])
   
   ;; -----------------------------------------------------------------------------------------------
   (define *keep #f)
   
   (define/public (reconcile-shares bad-shares)
     (handle (reconcile shares)
             (set! current-state (state-sub-shares current-state bad-shares))
             (set! shares (state-shares current-state))))
   
   (define/public (place-called)
     *keep)
   
   (define/public (place t h)
     (handle (place t h)
             (set! *keep #t)
             (xsend (turn-merge-writer t h))
             (xsend (booleans-writer (send player keep (okay 'keep-in-turn (keeps-parser (xreceive))))))
             (okay 'place (players-parser (xreceive)))))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  ;; String ->* Admin IPort OPort RemotePlayer 
  (define (create-bundle s)
    (define p0 (create "hello" ordered-s))
    (define i0 (open-input-string s))
    (define o0 (open-output-string))
    (define r0 (new remote-administrator% [in i0][out o0]))
    (values p0 i0 o0 r0))
  
  (define-syntax-rule 
    (run s op)
    (begin s
           (let loop ((op2 (open-input-string (get-output-string op))))
             (define next (read-xml-from op2))
             (if (eof-object? next) '() (cons next (loop op2))))))
  
  ;; -------------------------------------------------------------------------------------------------
  (define-values (p0 i0 o0 r0) (create-bundle "<signup name=\"spieker11:hello\" />"))
  
  (check-equal? (send p0 go r0) (void))
  
  (define-values (p1 i1 o1 r1) 
    (create-bundle 
     (with-output-to-string
      (lambda ()
        ;; --- sign up 
        (define name "spieler11:hello")
        (write-xml-to (signup-writer name))
        ;; --- set up 
        (define s0 (s0-name name)) ;; player owns a single tile here: A1
        (write-xml-to (state-writer s0))
        (define t (new turn% (current-state s0)))
        ;; --- turn 1 
        (write-xml-to (turn-writer t))
        (write-xml-to (tile-writer D7))
        (define s0+A1 (state-place-tile s0 A1))
        (write-xml-to (state-writer s0))
        ;;; --- end 
        (write-xml-to (end-writer (state-score s0+A1) s0+A1))))))
  
  ; (read-xml-from i1)
  (send p1 go r1)
  
  (define (parse e . parsers)
    (let loop ((e e) (parsers parsers))
      (if (empty? parsers)
          (if (null? e) (void) e)
          (if ((first parsers) (first e))
              (loop (rest e) (rest parsers))
              (error 'parse "bad fit: ~a vs ~a" (object-name (first parsers)) (first e))))))
  
  (parse (run (send r1 run 1) o1)
         signup-parser 
         void-parser 
         turn-plain-parser 
         void-parser 
         void-parser
         void-parser))
