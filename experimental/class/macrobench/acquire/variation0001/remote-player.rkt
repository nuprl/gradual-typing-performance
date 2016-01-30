#lang racket

;; ---------------------------------------------------------------------------------------------------
;; remote proxy player, implements player interface
;; -- communicates with remote-admin on client side to implement player functionality 

(require "admin-intf.rkt")

(provide
 (contract-out
  (remote-player% player/c)))

;; ---------------------------------------------------------------------------------------------------
(require "remote-actor.rkt" "Lib/io.rkt")

(module+ test
  (require rackunit "admin.rkt" "basics.rkt" "state.rkt" 
           (submod "state.rkt" sample-states) (submod "board.rkt" tiles+spots)))

;; for the interaction diagram, see protocols.rkt -- the below is a remote
;; proxy that simulates a full-fledged player on the server computer so
;; that the admin does not need to change 

(define remote-player%
  (class/remote
   (field (choice void))
   (define/public (go a)
     (with-handlers ((exn:fail:network? (lambda (x) (log x `(remote sign up failed ,name)))))
       (set! name (send a sign-up (okay 'go (signup-parser (xreceive))) this))
       (xsend (signup-writer name))))
   
   (define/public (setup s)
     (handle (setting up)
             (xsend (state-writer s))
             (okay 'setup (void-parser (xreceive)))))
   
   (define/public (take-turn t)
     (handle (take turn)
             (xsend (turn-writer t))
             (define response (xreceive))
             (define plain-turn (turn-plain-parser response))
             (cond
               [plain-turn (apply values plain-turn)]
               [else 
                (define action (turn-merge-parser response))
                (define tile (first action))
                (define hotel (second action))
                (xsend (players-writer (send t place tile hotel)))
                (values tile hotel (okay 'take-turn-order (order-parser (xreceive))))])))
   
   (define/public (keep loh)
     (handle (keep)
             (xsend (keeps-writer loh))
             (okay 'keep (booleans-parser (xreceive)))))
   
   (define/public (receive-tile t)
     (handle (receive tile)
             (xsend (tile-writer t))
             (okay 'receive-tile (void-parser (xreceive)))))
   
   (define/public (inform s)
     (handle (inform)
             (xsend (state-writer s))
             (okay 'inform (void-parser (xreceive)))))
   
   (define/public (the-end st sc)
     (handle (the end)
             (xsend (end-writer sc st))
             (okay 'the-end (void-parser (xreceive)))))))

(module+ test
  
  ;; String ->* Admin IPort OPort RemotePlayer 
  (define (create-bundle s)
    (define a0 (new administrator% [next-tile first]))
    (define i0 (open-input-string s))
    (define o0 (open-output-string))
    (define r0 (new remote-player% [in i0][out o0]))
    (values a0 i0 o0 r0))
  
  (define-syntax-rule 
    (run s op)
    (begin s (read-xml-from (open-input-string (get-output-string op)))))
  
  ;; -------------------------------------------------------------------------------------------------
  (define-values (a0 i0 o0 r0) (create-bundle "<signup name=\"hello\" />"))
  (check-true
   (cons? (regexp-match #px"(spieler|player)\\d\\d:hello" (signup-parser (run (send r0 go a0) o0)))))
  
  ;; -------------------------------------------------------------------------------------------------
  (define-values (a1 i1 o1 r1) (create-bundle "<void />"))
  (check-equal? (state-parser (run (send r1 setup s0) o1)) s0)
  
  ;; -------------------------------------------------------------------------------------------------
  (define plain0 (with-output-to-string (lambda () (write-xml-to (turn-plain-writer A1 #f '())))))
  (define-values (a2 i2 o2 r2) (create-bundle plain0)) ;; players place A1 no hotel no order
  (define t (new turn% (current-state s0)))
  (define-values (_t1 _h1 _o1) (send r2 take-turn t))
  
  ; (state-place-tile s2 C3 TOWER)
  (define merge0
    (with-output-to-string
     (lambda ()
       (write-xml-to (turn-merge-writer C3 TOWER))
       (write-xml-to (booleans-writer '(#t)))
       (write-xml-to (order-writer '())))))
  (define-values (am im  om  ep2a) (create-bundle merge0))
  (set-field! name ep2a "ep2a")
  (define merge2 (with-output-to-string (lambda () (write-xml-to (booleans-writer '(#t))))))
  (define-values (_1 bim bom ep2b) (create-bundle merge2))
  (set-field! name ep2b "ep2b")
  (define-values (_2 cim com ep2c) (create-bundle merge2))
  (set-field! name ep2c "ep2c")
  
  (define s2:s (s2 ep2a ep2b ep2c))
  
  (define t2 (new turn% [current-state s2:s]))
  (define-values (_t2 _h2 _o2) (send ep2a take-turn t2))
  (check-equal? C3 _t2)
  (check-equal? TOWER _h2)
  (check-equal? '() _o2)
  (check-true (state? (turn-parser (run (void) om))))
  
  ;; -------------------------------------------------------------------------------------------------
  (define hotels (list AMERICAN TOWER FESTIVAL))
  (define boolns (map (lambda (x) #f) hotels))
  (define b:str* (with-output-to-string (lambda () (write-xml-to (booleans-writer boolns)))))
  (define-values (a3 i3 o3 r3) (create-bundle b:str*))
  
  ;; -------------------------------------------------------------------------------------------------
  (define r (send r3 keep hotels))
  (check-equal? r boolns)
  (check-equal? (keeps-parser (run (void) o3)) hotels)
  
  ;; -------------------------------------------------------------------------------------------------
  (define-values (a4 i4 o4 r4) (create-bundle "<void />"))
  (check-equal? (state-parser (run (send r4 inform s1) o4)) s1)
  
  ;; -------------------------------------------------------------------------------------------------
  (define-values (a5 i5 o5 r5) (create-bundle "<void />"))
  (check-equal? (tile-parser (run (send r5 receive-tile A7) o5)) A7)
  
  ;; -------------------------------------------------------------------------------------------------
  (define-values (a6 i6 o6 r6) (create-bundle "<void />"))
  (define score (state-score s1))
  (check-equal? (end-parser (run (send r6 the-end s1 score) o6)) (cons s1 score)))
