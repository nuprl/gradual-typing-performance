#lang typed/racket

(provide
 ;; String 
 EOM
 DONE
 
 ;; constants, regexps that match PATH, DISABLE, and ENABLE requests
 PATH 
 DISABLE
 ENABLE
 
 ;; InputPort OutputPort -> Void 
 ;; read FROM, DISABLE, and ENABLE requests input-port, write responses to output-port, loop
 run-t)

;; ===================================================================================================
(require benchmark-util)
(require/typed/check "t-view.rkt" [manage% Manage])
(require "../base/t-view-types.rkt")
; (require/typed "t-view.rkt" [manage% Manage])
(module+ test (require typed/rackunit))

(define PATH    #rx"from (.*) to (.*)$")
(define DISABLE #rx"disable (.*)$")
(define ENABLE  #rx"enable (.*)$")

(define DONE    "done")
(define EOM     "eom")

(: manage [Instance Manage])
(define manage (new manage%))

(: run-t (-> Input-Port Output-Port Thread))
(define (run-t [input-port (current-input-port)] [output-port (current-output-port)])
  (thread 
   (lambda ()
     (parameterize ([current-input-port input-port]
                    [current-output-port output-port])
       (define next (read-line))
       (unless (eof-object? next)
         (cond
           [(regexp-match PATH next)
            => (lambda (x)
                 (define y (cast x [List String String String]))
                 (displayln (send manage find (second y) (third y))))]
           [(regexp-match DISABLE next)
            => (lambda (x) 
                 (define y (cast x [List String String]))
                 (status-check add-to-disabled (second y)))]
           [(regexp-match ENABLE next)
            => (lambda (x)
                 (define y (cast x [List String String]))
                 (status-check remove-from-disabled (second y)))]
           [else (displayln "message not understood")])
         (displayln EOM)
         (flush-output))))))

(define-syntax-rule
  (status-check remove-from-disabled enabled)
  (let ([status (send manage remove-from-disabled enabled)])
    (if (boolean? status) 
        (displayln DONE)
        (displayln status))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (define DONE-RESULT (string-append DONE "\n"))
  (: run-run-t-on (-> String String))
  (define (run-run-t-on request)
    (define r
      (with-input-from-string 
       request
       (lambda ()
         (with-output-to-string
          (lambda ()
            (sync (run-t (current-input-port) (current-output-port))))))))
    ;; strip extra newline & EOM 
    (define prep (substring r 0 (- (string-length r) (string-length EOM) 1)))
    prep))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; imperative lookup test
  
  (define DISABLE-GOVERNMENT "disable Government")
  (define ENABLE-GOVERNMENT "enable Government")
  
  (define AIRPORT-TO-NORTHEASTERN "from Airport to Northeastern")
  (define AIRPORT-TO-NORTHEASTERN-GOV-DISABLED
    #<< here
Airport Station, take blue
Maverick Station, take blue
Aquarium Station, take blue
State Station, take blue
---switch from blue to orange
Downtown Crossing Station, take orange
---switch from orange to red
Park Street Station, take red
---switch from red to green
Boylston Street Station, take green
Arlington Station, take green
Copley Station, take green
---ensure you are on E
Prudential Station, take E
Symphony Station, take E
Northeastern University Station, take E

 here
    )
  
  ;; set up 
  (check-equal? (run-run-t-on DISABLE-GOVERNMENT) DONE-RESULT)
  (check-equal? (run-run-t-on AIRPORT-TO-NORTHEASTERN) AIRPORT-TO-NORTHEASTERN-GOV-DISABLED)
  ;; tear down; if this fails the remaining tests fail, too
  (check-equal? (run-run-t-on ENABLE-GOVERNMENT) DONE-RESULT))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; functional lookup tests
  
  (define AIRPORT-TO-NORTHEASTERN-PATH
    #<< here
Airport Station, take blue
Maverick Station, take blue
Aquarium Station, take blue
State Station, take blue
Government Center Station, take blue
---switch from blue to green
Park Street Station, take green
Boylston Street Station, take green
Arlington Station, take green
Copley Station, take green
---ensure you are on E
Prudential Station, take E
Symphony Station, take E
Northeastern University Station, take E

 here
    )
  (check-equal? (run-run-t-on AIRPORT-TO-NORTHEASTERN) AIRPORT-TO-NORTHEASTERN-PATH))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; functional lookup tests
  (define RUGGLES-TO-CENTRE "from Ruggles to Newton Centre")
  (define RUGGLES-TO-CENTRE-PATH 
    #<< here
Ruggles Station, take orange
Massachusetts Avenue Station, take orange
Back Bay Station, take orange
Tufts Medical Center Station, take orange
Chinatown Station, take orange
Downtown Crossing Station, take orange
State Station, take orange
Haymarket Station, take orange
---switch from orange to green
Government Center Station, take green
Park Street Station, take green
Boylston Street Station, take green
Arlington Station, take green
Copley Station, take green
---ensure you are on C D B
Hynes Convention Center, take C D B
Kenmore Station, take C D B
---ensure you are on D
Longwood Station, take D
Brookline Village Station, take D
Brookline Hills Station, take D
Beaconsfield Station, take D
Reservoir Station, take D
Chestnut Hill Station D Riverside Line, take D
Newton Centre Station, take D

 here
    )
  
  (check-equal? (run-run-t-on RUGGLES-TO-CENTRE) RUGGLES-TO-CENTRE-PATH))
