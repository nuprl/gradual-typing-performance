#lang racket 

;; ---------------------------------------------------------------------------------------------------
;; basic remote connectivity functionality, including a base class for remote proxy classes 

(require "remote-actor-intf.rkt")

(remote-connectivity&
  class/remote
  
  signup-parser signup-writer void-parser void-writer tile-parser tile-writer  
  turn-parser turn-writer state-parser state-writer turn-plain-parser turn-plain-writer
  turn-merge-parser turn-merge-writer placement-parser placement-writer order-parser order-writer 
  keeps-parser keeps-writer booleans-parser booleans-writer players-parser players-writer
  end-parser end-writer)

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION

(require "admin.rkt" "basics.rkt" "board.rkt" "state.rkt" "Lib/io.rkt" "Lib/log.rkt" "Lib/xml.rkt")

(module+ test 
  (require rackunit (submod "board.rkt" tiles+spots) (submod "state.rkt" sample-states)))

;; ---------------------------------------------------------------------------------------------------

(define signup-parser 
  (xml-parser (signup ((name values)) #:action name)))

(define (signup-writer name)
  `(signup ((name ,name))))

(module+ test 
  (check-equal? (signup-parser (signup-writer "Hello")) "Hello"))

;; ---------------------------------------------------------------------------------------------------

(define void-parser
  (xml-parser (void () #:action (void))))

(define (void-writer)
  `(void ()))

(module+ test
  (check-equal? (void-parser (void-writer)) (void)))

;; ---------------------------------------------------------------------------------------------------

(define (tile-writer t)
  (tile->xexpr t))

(define tile-parser 
  (xml-parser (tile ((column string->column) (row string->row)) #:action (tile column row))))

(module+ test
  (check-equal? (tile-parser (tile-writer H11)) H11))

;; ---------------------------------------------------------------------------------------------------

(define (turn-writer t)
  (define s (state->xexpr (get-field current-state t)))
  (list* 'turn '() (rest (rest s))))

(define turn-parser
  (xml-parser
   (turn () (b board-parser) (p player-parser) ... #:action (*create-state b p))))

(module+ test 
  (define t0 (new turn% [current-state s0]))
  (check-equal? (turn-parser (turn-writer t0)) s0)
  
  (define t1 (new turn% [current-state s1]))
  (check-equal? (turn-parser (turn-writer t1)) s1))

;; ---------------------------------------------------------------------------------------------------

(define (state-writer s)
  (state->xexpr s))

(define state-parser 
  (xml-parser (state () (b board-parser) (p player-parser) ... #:action (*create-state b p))))

(define board-parser
  (xml-parser
   (board () (t tile-parser) ... (h boardhotel-parser) ... #:action (*create-board-with-hotels t h))))

(define player-parser
  (xml-parser
   (player ((name values) (cash string->cash)) (s share-p) ... (t tile-parser) ...
           #:action (*create-player name cash (*combine-shares s) t))))

(define boardhotel-parser 
  (xml-parser
   (hotel ((name string->hotel)) (t1 tile-parser) (t2 tile-parser) (t tile-parser) ... 
          #:action (list* name t1 t2 t))))

(define share-p
  (xml-parser
   (share ((name string->hotel) (count string->count)) #:action (*create-shares name count))))

(module+ test 
  (define a-player (first (state-players s0)))
  (check-equal? (player-parser (player->xexpr a-player)) a-player)
  
  (define a-board (state-board s0))
  (check-equal? (board-parser (board->xexpr a-board)) a-board)
  
  (check-equal? (state-parser (state-writer s0)) s0)
  (check-equal? (state-parser (state-writer s1)) s1))

;; ---------------------------------------------------------------------------------------------------

(define (turn-plain-writer t h o)
  `(pbuy () ,(placement-writer t h) ,(order-writer o)))

(define turn-plain-parser 
  (xml-parser
   (pbuy () (placement placement-parser) (order order-parser) #:action (append placement `(,order)))))

(define (turn-merge-writer t h)
  (placement-writer t h))

(define (turn-merge-parser x) 
  (define r (placement-parser x))
  (unless (or (boolean? r) (second r)) (error 'turn-merge-parser "bad merge response: ~e" x))
  r)

(define (placement-writer t h)
  (if h
      `(placement () ,(tile->xexpr t) ,(xhotel-writer h))
      `(placement () ,(tile->xexpr t))))

(define placement-parser
  (xml-parser 
   (placement () (tile tile-parser) #:action (list tile #f))
   (placement () (tile tile-parser) (hotel xhotel-parser) #:action (list tile hotel))))

(define (order-writer loh)
  `(order () ,@(map xhotel-writer loh)))

(define order-parser
  (xml-parser (order () (xhotel xhotel-parser) ... 
                     #:action 
                     (if (<= (length xhotel) SHARES-PER-TURN#)
                         xhotel
                         (error 'order-parser "FIX: mistake in protocol specification")))))

(define (xhotel-writer h)
  (hotel->xexpr h))

(define xhotel-parser
  (xml-parser (hotel ((label string->hotel)) #:action label)))

(module+ test
  (check-equal? (xhotel-parser (xhotel-writer AMERICAN)) AMERICAN)
  
  (check-equal? (order-parser (order-writer `())) `())
  (check-equal? (order-parser (order-writer `(,AMERICAN))) `(,AMERICAN))
  (check-equal? (order-parser (order-writer `(,AMERICAN ,TOWER))) `(,AMERICAN ,TOWER))
  
  (check-equal? (placement-parser (placement-writer I12 FESTIVAL)) `(,I12 ,FESTIVAL))
  (check-equal? (placement-parser (placement-writer I12 #f)) `(,I12 #f))
  
  (check-equal? (turn-plain-parser (turn-plain-writer B3 #f `(,TOWER))) `(,B3 ,#f (,TOWER)))
  (check-false (turn-plain-parser (turn-merge-writer B3 TOWER)))
  (check-equal? (turn-merge-parser (turn-merge-writer B3 TOWER)) `(,B3 ,TOWER)))

;; ---------------------------------------------------------------------------------------------------

(define (keeps-writer loh)
  `(keep () ,@(map xhotel-writer loh)))

(define keeps-parser 
  (xml-parser (keep () (xhotel xhotel-parser) ... 
                    #:action 
                    (if (<= 1 (length xhotel) 3) ;; <--- MAGIC: max number of hotels acquired
                        xhotel
                        (error 'keeps "protocol violation (wrong # of acquired hotels): ~e" xhotel)))))

(module+ test
  (check-equal? (keeps-parser (keeps-writer `(,AMERICAN))) `(,AMERICAN))
  (check-equal? (keeps-parser (keeps-writer `(,AMERICAN ,TOWER))) `(,AMERICAN ,TOWER))
  (check-equal?
   (keeps-parser (keeps-writer `(,AMERICAN ,FESTIVAL ,TOWER))) `(,AMERICAN ,FESTIVAL ,TOWER)))

;; ---------------------------------------------------------------------------------------------------

(define (booleans-writer lob)
  `(keep () ,@(map boolean-writer lob)))

(define booleans-parser 
  (xml-parser (keep () (b boolean-parser) ... #:action (map (lambda (x) (if (symbol? x) #f #t)) b))))

(define (boolean-writer b)
  (if b `(true ()) `(false ())))

(define boolean-parser
  (xml-parser 
   (true () #:action #t) ;; parsing false as #f would fail in booleans-parser
   (false () #:action 'false)))

(module+ test
  (check-equal? (booleans-parser (booleans-writer '(#t #t #f))) '(#t #t #f)))

;; ---------------------------------------------------------------------------------------------------

(define (players-writer lop)
  `(players () ,@(map player->xexpr lop)))

(define players-parser
  (xml-parser (players () (p player-parser) ... #:action p)))

(module+ test 
  (define p0 (state-players s0))
  (check-equal? (players-parser (players-writer p0)) p0)
  
  (define p1 (state-players s1))
  (check-equal? (players-parser (players-writer p1)) p1))


;; ---------------------------------------------------------------------------------------------------

(define (end-writer sc st)
  `(score () ,@(map result-writer sc) ,(state-writer st)))

(define end-parser 
  (xml-parser 
   (score () (result result-parser) ... (state state-parser) #:action (cons state result))))

(define (result-writer r)
  `(result ((name ,(first r)) (score ,(cash->string (second r))))))

(define result-parser
  (xml-parser (result ((name values) (score string->cash)) #:action (list name score))))

(module+ test
  (check-equal? (result-parser (result-writer (list "Hello" 100))) (list "Hello" 100))
  
  (define sc `(("c" 200) ("a" 100)))
  (check-equal? (end-parser (end-writer sc s0)) (cons s0 sc))
  
  (check-equal? (end-parser (end-writer sc s1)) (cons s1 sc)))



(define-syntax (class/remote stx)
  (syntax-case stx ()
    [(class/remote clause ...)
     (let ((xsend (datum->syntax stx 'xsend))
           (xreceive (datum->syntax stx 'xreceive))
           (okay (datum->syntax stx 'okay))
           (name (datum->syntax stx 'name))
           (in (datum->syntax stx 'in))
           (out (datum->syntax stx 'out))
           (handle (datum->syntax stx 'handle)))
       #`(class remote-actor% 
           (inherit #,xsend #,xreceive #,okay)
           (inherit-field #,name #,in #,out)
           (make-handle #,handle #,in #,out #,name)
           (super-new)
           clause ...))]))

;; auxiliary macro: defines an exception handler for remote methods 
(define-syntax-rule 
  (make-handle handle in out name)
  (define-syntax-rule 
    (handle ls ed (... ...))
    (with-handlers ((exn:fail:network? 
                     (lambda (x)
                       (log `(X ,x) `(remote ,@'ls failed: ,name))
                       (close-input-port in)
                       (close-output-port out)
                       (raise x)))
                    (exn:fail? 
                     (lambda (x) 
                       (log `(Y ,x) `(**remote ,@'ls failed: ,name))
                       (close-input-port in)
                       (close-output-port out)
                       (raise x))))
      ed (... ...))))

;; a remote actor base class 
(define remote-actor% 
  (class object% 
    (init-field in out)
    (super-new)
    
    (define/public (xsend x)
      (write-xml-to x out))
    
    (define/public (xreceive)
      (read-xml-from in))
    
    (define/public (okay tag r)
      (if r r (error tag "bad result message")))
    
    (field (name ""))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (define remote-service%
    (class/remote 
     
     (define/public (hello)
       (handle (hello)
               (xsend '(hello))
               (xreceive)))))
  
  (define sin (open-input-string "<bye />"))
  (define sout (open-output-string))
  (define x (send (new remote-service% [in sin][out sout]) hello))
  (check-equal? (get-output-string sout) "\n<hello>\n</hello>\n")
  (check-equal? x '(bye ())))
