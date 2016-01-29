#lang typed/racket/base

(require racket/list
         racket/bool
         racket/match
         racket/math
         typed/racket/class
         typed/racket/gui/base)

(require/typed racket/base [prefab-struct-key (-> Any (U #f Symbol (Listof Any)))])
(require/typed htdp/error
               [check-arg ((U String Symbol) Boolean String
                           (U String Symbol Natural) Any -> Void)])

(provide (all-defined-out))

(define INSET  5)     ;; the space around the image in the canvas
(define RATE   1/30)   ;; the clock tick rate 
(define TRIES  3)     ;; how many times should register try to connect to the server 
(define PAUSE  1/2)     ;; # secs to wait between attempts to connect to server 
(define SQPORT 4567) ;; the port on which universe traffic flows

(define-type Mouse-Event (U "button-down" "button-up" "drag" "move" "enter" "leave"))
(define-type Key-Event String)

;                                                                               
;                                                                               
;                                                                               
;    ;;;                                              ;;;                       
;   ;   ;                                            ;   ;                      
;   ;   ;                                            ;   ;                      
;   ;       ;;;   ;;;;;  ;;;;;   ;;;   ;;;;          ;   ;  ;   ;  ;   ;        
;   ;      ;   ;  ; ; ;  ; ; ;  ;   ;  ;   ;         ;;;;;  ;   ;   ; ;         
;   ;      ;   ;  ; ; ;  ; ; ;  ;   ;  ;   ;         ;   ;  ;   ;    ;          
;   ;      ;   ;  ; ; ;  ; ; ;  ;   ;  ;   ;         ;   ;  ;   ;    ;          
;   ;   ;  ;   ;  ; ; ;  ; ; ;  ;   ;  ;   ;         ;   ;  ;  ;;   ; ;    ;;   
;    ;;;    ;;;   ; ; ;  ; ; ;   ;;;   ;   ;         ;   ;   ;; ;  ;   ;   ;;   
;                                                                               
;                                                                               
;                                                                               

;; ---------------------------------------------------------------------------------------------------

;; Any -> Boolean
(define nat? exact-nonnegative-integer?)

(: number->integer (->* [Real] [(U Symbol String) (U Symbol String)] Integer))
(define (number->integer x [t ""] [p ""])
  (check-arg t (and (number? x) (real? x)) "real number" p x)
  (exact-floor x))

;; ---------------------------------------------------------------------------------------------------
;; converts i to a string, adding leading zeros, make it at least as long as L
(: zero-fill (Natural Natural -> String))
(define (zero-fill i L)
  (let ([n (number->string i)])
    (string-append (make-string (max (- L (string-length n)) 0) #\0) n)))

;; ---------------------------------------------------------------------------------------------------

;; turn a mouse event into its pieces
(: mouse-event->parts ((Instance Mouse-Event%) -> (Values Integer Integer Mouse-Event)))
(define (mouse-event->parts e)
  (define x (- (send e get-x) INSET))
  (define y (- (send e get-y) INSET))
  (values x y 
          (cond [(send e button-down?) "button-down"]
                [(send e button-up?)   "button-up"]
                [(send e dragging?)    "drag"]
                [(send e moving?)      "move"]
                [(send e entering?)    "enter"]
                [(send e leaving?)     "leave"]
                [else ; (send e get-event-type)
                 (let ([m (send e get-event-type)])
                   (error 'on-mouse (format "Unknown event: ~a" m)))])))

(: key-event->parts ((Instance Key-Event%) -> Key-Event))
(define (key-event->parts e)
  (define x (send e get-key-code))
  (cond
    [(char? x) (string x)]
    [(symbol? x) (symbol->string x)]
    [else (error 'on-key (format "Unknown event: ~a" x))]))

(: key-release->parts ((Instance Key-Event%) -> String))
(define (key-release->parts e)
  (define x (send e get-key-release-code))
  (cond
    [(char? x) (string x)]
    [(symbol? x) (symbol->string x)]
    [else (error 'on-key (format "Unknown event: ~a" x))]))

;; ---------------------------------------------------------------------------------------------------
(: name-of (Any Symbol -> Symbol))
(define (name-of draw tag)
  (define fname (assert (object-name draw) symbol?))
  (if fname fname tag))

;; ---------------------------------------------------------------------------------------------------
(: sexp? (Any -> Boolean))
(define (sexp? x)
  (cond
    [(empty? x) true]
    [(string? x) true]
    [(symbol? x) true]
    [(number? x) true]
    [(boolean? x) true]
    [(char? x) true]
    [(pair? x) (and (list? x) (andmap sexp? x))]
    [(and (struct? x) (prefab-struct-key x)) (for/and ((i (struct->vector x))) (sexp? i))]
    [else false]))

; tests:
;(struct s (t) #:prefab)
;(unless (sexp? (list (s (list 'a))))
;  (error 'prefab "structs should be sexp?"))

(: no-newline? (String -> Boolean))
(define (no-newline? x)
  (not (member #\newline (string->list x))))

;; ---------------------------------------------------------------------------------------------------
;; exchange one-line messages between worlds and the server

(define tcp-eof (gensym 'tcp-eof))

(define (tcp-eof? [a : Any]) (and (symbol? a) (eq? tcp-eof a)))

(: tcp-send (Output-Port Any -> Void))
(define (tcp-send out msg)
  (write msg out)
  (newline out)
  (flush-output out))

(: tcp-receive (Input-Port -> Any))
(define (tcp-receive in)
  (with-handlers ((exn? (lambda (x) (raise tcp-eof))))
    (define x (read in))
    (if (eof-object? x) 
        (raise tcp-eof)
        (begin
          (read-line in) ;; read the newline 
          x))))

;; process a registration from a potential client, invoke k on name if it is okay
(: tcp-process-registration (All (Y) (Input-Port Output-Port (Any -> Y) -> (U Y Void))))
(define (tcp-process-registration in out k)
  (define next (tcp-receive in))
  (match next
    [`(REGISTER ((name ,name)))
     (tcp-send out '(OKAY))
     (k name)]))
  
;; register with the server, send the given name or make up a symbol
(: tcp-register (Input-Port Output-Port (Option String) -> Void))
(define (tcp-register in out name)
  (define msg `(REGISTER ((name ,(if name name (gensym 'world))))))
  (tcp-send out msg)
  (define ackn (tcp-receive in))
  (unless (equal? ackn '(OKAY))
    (raise tcp-eof)))

;                                                   
;                                                   
;                                                   
;    ;;;                         ;;;   ;      ;     
;   ;   ;                       ;   ;  ;      ;     
;   ;   ;                       ;   ;  ;      ;     
;   ;   ;  ; ;;    ;;;;         ;      ;;;;   ;  ;  
;   ;;;;;  ;;  ;  ;   ;         ;      ;   ;  ; ;   
;   ;   ;  ;   ;  ;   ;         ;      ;   ;  ;;    
;   ;   ;  ;      ;   ;         ;      ;   ;  ; ;   
;   ;   ;  ;      ;   ;         ;   ;  ;   ;  ;  ;  
;   ;   ;  ;       ;;;;          ;;;   ;   ;  ;   ; 
;                     ;                             
;                 ;   ;                             
;                  ;;;                              

(: check-pos (Symbol Any String -> Void))
(define (check-pos t c r)
  (check-arg 
   t (and (real? c) (>= (number->integer c t r) 0)) "positive integer" r c))
