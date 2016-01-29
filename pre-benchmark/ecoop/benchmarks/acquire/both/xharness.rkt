#lang racket

;; ---------------------------------------------------------------------------------------------------
;; the basic pieces for creating a test harness 

(require (only-in xml xexpr?))

(provide 
 (contract-out 
  (make-main 
   (-> (-> xexpr? xexpr?) (-> any)))
  (make-process-request 
   (-> (-> xexpr? xexpr?) (-> xexpr? xexpr?))))
 
 state-p 
 player-p
 hotel-p
 board-p
 tile-p
 share-p)

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION

(require "basics.rkt" "board.rkt" "state.rkt" "Lib/xml.rkt" "Lib/io.rkt")

;; -> Void 
(define (make-main interpret)
  (define process (make-process-request interpret))
  (define (main)
    (let loop ()
      (define request (read-xml-from))
      (unless (eof-object? request)
        (define response (process request))
        (cond
          [(boolean? response) (error 'main "invalid XML: ~e" request)]
          [else (write-xml-to response) (loop)]))))
  main)

;; Request -> Response 
(define ((make-process-request interpret) request)
  (with-handlers ((exn:fail:contract:blame? (lambda (x) `(error ((msg ,(exn-message x)))))))
    (interpret request)))

;; COMMON DATA

;; Xexpr -> State 
(define state-p 
  (xml-parser (state () (b board-p) (p player-p) ... #:action (*create-state b p))))

;; Xexpr -> Player
(define player-p
  ;; any name is a good name for a player 
  (xml-parser (player ((name values) (cash string->cash)) (s share-p) ... (t tile-p) ...
                      #:action (*create-player name cash (*combine-shares s) t))))

;; Xexpr -> Hotel 
(define hotel-p
  (xml-parser (hotel ((label string->hotel)) #:action label)))

;; Xexpr -> [Maybe Board]
(define board-p
  (xml-parser
   (board () (t tile-p) ... (h board-hotel-p) ... #:action (*create-board-with-hotels t h))))

;; Xexpr -> [Maybe (cons Hotel (cons Tile (cons Tile [Listof Tile])))]
(define board-hotel-p 
  (xml-parser
   (hotel ((name string->hotel)) (t1 tile-p) (t2 tile-p) (t tile-p) ... 
          #:action (list* name t1 t2 t))))

;; Xexpr -> Tile 
(define tile-p 
  (xml-parser (tile ((column string->column) (row string->row)) #:action (tile column row))))

;; Xexpr -> Hotel
(define share-p
  (xml-parser (share ((name string->hotel) (count string->count)) 
                     #:action (*create-shares name count))))
