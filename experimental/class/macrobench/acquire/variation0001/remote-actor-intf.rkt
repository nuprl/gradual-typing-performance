#lang racket

;; ---------------------------------------------------------------------------------------------------
;; parsers and writers for remote XML messages

(require "basics.rkt" "board.rkt" "protocols.rkt" "state-intf.rkt" "state.rkt" "Lib/contract.rkt" xml)

(define (parser/c p) (-> any/c (maybe/c p)))
(define-syntax-rule (writer/c d ...) (-> d ... xexpr?))

(interface remote-connectivity&
  ;; (class/remote class-clause ...)
  ;; create class extension of remote-actor% that 
  ;; -- inherits three methods: xsend, xreceive, okay
  ;; -- inherits three fields: name, in, out 
  ;; -- sets up an exn macro for dealing with exns in handlers
  class/remote
  
  (signup-parser (parser/c string?))
  (signup-writer (writer/c string?))
  
  (void-parser (parser/c any/c))
  (void-writer (writer/c))
  
  (tile-parser (parser/c tile?))
  (tile-writer (writer/c tile?))
  
  (turn-parser (parser/c state?))
  (turn-writer (writer/c (instanceof/c turn-player/c)))
  
  (state-parser (parser/c state?))
  (state-writer (writer/c state?))
  
  (turn-plain-parser (parser/c (list/c tile? (maybe/c hotel?) shares-order/c)))
  (turn-plain-writer (writer/c tile? (maybe/c hotel?) shares-order/c))
  
  (turn-merge-parser (parser/c (list/c tile? (maybe/c hotel?))))
  (turn-merge-writer (writer/c tile? (maybe/c hotel?)))
  
  (placement-parser (parser/c (list/c tile? (maybe/c hotel?))))
  (placement-writer (writer/c tile? (maybe/c hotel?)))
  
  (order-parser (parser/c shares-order/c))
  (order-writer (writer/c shares-order/c))
  
  (keeps-parser (parser/c (listof hotel?)))
  (keeps-writer (writer/c (listof hotel?)))
  
  (booleans-parser (parser/c (listof boolean?)))
  (booleans-writer (writer/c (listof boolean?)))
  
  (players-parser (parser/c (listof player?)))
  (players-writer (writer/c (listof player?)))
  
  (end-parser (parser/c (cons/c state? score/c)))
  (end-writer (writer/c score/c state?)))
