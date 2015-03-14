#lang racket

;; Copyright 2013 John Clements (clements@racket-lang.org)
;; Code licensed under the Mozilla Public License 2.0

;; this file contains functions to convert text into sounds

;;bg
;; Original file would make a SOUND from the sequence of dots and dashes.
;; We just make the . and -

(provide string->morse)

(require "morse-code-table.rkt")

;; map a character to a dit-dah string
(define (char->dit-dah-string letter)
  (match (hash-ref char-table (char-downcase letter) #f)
    [#f (raise-argument-error 'letter-map "character in map"
                              0 letter)]
    [str str]))

(define (string->morse str)
  (apply string-append
    (for/list ([c str])
      (char->dit-dah-string c))))
