#lang racket/base

;; Copyright 2014 John Clements (clements@racket-lang.org)
;; Code licensed under the Mozilla Public License 2.0

;;bg
;; Randomly generate some english words,
;; convert them to morse code,
;; compare the levenshtein distance between the words and a simulated user guess
(define (run-test)
  ;; Pick random word
  (define w1 (text))
  ;; Convert word to morse code (in 'real life', we'd play the sound)
  (string->morse w1)
  ;; Simulate user input (pick another random word)
  (define w2 (text))
  ;; Compare the expected and actual words
  (string-levenshtein w1 w2)
  (void))

;; --- test parameters

(define MAX-WORD-LEN 10)
(define NUM-WORDS-CHOSEN 100)
(define NUM-TESTS 10)

;; -----------------------------------------------------------------------------

(require
  "levenshtein.rkt"
  "morse-code-sounds.rkt"
  racket/runtime-path
  (only-in racket/file file->value))

(define-runtime-path common-words-list "./../base/Lemmatized-NGSL-ezi1.txt")
(define-runtime-path word-frequency-list "./../base/frequency.rktd")

(define allwords
   (file->value word-frequency-list))

;; read all words matching the given regexp
(define (regexp->wordlist rx maxlen)
  (filter (lambda (w) (and (not (eq? #f (regexp-match rx w)))
                                       (<= (string-length w) maxlen)))
          (map car allwords)))

;; Word bank, to choose from
(define wordlist (regexp->wordlist #px"^[qwertyuiopasdfghjklzxcvbnm]*$" MAX-WORD-LEN))

;; Randomly pick some words from the wordlist
(define (rand-words)
  (for/list ([i NUM-WORDS-CHOSEN]) (list-ref wordlist (random (length wordlist)))))

;; Convert a list of words to a string, for levenshtein
(define (text) (apply string-append (rand-words)))

(define (main)
  (for ([_ (in-range NUM-TESTS)])
    (run-test)))

(time (main))
