#lang typed/racket/base

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
  benchmark-util
  (only-in racket/file file->value)
  racket/runtime-path)

(require/typed/check "morse-code-strings.rkt"
  [string->morse (-> String String)])

(require/typed/check "levenshtein.rkt"
               [string-levenshtein (String String -> Integer)])


(define-runtime-path common-words-list "./../base/Lemmatized-NGSL-ezi1.txt")
(define-runtime-path word-frequency-list "./../base/frequency.rktd")

(define-predicate freq-list? (Listof (List String Integer)))

(: allwords (Listof (List String Integer)))
(define allwords
  ((lambda (v) (cond [(freq-list? v) v]
                     [else (error 'allwords "expected a frequency list")]))
   (file->value word-frequency-list)))

;; read all words matching the given regexp
(: regexp->wordlist (Regexp Index -> (Listof String)))
(define (regexp->wordlist rx maxlen)
  (filter (lambda: ([w : String]) (and (not (eq? #f (regexp-match rx w)))
                                       (<= (string-length w) maxlen)))
          (map (ann car ((List String Integer) -> String)) allwords)))

;; Word bank, to choose from
(define wordlist (regexp->wordlist #px"^[qwertyuiopasdfghjklzxcvbnm]*$" MAX-WORD-LEN))

;; Randomly pick some words from the wordlist
(define (rand-words)
  (for/list: : (Listof String) ([i NUM-WORDS-CHOSEN]) (list-ref wordlist (random (length wordlist)))))

;; Convert a list of words to a string, for levenshtein
(: text (-> String))
(define (text) (apply string-append (rand-words)))

(define (main)
  (for ([_ (in-range NUM-TESTS)])
    (run-test)))

(time (main))
