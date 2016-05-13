#lang racket/base

;; Copyright 2014 John Clements (clements@racket-lang.org)
;; Code licensed under the Mozilla Public License 2.0


;; -----------------------------------------------------------------------------

(require
  benchmark-util
  (only-in racket/file file->value))

(require (only-in "morse-code-strings.rkt"
  string->morse))

(require (only-in "levenshtein.rkt"
               string-levenshtein))

;(define-runtime-path common-words-list "./../base/Lemmatized-NGSL-ezi1.txt")
;(define word-frequency-list "./../base/frequency.rktd")
;(define word-frequency-list-small "./../base/frequency-small.rktd")

(define words+freqs '(("the" 22038615) ("be" 12545825) ("and" 10741073) ("of" 10343885) ("to" 10189111) ("a" 10144200) ("in" 7291468) ("that" 5186761) ("have" 4303955) ("I" 3978265) ("it" 3872477) ("for" 3296048) ("you" 3081151) ("he" 2909254) ("on" 2722286) ("with" 2683014) ("do" 2573587) ("as" 2407380) ("say" 1915138) ("this" 1890883) ("they" 1865580) ("his" 1821371) ("we" 1820935) ("but" 1797728) ("at" 1767638) ("not" 1650125) ("from" 1635914) ("n't" 1619007) ("by" 1523108) ("she" 1484869) ("or" 1379320) ("her" 1367541) ("one" 1183381) ("what" 1181023)))

(define words
    (for/list ([word+freq  words+freqs])
      (car word+freq)))

;(define allwords (file->words word-frequency-list))

;(define words-small (file->words word-frequency-list-small))

(define (main words)
 (for ([i (in-range 200)])
  (for* ([w1 (in-list words)]
         [w2 (in-list words)])
    (string->morse w1)
    (string->morse w2)
    (string-levenshtein w1 w2)
    (string-levenshtein w2 w1)
    (void))))

;(time (main allwords)) ;; 68,000ms
;(time (main words-small)) ;; 200ms
(time (main words))
