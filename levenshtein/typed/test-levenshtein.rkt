;; See levenshtein.ss for legal information.
;; $Id: test-levenshtein.ss,v 1.1 2009/02/24 19:55:45 neilpair Exp $

#lang typed/racket

(module+ test
  (require typed/rackunit
         "levenshtein.rkt")

 (check-equal? (levenshtein "adresse" "address") 2)
 (check-equal? (levenshtein "adresse" "addressee") 2)
 (check-equal? (levenshtein "gambol"  "gumbo") 2)
 (check-equal? (levenshtein "gumbo"   "gambol") 2)
 (check-equal? (levenshtein "gumbo"  "bumble") 3)

 (check-equal?
             (levenshtein '#(#\g #\u #\m #\b #\o)
                          '#(#\b #\u #\m #\b #\l #\e))
             3)

 (check-equal? (levenshtein "a"      "abcde") 4)
 (check-equal? (levenshtein "abcde"      "a") 4)
)
