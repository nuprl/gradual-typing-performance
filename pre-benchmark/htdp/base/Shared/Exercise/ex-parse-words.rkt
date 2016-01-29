;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex-parse-words) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Line -> [List-of String]
;; turn a line into a list of words

(check-expect 
 (line->words (list "h" "o" "w" " " "a" "r" "e" " " "y" "o" "u" " " "d" "o" "i" "n" "g" "?"))
 (list "how" "are" "you" "doing" "?"))

(check-expect 
 (line->words (list "h" "o" "w" " "))
 (list "how"))

(check-expect 
 (line->words (list "h" "o" "w"))
 (list "how"))

(define (line->words l)
  (cond
    [(empty? l) '()]
    [else 
     (cond
       [(string-whitespace? (first l)) (line->words (rest l))]
       [(member? (first l) LETTERS) (cons (first-word l) (line->words (remove-first-word l)))]
       [else (cons (first l) (line->words (rest l)))])]))

;; @tech{File} -> String
;; retrieve the prefix of @racket[aline] up to the first occurrence of @racket[NEWLINE]
(define (first-word aline)
  (cond
    [(empty? aline) ""]
    [else 
     (if (member? (first aline) LETTERS)
         (string-append (first aline) (first-word (rest aline)))
         "")]))

;; @tech{File} -> @tech{Line}
;; drop the suffix of @racket[aline] behind the first occurrence of @racket[NEWLINE]
(define (remove-first-word aline)
  (cond
    [(empty? aline) '()]
    [else 
     (if (member? (first aline) LETTERS)
         (remove-first-word (rest aline))
         aline)]))

(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))
