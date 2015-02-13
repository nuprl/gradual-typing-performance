;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-palindrome) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))


;; [cons 1String [List-of 1String]] -> [cons 1String [List-of 1String]]
;; create a palindrome by mirroring the given list around the last item

(check-expect (mirror (explode "abc")) (explode "abcba"))

(define (mirror s0)
  (append (all-but-last s0) (list (last s0)) (reverse (all-but-last s0))))

(define (all-but-last s)
  (cond
    [(empty? (rest s)) '()]
    [else (cons (first s) (all-but-last (rest s)))]))

(define (last s)
  (cond
    [(empty? (rest s)) (first s)]
    [else (last (rest s))]))


(check-expect (mirror.v2 (explode "abc")) (explode "abcba"))

(define (mirror.v2 s0)
  (local (;; [List-of 1String] 1String -> [List-of 1String]
          ;; mirror 
          ;; accumulator all 1String-s between s and s0 in reverse order
          (define (mirror/a s a)
            (cond
              [(empty? (rest s)) (cons (first s) a)]
              [else (cons (first s) (mirror/a (rest s) (cons (first s) a)))])))
    (mirror/a s0 '())))