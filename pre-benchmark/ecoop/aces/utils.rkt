
(module utils typed/racket
  (provide shuffle-list)
  (: shuffle-list (All (X) (-> (Listof X) Natural (Listof X))))
  (define shuffle-list 
   (lambda (l c)
     (if (zero? c)
	 l
	 (let-values ([(a b)
		       (let ([half (floor (/ (length l) 2))])
			 (values
			  (let: loop : (Listof X) ([l : (Listof X) l][n : Natural half])
			    (if (zero? n)
				null
				(cons (car l) (loop (cdr l) (sub1 n)))))
			  (list-tail l half)))])
	   (shuffle-list
	    (let loop : (Listof X) ([a : (Listof X) a][b : (Listof X) b][l : (Listof X) null])
	      (cond
	       [(null? a) (append (reverse b) l)]
	       [(null? b) (append (reverse a) l)]
	       [(zero? (random 2))
		(loop (cdr a) b (cons (car a) l))]
	       [else
		(loop a (cdr b) (cons (car b) l))]))
	    (sub1 c)))))))
