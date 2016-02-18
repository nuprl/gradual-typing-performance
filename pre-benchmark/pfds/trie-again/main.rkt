#lang racket/base

(module trie typed/racket
  (provide lookup bind trie insert Trie tries)
  
  (require racket/match)
  (define-type-alias (Key A) (Listof A))
  
  (define-struct: Mt ())
  (define-struct: (A) Some ([elem : A]))
  
  (define-type-alias (Option A) (U Mt (Some A)))
  
  (define-struct: (K V) Trie ([opt : (Option V)]
                              [map : (Listof (Pairof K (Trie K V)))]))
  
  ;; -----------------------------------------------------------------------------
  ;;bg; hash operations
  
  (: b:hash-ref (All (K V) (-> (Listof (Pairof K (Trie K V))) K (Trie K V))))
  (define (b:hash-ref h k)
    (or
      (for/or : (U #f (Trie K V))
              ([k+v (in-list h)])
        (and (equal? (car k+v) k)
             (cdr k+v)))
      (error 'b:hash-ref "Sorry")))
  
  (: b:hash-set (All (K V) (-> (Listof (Pairof K (Trie K V))) K (Trie K V) (Listof (Pairof K (Trie K V))))))
  (define (b:hash-set h k v)
    (: seen? (Boxof Boolean))
    (define seen? (box #f))
    (define h*
      (for/list : (Listof (Pairof K (Trie K V)))
                ([k+v (in-list h)])
        (if (and (not (unbox seen?))
                 (equal? (car k+v) k))
          (begin (set-box! seen? #t) (cons k v))
          k+v)))
    (if (unbox seen?)
      h*
      (cons (cons k v) h*)))
  
  ;; -----------------------------------------------------------------------------
  
  (: empty : (All (K V) (-> (Trie K V))))
  (define (empty) 
    (make-Trie (make-Mt) 
               (ann '() (Listof (Pairof K (Trie K V))))))
  
  (: lookup : (All (K V) ((Key K) (Trie K V) -> V)))
  (define (lookup keys map)
    (if (null? keys)
        (let ([opt (Trie-opt map)])
          (if (Mt? opt)
              (error 'lookup "given key not found in the trie")
              (Some-elem opt)))
        (let ([fst (car keys)]
              [hash (Trie-map map)])
          (with-handlers
              ([exn:fail? (lambda (error?) 
                            (error 'lookup "given key not found in the trie"))])
            (lookup (cdr keys) (b:hash-ref hash fst))))))
  
  (: bind : (All (K V) ((Key K) V (Trie K V) -> (Trie K V))))
  (define (bind lok v map)
    (let ([hash (Trie-map map)]
          [fst (car lok)]
          [rst (cdr lok)]
          [opt (Trie-opt map)])
      (make-Trie opt (b:hash-set hash fst 
                               (ann (with-handlers 
                                        ([exn:fail? 
                                          (lambda (error?) (build v rst))])
                                      (bind rst v (b:hash-ref hash fst)))
                                    (Trie K V))))))
  
  (: build : (All (K V) (V (Listof K) -> (Trie K V))))
  (define (build val lstk)
    (if (null? lstk)
        (make-Trie (make-Some val) 
                   (ann '()
                        (Listof (Pairof K (Trie K V)))))
        (make-Trie (make-Mt) 
                   (list (cons (car lstk) (build val (cdr lstk)))))))
  
  (: trie : (All (K) ((Listof (Listof K)) -> (Trie K Integer))))
  (define (trie lst)
    (insert (get-vals lst) lst (ann (empty) (Trie K Integer))))
  
  (: get-vals : (All (K) ((Listof (Listof K)) -> (Listof Integer))))
  (define (get-vals lst)
    (: local : (All (K) (Integer (Listof (Listof K)) -> (Listof Integer))))
    (define (local ctr lstk)
      (if (null? (cdr lstk))
          (cons ctr null)
          (cons ctr (local (add1 ctr) (cdr lstk)))))
    (local 1 lst))
  
  ;; While creating the tree, 
  ;; if   (hash-ref hash k) throws an error, 
  ;; then it means that that there is no entry for k. So build a new
  ;;      Trie for rest of the key and create an entry for k. 
  ;; else go deeper into the insert searching for the rest of the key.
  
  (: insert : 
     (All (K V) ((Listof V) (Listof (Listof K)) (Trie K V) -> (Trie K V))))
  (define (insert lstv lstk tri)
    (match (list lstv lstk)
      [(list null null) tri]
      [(list (cons v vs) (cons (cons k ks) rstk))
       (let* ([hash (Trie-map tri)]
              [tree (ann (with-handlers ([exn:fail? (lambda (error?) 
                                                      (build v ks))])
                           (go-deep (b:hash-ref hash k) ks v)) 
                         (Trie K V))])
         (insert vs rstk
                 (Trie (Trie-opt tri) (b:hash-set hash k tree))))]))
  
  (: tries : (All (K V) ((Listof V) (Listof (Listof K)) -> (Trie K V))))
  (define (tries lstv lstk)
    (insert lstv lstk (ann (empty) (Trie K V))))
  
  ;; Uses the same trick as previous one does
  (: go-deep : (All (K V) ((Trie K V) (Listof K) V -> (Trie K V))))
  (define (go-deep tri lstk val)
    (if (null? lstk)
        (make-Trie (make-Some val) (Trie-map tri))
        (let* ([hash (Trie-map tri)]
               [k (car lstk)]
               [ks (cdr lstk)]
               [insert (ann (with-handlers
                                ([exn:fail? (lambda (error?) (build val ks))])
                              (go-deep (b:hash-ref hash k) ks val))
                            (Trie K V))])
          (make-Trie (Trie-opt tri) (b:hash-set hash k insert)))))
)

;; -----------------------------------------------------------------------------

(module test typed/racket
  (require (submod ".." trie))
  (require typed/test-engine/scheme-tests)

  (check-expect
   (lookup (string->list "Hari")
           (bind (string->list "JP") 5 
                 (trie 
                  (map string->list 
                       (list "Hari Prashanth" "Hari" "Hari " "K R H P")))))
   2)

  (check-expect
   (lookup (string->list "Hari") 
           (tries
            (list 1 2 3 4 5) 
            (map string->list (list "Hari" "Prashanth" "K R" "KRHP" "K R H P"))))
   1)

  (check-expect
   (lookup (string->list "Prashanth") 
           (tries
            (list 1 2 3 4 5) 
            (map string->list (list "Hari" "Prashanth" "K R" "KRHP" "K R H P"))))
   2)
  
  (check-expect
   (lookup (string->list "KRHP") 
           (tries
            (list 1 2 3 4 5) 
            (map string->list (list "Hari" "Prashanth" "K R" "KRHP" "K R H P"))))
   4)
  
  (check-expect
   (lookup (string->list "K R H P") 
           (tries
            (list 1 2 3 4 5) 
            (map string->list (list "Hari" "Prashanth" "K R" "KRHP" "K R H P"))))
   5)
  
  (check-expect
   (lookup (string->list "K R") 
           (tries
            (list 1 2 3 4 5) 
            (map string->list (list "Hari" "Prashanth" "K R" "KRHP" "K R H P"))))
   3)
  
  (check-expect
   (lookup (string->list "Hari Prashanth") 
           (tries
            (list 1 2 3 4) 
            (map string->list (list "Hari Prashanth" "K R" "KRHP" "K R H P"))))
   1)
  
  (check-expect
   (lookup (string->list "Hari ") 
           (tries
            (list 1 2 3 4) 
            (map string->list (list "Hari Prashanth" "Hari" "Hari " "K R H P"))))
   3)
  
  
  (check-expect
   (lookup (string->list "HariKRH") 
           (bind (string->list "HariKRH") 5 
                 (tries
                  (list 1 2 3 4) 
                  (map string->list 
                       (list "Hari Prashanth" "Hari" "Hari " "K R H P")))))
   5)
   
  (check-expect
   (lookup (string->list "JP") 
           (bind (string->list "JP") 5 
                 (tries
                  (list 1 2 3 4) 
                  (map string->list 
                       (list "Hari Prashanth" "Hari" "Hari " "K R H P")))))
   5)
  
  (check-error
   (lookup (string->list "Hari123") 
           (bind (string->list "JP") 5 
                 (tries
                  (list 1 2 3 4) 
                  (map string->list 
                       (list "Hari Prashanth" "Hari" "Hari " "K R H P")))))
   "lookup: given key not found in the trie")
  
  (check-error
   (lookup (string->list "Har") 
           (bind (string->list "JP") 5 
                 (tries
                  (list 1 2 3 4) 
                  (map string->list 
                       (list "Hari Prashanth" "Hari" "Hari " "K R H P")))))
   "lookup: given key not found in the trie")
  
  (test)
)

;; -----------------------------------------------------------------------------

(module u:user racket

  (require (submod ".." trie))

  (define (rand-list)
    (for/list ([i (in-range 128)])
        (random 256)))

  (define t (trie (list (rand-list))))
  (define (u:main)
    (bind (rand-list) 0 t)
    (void))
  (provide u:main))

;; -----------------------------------------------------------------------------

(module t:user typed/racket

  (require (submod ".." trie))

  (define (rand-list)
    (for/list : (Listof Integer)
              ([i (in-range 128)])
        (random 256)))

  (define t (trie (list (rand-list))))
  (define (t:main)
    (bind (rand-list) 0 t)
    (void))
  (provide t:main))

;; -----------------------------------------------------------------------------

(require
  't:user
  'u:user
  contract-profile)

(define (cp t [cbf 'stdout])
  (contract-profile-thunk
    #:cost-breakdown-file cbf
    #:module-graph-file #f
    #:boundary-view-file #f
    #:boundary-view-key-file #f
    t))

(time (cp u:main))
;(time (cp t:main))
;(time (cp t2:main))
;(time (cp u2:main))