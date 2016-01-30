acquire, exponential
====

- find a version of acquire with exponential slowdown
  - 0001 0011 0101, maybe more
  - see `variation0001` directory, edit NUM-TURNS in `main.rkt` to get exponential slowdown
- what Racket versions are fast/slow?
  - slow : <= 6.1.1
  - fast : >= 6.2
- slow contracts are for tree-generate (PATH = `pwd`/variation0001/tree.rkt)
  - 6.1.1 Running time is 93.96% contracts (11604/12350 ms)
    tree-next
    15907/2 ms
    (-> (instanceof/c (recursive-contract State%676707 #:impersonator))
        tile?
        (or/c #f hotel?)
        (listof (cons/c player?
                  (cons/c (listof (cons/c hotel? (cons/c (or/c #f #t) ()))) ())))
        shares-order?
        (-> any/c tile?)
        (values any/c
               (instanceof/c (recursive-contract State%692711 #:impersonator))))
    @ #(struct:srcloc PATH 13 12 446 9)
  - 6.2 Running time is 13.36% contracts (150/1123 ms)
    tree-next
    113/2 ms
    (-> (recursive-contract anonymous-contract #:impersonator)
        tile?
        (or/c #f hotel?)
        (listof (cons/c player?
                  (cons/c (listof (cons/c hotel? (cons/c (or/c #f #t) ()))) ())))
        shares-order?
        (-> any/c tile?)
        (values any/c
                (recursive-contract anonymous-contract #:impersonator)))
    @ #(struct:srcloc PATH 13 12 446 9)

