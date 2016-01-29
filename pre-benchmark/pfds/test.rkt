#lang racket/base

;; -----------------------------------------------------------------------------

(module u racket

  (require pfds/trie)

  (define (rand-list)
    (for/list ([i (in-range 128)])
        (random 256)))

  (define t (trie (list (rand-list))))
  (define (u:main)
    (bind (rand-list) 0 t))
  (provide u:main))

;; -----------------------------------------------------------------------------

(module t typed/racket

  (require pfds/trie)

  (define (rand-list)
    (for/list : (Listof Integer)
              ([i (in-range 128)])
        (random 256)))

  (define t (trie (list (rand-list))))
  (define (t:main)
    (bind (rand-list) 0 t))
  (provide t:main))

;; -----------------------------------------------------------------------------

;; Should be as slow as untyped
(module t2 typed/racket
  (require/typed/provide pfds/trie
    (#:opaque Trie trie?)
    (trie (-> (Listof (Listof Integer)) Trie))
    (bind (-> (Listof Integer) Integer Trie Trie)))

  (define (rand-list)
    (for/list : (Listof Integer)
              ([i (in-range 128)])
        (random 256)))

  (define t (trie (list (rand-list))))
  (define (t2:main)
    (bind (rand-list) 0 t))
  (provide t2:main))

;; ---

(module u2 racket

  (require (submod ".." t2))

  (define (rand-list)
    (for/list ([i (in-range 128)])
        (random 256)))

  (define t (trie (list (rand-list))))
  (define (u2:main)
    (bind (rand-list) 0 t))
  (provide u2:main))

;; -----------------------------------------------------------------------------

(require
  't
  'u
  't2
  'u2
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
