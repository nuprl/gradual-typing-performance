dungeon
====

Dungeon-building benchmark
from stamourv, 2016-01-13
uses math/array

From Vincent:
    FWIW, the bulk of the boundary cost is due to `smooth-walls`, which accesses math/arrays a lot (that function spends 80% of its time checking contracts).


Preliminary contract-profile results `(generate-dungeon 10)`
    cpu time: 1923 real time: 1923 gc time: 24
    Running time is 75.57% contracts
    2035/2693 ms

    (-> (struct/c Array (vectorof Index) Index (box/c (or/c #f #t)) (587.5 ms
    (lib math/private/array/typed-array-struct.rkt):49:13            
    Array-shape                                                  587.5 ms

    (-> (struct/c Array (vectorof Index) Index (box/c (or/c #f #t)) (492 ms
    /home/ben/code/racket/fork/extra-pkgs/math/math-lib/math/private/
    Array-unsafe-proc                                            492 ms

    (-> (struct/c Array (vectorof Index) Index (box/c (or/c #f #t)) (289 ms
    /home/ben/code/racket/fork/extra-pkgs/math/math-lib/math/private/
    Array-strict!                                                289 ms

    (-> (struct/c Array (vectorof Index) Index (box/c (or/c #f #t)) (217 ms
    /home/ben/code/racket/fork/extra-pkgs/math/math-lib/math/private/
    Array-strict?                                                217 ms

    (-> (struct/c Array (vectorof Index) Index (box/c (or/c #f #t)) (140 ms
    (lib math/private/array/typed-mutable-array.rkt):48:9            
    array->mutable-array                                         140 ms

    (-> (struct/c Array (vectorof Index) Index (box/c (or/c #f #t)) (158 ms
    /home/ben/code/racket/fork/extra-pkgs/math/math-lib/math/private/
    array-ref                                                    158 ms

    (-> (struct/c Settable-Array (vectorof Index) Index (box/c (or/c 66.5 ms
    /home/ben/code/racket/fork/extra-pkgs/math/math-lib/math/private/
    Settable-Array-set-proc                                      66.5 ms

    (-> (struct/c Settable-Array (vectorof Index) Index (box/c (or/c 85 ms
    (lib math/private/array/array-indexing.rkt):14:2                 
    array-set!                                                   85 ms
