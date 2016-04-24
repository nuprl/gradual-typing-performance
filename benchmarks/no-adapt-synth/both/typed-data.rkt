#lang typed/racket/base

(require benchmark-util)

(define-type Array (Pairof 'array (List (Vectorof Integer) Integer (Boxof Boolean) (-> Void) (-> (Vectorof Integer) Float))))

(define-type Settable-Array (Pairof 'settable-Array (List (Vectorof Integer) Integer (Boxof Boolean) (-> Void) (-> (Vectorof Integer) Float) ((Vectorof Integer) Float -> Void))))

(define-type Mutable-Array (Pairof 'mutable-Array (List (Vectorof Integer) Integer (Boxof Boolean) (-> Void) (-> (Vectorof Integer) Float) (Vectorof Float))))

(require/typed/check "data.rkt"
  (array (-> (Vectorof Integer) Integer (Boxof Boolean) (-> Void) (-> (Vectorof Integer) Float) Array))
  (array? (-> Any Boolean))
  (array-shape (-> Array (Vectorof Integer)))
  (array-size (-> Array Integer))
  (array-strict? (-> Array (Boxof Boolean)))
  (array-strict! (-> Array (-> Void)))
  (array-unsafe-proc (-> Array (-> (Vectorof Integer) Float)))

  (settable-array (-> (Vectorof Integer) Integer (Boxof Boolean) (-> Void) (-> (Vectorof Integer) Float) (-> (Vectorof Integer) Float Void) Settable-Array))
  (settable-array? (-> Any Boolean))
  (settable-array-shape (-> Array (Vectorof Integer)))
  (settable-array-size (-> Array Integer))
  (settable-array-strict? (-> Array (Boxof Boolean)))
  (settable-array-strict! (-> Array (-> Void)))
  (settable-array-unsafe-proc (-> Array (-> (Vectorof Integer) Float)))
  (settable-array-set-proc (-> Array (-> (Vectorof Integer) Float Void)))

  (mutable-array (-> (Vectorof Integer) Integer (Boxof Boolean) (-> Void) (-> (Vectorof Integer) Float) (-> (Vectorof Integer) Float Void) (Vectorof Float) Mutable-Array))
  (mutable-array? (-> Any Boolean))
  (mutable-array-shape (-> Array (Vectorof Integer)))
  (mutable-array-size (-> Array Integer))
  (mutable-array-strict? (-> Array (Boxof Boolean)))
  (mutable-array-strict! (-> Array (-> Void)))
  (mutable-array-unsafe-proc (-> Array (-> (Vectorof Integer) Float)))
  (mutable-array-set-proc (-> Array (-> (Vectorof Integer) Float Void)))
  (mutable-array-data (-> Array (Vectorof Float)))
)

(define-type Indexes (Vectorof Integer))
(define-type In-Indexes Indexes)

;; From mix: A Weighted-Signal is a (List (Array Float) Real)
(define-type Weighted-Signal (List Array Real))

;; drum patterns are simply lists with either O (bass drum), X (snare) or
;; #f (pause)
(define-type Drum-Symbol (U 'O 'X #f))
(define-type Pattern (Listof Drum-Symbol))


(provide
  Indexes
  In-Indexes
  Weighted-Signal
  Drum-Symbol
  Pattern

  Array
  array
  array?
  array-shape
  array-size
  array-i:strict?
  array-i:strict!
  array-unsafe-proc

  Settable-Array
  settable-array
  settable-array?
  settable-array-shape
  settable-array-size
  settable-array-i:strict?
  settable-array-i:strict!
  settable-array-unsafe-proc
  settable-array-set-proc

  Mutable-Array
  mutable-array
  mutable-array?
  mutable-array-shape
  mutable-array-size
  mutable-array-i:strict?
  mutable-array-i:strict!
  mutable-array-unsafe-proc
  mutable-array-set-proc
  mutable-array-data
)
