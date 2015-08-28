#lang typed/racket/base

(require
  "../base/types.rkt"
  benchmark-util)
(require/typed/check "core-structs.rkt"
  [#:struct YMD ([y : Natural]
                 [m : Month]
                 [d : Natural])]
  [#:struct HMSN ([h : Integer]
                 [m : Integer]
                 [s : Integer]
                 [n : Integer])])

(provide
  (struct-out Date)
  (struct-out Time)
  (struct-out DateTime)
  (struct-out Moment))

;; Structs from the main gregor modules
;; `date.rkt`, `time.rkt`, `datetime.rkt`, `moment-base.rkt`

(struct Date ([ymd : YMD]
              [jdn : Integer]))

(struct Time ([hmsn : HMSN] [ns : Natural]))

(struct DateTime ([date : Date]
                  [time : Time]
                  [jd : Exact-Rational]))

(struct Moment ([datetime/local : DateTime]
                [utc-offset : Integer]
                [zone : (U String #f)]))

