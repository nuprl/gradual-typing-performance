#lang typed/racket/base

(require
  benchmark-util
  "core-adapter.rkt"
  "date-adapted.rkt"
  "time-adapted.rkt"
)
(require/typed/check "datetime.rkt"
  [#:struct DateTime ([date : Date] [time : Time] [jd : Exact-Rational])]
( datetime (->* (Natural) (Month Natural Natural Natural Natural Natural) DateTime))
( datetime->date (-> DateTime Date))
( datetime->time (-> DateTime Time))
( datetime->jd (-> DateTime Exact-Rational))
( datetime->posix (-> DateTime Exact-Rational))
( date+time->datetime (-> Date Time DateTime))
( jd->datetime (-> Exact-Rational DateTime))
( posix->datetime (-> Exact-Rational DateTime))
( datetime->iso8601 (-> DateTime String))
( datetime-add-nanoseconds (-> DateTime Integer DateTime))
( datetime-add-seconds (-> DateTime Integer DateTime))
( datetime=? (-> DateTime DateTime Boolean))
( datetime<? (-> DateTime DateTime Boolean))
( datetime<=? (-> DateTime DateTime Boolean))
)


(provide
 datetime
 datetime->date
 datetime->time
 datetime->jd
 datetime->posix
 date+time->datetime
 jd->datetime
 posix->datetime
 datetime->iso8601
 datetime-add-nanoseconds
 datetime-add-seconds
 datetime=?
 datetime<?
 datetime<=?
 (struct-out DateTime)
)
