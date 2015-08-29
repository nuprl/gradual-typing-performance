#lang typed/racket/base

(provide
  (struct-out Date)
 date
 date->ymd
 date->jdn
 ymd->date
 jdn->date
 date->iso-week
 date->iso-wyear
 date->iso8601
 date=?
 date<=?
)

(require benchmark-util "core-adapter.rkt")
(require/typed/check "date.rkt"
  [#:struct Date ([ymd : YMD] [jdn : Integer])]
( date (->* (Natural) (Month Natural) Date))
( date->ymd (-> Date YMD))
( date->jdn (-> Date Integer))
( ymd->date (-> YMD Date))
( jdn->date (-> Exact-Rational Date))
( date->iso-week (-> Date Natural))
( date->iso-wyear (-> Date Natural))
( date->iso8601 (-> Date String))
( date=? (-> Date Date Boolean))
( date<=? (-> Date Date Boolean))
)
