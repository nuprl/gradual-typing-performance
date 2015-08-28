#lang typed/racket/base

(require
  benchmark-util
  "datetime-adapted.rkt"
)

(require/typed/check "moment-base.rkt"
  [#:struct Moment ([datetime/local : DateTime] [utc-offset : Integer] [zone : (U String #f)])]
( moment->iso8601/tzid (-> Moment String))
( moment->iso8601 (-> Moment String))
( make-moment (-> DateTime Integer (U String #f) Moment))
)

(provide
 moment->iso8601
 moment->iso8601/tzid
 make-moment
 (struct-out Moment)
)
