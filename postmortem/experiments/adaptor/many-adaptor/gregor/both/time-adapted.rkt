#lang typed/racket/base

(require benchmark-util "core-adapter.rkt")

(require/typed/check "time.rkt"
  [#:struct Time ([hmsn : HMSN] [ns : Natural])]
( make-time (->* (Integer) (Integer Integer Integer) Time))
( time->hmsn (-> Time HMSN))
( time->ns (-> Time Natural))
( day-ns->time (-> Natural Time))
( time->iso8601 (-> Time String))
( time=? (-> Time Time Boolean))
( time<=? (-> Time Time Boolean))
( time<? (-> Time Time Boolean))
)
(provide
 (struct-out Time)
 make-time
 time->hmsn
 time->ns
 day-ns->time
 time->iso8601
 time=?
 time<?
 time<=?
)
