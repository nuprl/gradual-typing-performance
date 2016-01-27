#lang racket/base

;; Canonical data files for each benchmark

(define-syntax-rule (define+provide [name value] ...)
  (begin
    (define name value) ...
    (provide name ...)))

(define+provide
  [GREGOR-DATA     "./data/gregor-2015-07-26T05:27:18.rktd"]
  [KCFA-DATA       "./data/kcfa-2015-06-25T13:48:52.rktd"]
  [LNM-DATA        "./data/lnm-large-06-28.rktd"]
  [MBTA-DATA       "./data/mbta-2015-09-07T23:17:52.rktd"]
  [MORSECODE-DATA  "./data/morsecode-large-06-27.rktd"]
  [QUAD-DATA       "./data/quad-galicia-07-26.rktd"]
  [SIEVE-DATA      "./data/sieve-2015-06-28T14:08:55.rktd"]
  [SNAKE-DATA      "./data/snake-2015-06-30T14:55:40.rktd"]
  [SUFFIXTREE-DATA "./data/suffixtree-large-06-30.rktd"]
  [SYNTH-DATA      "./data/synth-2015-07-02T01:47:43.rktd"]
  [TETRIS-DATA     "./data/tetris-2015-07-01T16:39:46.rktd"]
  [ZORDOZ-DATA     "./data/zordoz-04-09.rktd"]
)

(define+provide
  [FSM-6.3                   "data/v6.3/fsm-2015-12-10T22:19:30.rktd"]
  [FSMV2-6.3                 "data/v6.3/fsmv2-2015-12-13T11:21:01.rktd"]
  [GREGOR-6.3                "data/v6.3/gregor-2015-12-10T11:04:56.rktd"]
  [KCFA-6.3                  "data/v6.3/kcfa-2015-12-14T03:25:44.rktd"]
  [LNM-6.3                   "data/v6.3/lnm-2015-12-16T20:05:38.rktd"]
  [MBTA-6.3                  "data/v6.3/mbta-2015-12-16T20:12:50.rktd"]
  [MORSECODE-6.3             "data/v6.3/morse-code-2015-12-16T20:17:19.rktd"]
  [QUAD-6.3                  "data/v6.3/quad-2015-12-16T20:21:23.rktd"]
  [QUADU-6.3                 "data/v6.3/quadU-2015-12-07T17:22:40.rktd"]
  [SIEVE-6.3                 "data/v6.3/sieve-2016-01-11T05:43:20.rktd"]
  [SNAKE-6.3                 "data/v6.3/snake-2016-01-11T20:53:41.rktd"]
  [SUFFIXTREE-6.3            "data/v6.3/suffixtree-2016-01-12T01:23:45.rktd"]
  [SYNTH-6.3                 "data/v6.3/synth-2016-01-14T05:00:34.rktd"]
  [TETRIS-6.3                "data/v6.3/tetris-2016-01-14T12:10:28.rktd"]
  [ZORDOZ-6.3                "data/v6.3/zordoz.6.2.900.15-2016-01-15T01:03:57.rktd"]
)

(define+provide
  [MBTA-6.4.0.3 "data/v6.4.0.3/mbta-2016-01-16T06:02:22.rktd"]
  [SNAKE-6.4.0.3 "data/v6.4.0.3/snake-2016-01-16T06:03:19.rktd"]
  [SUFFIXTREE-6.4.0.3 "data/v6.4.0.3/suffixtree-2016-01-16T06:19:37.rktd"]
  [SYNTH-6.4.0.3 "data/v6.4.0.3/synth-2016-01-16T07:34:17.rktd"]
  [TETRIS-6.4.0.3 "data/v6.4.0.3/tetris-2016-01-16T07:39:25.rktd"]
  [ZORDOZ-6.4.0.3 "data/v6.4.0.3/zordoz.6.2.900.15-2016-01-16T08:22:50.rktd"]
)

(define+provide
  [KCFA-6.4.0.4  "data/v6.4.0.4/kcfa-2016-01-17T15:59:32.rktd"]
  [MBTA-6.4.0.4 "data/v6.4.0.4/mbta-2016-01-17T12:23:15.rktd"]
  [SNAKE-6.4.0.4 "data/v6.4.0.4/snake-2016-01-17T12:24:11.rktd"]
  [SUFFIXTREE-6.4.0.4    "data/v6.4.0.4/suffixtree-2016-01-17T12:40:38.rktd"]
  [SYNTH-6.4.0.4    "data/v6.4.0.4/synth-2016-01-17T13:56:20.rktd"]
  [TETRIS-6.4.0.4   "data/v6.4.0.4/tetris-2016-01-17T15:14:17.rktd"]
  [ZORDOZ-6.4.0.4   "data/v6.4.0.4/zordoz.6.2.900.15-2016-01-17T15:57:49.rktd"]
)

