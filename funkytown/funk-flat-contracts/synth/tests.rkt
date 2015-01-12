#lang racket

(require "math/array.rkt"
         "synth.rkt"
         "sequencer.rkt"
         "mixer.rkt")

;; (define sin-test (build-array `#(,(seconds->samples 2)) (sine-wave 440)))
;; (define square-test (build-array `#(,(seconds->samples 2)) (square-wave 440)))
;; (define sawtooth-test (build-array `#(,(seconds->samples 2)) (sawtooth-wave 440)))
;; (define inverse-sawtooth-test (build-array `#(,(seconds->samples 2)) (sawtooth-wave 440)))
;; (define triangle-test (build-array `#(,(seconds->samples 2)) (triangle-wave 440)))

;; (emit sin-test "sin.wav")
;; (emit square-test "square.wav")
;; (emit sawtooth-test "sawtooth.wav")
;; (emit inverse-sawtooth-test "inverse-sawtooth.wav")
;; (emit triangle-test "triangle.wav")

;; (plot-signal (build-array #(200) (sine-wave 440)))
;; (plot-signal (build-array #(2000) (square-wave 440)))
;; (plot-signal (build-array #(2000) (sawtooth-wave 440)))
;; (plot-signal (build-array #(2000) (inverse-sawtooth-wave 440)))
;; (plot-signal (build-array #(2000) (triangle-wave 440)))


;; (emit (mix (list (sequence 3 (scale 'C 3 1 'major) 240 sine-wave)
;;                  0.5)
;;            (list (sequence 1 (scale 'C 4 1 'major-arpeggio) 120 sine-wave)
;;                  0.5))
;;       "scale.wav")

;; ;; TODO have a better data entry method, esp. for pauses
;; (emit (sequence 3 (list (chord 'C 4 2 'major-arpeggio)
;;                         (chord 'C 4 1 'minor-arpeggio)
;;                         '(#f . 2))
;;                 120 sine-wave)
;;       "chords.wav")

(time (emit (mix (list (sequence 2 (list (chord 'C 3 3 'major-arpeggio)
                                         (chord 'C 3 3 'minor-arpeggio))
                                 120 square-wave)
                       1)
                 (list (sequence 2 (append (scale 'C 4 1 'major-arpeggio)
                                           (scale 'C 4 1 'minor-arpeggio))
                                 120 sawtooth-wave)
                       2))
            "arpeggio.wav"))
