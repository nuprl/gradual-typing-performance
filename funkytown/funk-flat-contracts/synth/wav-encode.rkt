#lang racket/base

;; Simple WAVE encoder

;; Very helpful reference:
;; http://ccrma.stanford.edu/courses/422/projects/WaveFormat/

(provide write-wav)

(require racket/contract)

(define/contract
  (sequence-length s)
  (-> sequence? natural-number/c)
  (unless (sequence? s) (raise-argument-error 'sequence-length "sequence?" s))
  (for/fold ([c 0]) ([i (in-values*-sequence s)])
    (add1 c)))


;; A WAVE file has 3 parts:
;;  - the RIFF header: identifies the file as WAVE
;;  - fmt subchunk: describes format of the data subchunk
;;  - data subchunk

;; data : sequence of 32-bit unsigned integers
(define/contract
  (write-wav data
                   #:num-channels    [num-channels    1] ; 1 = mono, 2 = stereo
                   #:sample-rate     [sample-rate     44100]
                   #:bits-per-sample [bits-per-sample 16])
  (->* ((vectorof natural-number/c)) (#:num-channels (or/c 1 2) #:sample-rate natural-number/c #:bits-per-sample natural-number/c) void?)
  (define bytes-per-sample (quotient bits-per-sample 8))
  (define/contract
    (write-integer-bytes i [size 4])
    (->* (natural-number/c) (natural-number/c) natural-number/c)
    (write-bytes (integer->integer-bytes i size #f)))
  (define data-subchunk-size
    (* (sequence-length data) num-channels (/ bits-per-sample 8)))

  ;; RIFF header
  (write-bytes #"RIFF")
  ;; 4 bytes: 4 + (8 + size of fmt subchunk) + (8 + size of data subchunk)
  (write-integer-bytes (+ 36 data-subchunk-size))
  (write-bytes #"WAVE")

  ;; fmt subchunk
  (write-bytes #"fmt ")
  ;; size of the rest of the subchunk: 16 for PCM
  (write-integer-bytes 16)
  ;; audio format: 1 = PCM
  (write-integer-bytes 1 2)
  (write-integer-bytes num-channels 2)
  (write-integer-bytes sample-rate)
  ;; byte rate
  (write-integer-bytes (* sample-rate num-channels bytes-per-sample))
  ;; block align
  (write-integer-bytes (* num-channels bytes-per-sample) 2)
  (write-integer-bytes bits-per-sample 2)

  ;; data subchunk
  (write-bytes #"data")
  (write-integer-bytes data-subchunk-size)
  (for ([sample data])
    (write-integer-bytes sample bytes-per-sample)))
