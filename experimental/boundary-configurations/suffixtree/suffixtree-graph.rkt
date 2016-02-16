#lang racket/base

(require data/bit-vector
         graph
         (only-in math mean)
         racket/dict
         racket/file
         racket/format
         racket/list
         racket/math)

(define suffixtree-graph
  ;; A -> B means A depends on B
  (directed-graph '((main lcs)
                    (lcs ukkonen)
                    (lcs structs)
                    (lcs label)
                    (lcs data)
                    (ukkonen structs)
                    (ukkonen label)
                    (ukkonen data)
                    (structs label)
                    (structs data)
                    (label data))))

(define config-boundary-mapping
  '(((out lcs ukkonen)     .  000001-0)
    ((in ukkonen data)     .  000001-a)
    ((in ukkonen label)    .  000001-b)
    ((in ukkonen structs)  .  000001-c)
    ((out lcs structs)     .  000010-0)
    ((out ukkonen structs) .  000010-1)
    ((in structs data)     .  000010-a)
    ((in structs label)    .  000010-b)
    ((in main lcs)         .  000100-a)
    ((out main lcs)        .  001000-0)
    ((in lcs data)         .  001000-a)
    ((in lcs label)        .  001000-b)
    ((in lcs structs)      .  001000-c)
    ((in lcs ukkonen)      .  001000-d)
    ((out lcs label)       .  010000-0)
    ((out structs label)   .  010000-1)
    ((out ukkonen label)   .  010000-2)
    ((in label data)       .  010000-a)
    ((out label data)      .  100000-0)
    ((out lcs data)        .  100000-1)
    ((out structs data)    .  100000-2)
    ((out ukkonen data)    .  100000-3)))

(define costs
  #hash((000000   . [4588 4572 4676])
        (100000-0 . [64168 64248 63816])
        (100000-1 . [5144 5136 5060])
        (100000-2 . [8916 8980 9028])
        (100000-3 . [5492 5460 5380])
        (010000-a . [69632 69612 69652])
        (010000-0 . [5956 5976 5972])
        (010000-1 . [8520 8444 8348])
        (010000-2 . [6244 6124 6156])
        (001000-0 . [4268 4144 4156])
        (001000-a . [4896 4852 5036])
        (001000-b . [4912 4772 4744])
        (001000-c . [4336 4328 4320])
        (001000-d . [4280 4224 4312])
        (000100-a . [4532 4564 4516])
        (000010-0 . [3812 3764 3784])
        (000010-1 . [5884 5936 5764])
        (000010-a . [10248 10136 10192])
        (000010-b . [20828 20488 20264])
        (000001-0 . [4616 4676 4960])
        (000001-a . [5916 5908 5988])
        (000001-b . [7892 7304 7324])
        (000001-c . [9520 9904 9840])))

;; (Listof Symbol) -> Natural
;; Given a list of module names that are typed, give the prediction
(define (prediction typed-mods)
  (define baseline-cost (mean (hash-ref costs '000000)))
  (+ baseline-cost
     (for/sum ([mod (in-list typed-mods)])
       (define imports
         (get-neighbors suffixtree-graph mod))
       (define exports
         (get-neighbors (transpose suffixtree-graph) mod))
       ;; FIXME: abstract these two loops
       (+ 0.0
          (for/sum ([export (in-list exports)]
                    #:unless (member export typed-mods))
            (define bconfig
              (dict-ref config-boundary-mapping `(out ,export ,mod)))
            (define delta
              (- (mean (hash-ref costs bconfig)) baseline-cost))
            delta)
          (for/sum ([import (in-list imports)]
                    #:unless (member import typed-mods))
            (define bconfig
              (dict-ref config-boundary-mapping `(in ,mod ,import)))
            (define delta
              (- (mean (hash-ref costs bconfig)) baseline-cost))
            delta)))))

(define labels '(data label lcs main structs ukkonen))
(define data (file->value "suffixtree-2016-02-16T00:13:52.rktd"))

(for ([i (in-range 64)])
  (define bitstring
    (~r #:base 2
        #:min-width 6
        #:pad-string "0"
        i))
  (define bv (string->bit-vector bitstring))
  (define config
    (for/list ([on? (in-bit-vector bv)]
               [label (in-list labels)]
               #:when on?)
      label))
  (define actual (vector-ref data i))
  (define actual-mean (mean actual))
  (define the-prediction (prediction config))
  (printf "~a : prediction ~a actual ~a error ~a%~n"
          bitstring
          (~a #:min-width 7 (exact-round the-prediction))
          (~a #:min-width 7 (exact-round actual-mean))
          (exact-round
           (* 100 (- (/ the-prediction actual-mean) 1)))))
