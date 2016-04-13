#lang racket

(require data/enumerate/lib
         gtp-summarize/summary
         gtp-summarize/bitstring
         gtp-summarize/predict
         gtp-summarize/modulegraph
         glob
         math/statistics)

(define *truth-file* "morsecode-truth.rktd")
(define *falsehood-file* "morsecode-falsehood.rktd")
(define *proj-name* "morsecode")
;; (define *truth-file* "data/predict/no-adapt-tetris-truth.rktd")
;; (define *falsehood-file* "data/predict/no-adapt-tetris-predict.rktd")
;; (define *proj-name* "no-adapt-tetris")

(current-directory "..")
(define truth
  (vector-map
   (compose exact->inexact mean)
   (with-input-from-file *truth-file*
     (λ () (read)))))

(define (map-hash-values f ht)
  (for/hash ([(k v) (in-hash ht)])
    (values k (f v))))
(define falsehood
  (for/hash ([(k v)
              (in-hash
               (with-input-from-file *falsehood-file*
                 (λ () (read))))])
    (values (parse-prediction-config k)
            (exact->inexact (mean v)))))
(define mg (project-name->modulegraph *proj-name*))
(define unty-perf (vector-ref truth 0))
(define slow-configs
  (for/list ([(k v) (in-hash falsehood)]
             #:when (not (optimize-config? k)))
    v))
(define prediction
  (let ()
    (define pad (log2 (vector-length truth)))
    (for/vector ([real (in-vector truth)]
                 [i    (in-naturals)])
      (define bs (natural->bitstring i #:pad pad))
      (predict mg bs falsehood unty-perf))))

(define (logdiff true false)
  (define (log2 n)
    (/ (log n) (log 2)))
  (- (log2 true)
     (log2 false)))
(define errors
  (vector-map logdiff truth prediction))

;; (define (indices->bitstring s len)
;;   (apply string
;;          (for/list ([i (in-range len)])
;;            (if (set-member? s i)
;;                #\1
;;                #\0))))

;; (define (bitstring->indices bs)
;;   (for/set ([i (in-naturals)]
;;             [b (in-string bs)]
;;             #:when (equal? b #\1))
;;     i))

;; (define (in-<-typed bs #:max-card [max-cardinality #f])
;;   (define num-typed (length (set->list (bitstring->indices bs))))
;;   (define mc2 (if max-cardinality
;;                   (min max-cardinality (sub1 num-typed))
;;                   (sub1 num-typed)))
;;   (in-<=-typed bs #:max-card mc2))

;; (define (in-<=-typed bs #:max-card [max-cardinality #f])
;;   (define typed (set->list (bitstring->indices bs)))
;;   (define len (string-length bs))
;;   (define cutoff
;;     (add1
;;      (if max-cardinality
;;          (min max-cardinality (length typed))
;;          (length typed))))
;;   (sequence-map
;;    (λ (x)
;;      (indices->bitstring (list->set x) len))
;;    (apply
;;     sequence-append
;;     (for/list ([k (in-range cutoff)])
;;       (in-combinations typed k)))))

;; (define (predict-and-test module #:version [vsn 6.3] #:cutoff [cutoff 3])
;;   (define path (first (glob (string-append "../data/" (number->string vsn) "/" module "*"))))
;;   (define data (from-rktd path))
;;   (define num-mods (get-num-modules data))

;;   (define (perf-at bs)
;;     (configuration->mean-runtime data bs))
;;   (define deltas (make-hash))

;;   ;; the performance delta corresponding to the interaction of the
;;   ;; typed modules in the bitstring
;;   ;; BitString -> PerformanceDelta
;;   (define (interaction! bs)
;;     (hash-ref!
;;      deltas
;;      bs
;;      (λ () ((perf-at bs) . - . (bottom-up-predict bs)))))
  
;;   (define (bottom-up-predict modules)
;;     (for/sum ([bs< (in-<-typed modules #:max-card cutoff)])
;;       (interaction! bs<)))

;;   (define (predict m)
;;     (max 1
;;          (for/sum ([bs<= (in-<=-typed m #:max-card cutoff)])
;;            (interaction! bs<=))))

;;   (define (test bs)
;;     (define prediction (predict bs))
;;     (define actual (perf-at bs))
;;     (define the-error (- (log2 prediction) (log2 actual)))
;;     `(prediction ,(* 1.0 prediction)
;;                  actual     ,(* 1.0 actual)
;;                  error      ,(* 1.0 the-error)
;;                  bitstring ,bs))

;;   (cons
;;    deltas
;;    (for/hash ([bs (all-configurations data)])
;;      (values bs (test bs)))))

;; (define (log2 n)
;;   (/ (log n) (log 2)))

;; (define (by < f)
;;   (λ xs (apply < (map f xs))))


;; (define (abs-error x)
;;   (abs (sixth x)))
;; (define (organize 
;;         errors)

  ;; (list
  ;;  (list
  ;;   'abs-error
  ;;   (median (by < abs-error) errors)
  ;;   (quantile 0.75 (by < abs-error) errors)
  ;;   (quantile 0.90 (by < abs-error) errors)
  ;;   (quantile 0.99 (by < abs-error) errors))
  ;;  #;
  ;;  (list
  ;;   'neg-errors
  ;;   (quantile 0.75 (by > sixth) (filter (compose negative? sixth) errors))
  ;;   (quantile 0.90 (by > sixth) (filter (compose negative? sixth) errors))
  ;;   (quantile 0.99 (by > sixth) (filter (compose negative? sixth) errors)))))

;; (define (pa module #:version [vsn 6.3] #:cutoff [cutoff 2])
;;   (hash-values (cdr (predict-and-test module #:version vsn #:cutoff cutoff))))

;; (define (blah module #:version [vsn 6.3] #:cutoff [cutoff 2])
;;   (organize (pa module #:version vsn #:cutoff cutoff)))

