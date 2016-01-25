#lang racket/base

;; Script for counting chaperones in an instrumented version of Racket.
;;
;; Usage:
;; - Apply the patch from the "Chaperones & Impersonators" paper
;;   (Adapt from the file 'count-chap-utils.patch' in this folder
;;    or download from 'http://cs.brown.edu/~sstrickl/chaperones/')
;; - Add a (require benchmark-utils) to the top of your program
;; - Add a (count-chaps) to the bottom of your program;
;;   results will be saved to the file (*count-chaps-out*).
;;
;; Example:
;;   #lang typed/racket/base
;;   (require benchmark-util)
;;   ....
;;   (parameterize ([*count-chaps-out* "output.rktd"])
;;     (count-chaps))
;;   <end-of-file>
;;
;; Obviously the hardest part is applying the patch.
;; Feel free to email 'types@ccs.neu.edu' if I'm still around.

(provide
  *count-chaps-out*
  ;; (Parameterof Path-String)

  count-chaps
  ;; (-> Void)
  ;; Saves chaperone counts to `chaps.rktd`, appends if file exists

  (struct-out chaps)
  ;; Undocumented
)

(require
  (only-in ffi/unsafe _array in-array _int get-ffi-obj))

;; =============================================================================

(struct chaps (
  proc_makes
  proc_apps
  proc_wraps
  proc_maxdepth
  proc_depth*

  struct_makes
  struct_apps
  struct_wraps
  struct_maxdepth
  struct_depth*

  vec_makes
  vec_apps
  vec_wraps
  vec_maxdepth
  vec_depth*
) #:prefab )

(define *count-chaps-out* (make-parameter "chaps6.3.rktd"))

;; Fill a `chaps` struct with data from the runtime, then print it.
(define (count-chaps)
 (with-output-to-file (*count-chaps-out*) #:exists 'append
   (lambda ()
     (let ([v (make-vector 12 #f)])
       (vector-set-performance-stats! v)
       (displayln v)
       (newline))
     (writeln
      (chaps
        ;; -- fun
        (get-ffi-obj 'proc_makes #f _int)
        (get-ffi-obj 'proc_apps #f _int)
        (get-ffi-obj 'proc_wraps #f _int)
        (get-ffi-obj 'proc_maxdepth #f _int)
        (for/vector ([c (in-array (get-ffi-obj 'proc_depth #f (_array _int 901)))]
                   [i (in-naturals)]
                   #:when (not (zero? c)))
          (cons i c))
        ;; -- struct
        (get-ffi-obj 'struct_makes #f _int)
        (get-ffi-obj 'struct_apps #f _int)
        (get-ffi-obj 'struct_wraps #f _int)
        (get-ffi-obj 'struct_maxdepth #f _int)
        (for/vector ([c (in-array (get-ffi-obj 'struct_depth #f (_array _int 901)))]
                   [i (in-naturals)]
                   #:when (not (zero? c)))
          (cons i c))
        ;; -- vector
        (get-ffi-obj 'vec_makes #f _int)
        (get-ffi-obj 'vec_apps #f _int)
        (get-ffi-obj 'vec_wraps #f _int)
        (get-ffi-obj 'vec_maxdepth #f _int)
        (for/vector ([c (in-array (get-ffi-obj 'vec_depth #f (_array _int 901)))]
                   [i (in-naturals)]
                   #:when (not (zero? c)))
          (cons i c))))
     (void))))
