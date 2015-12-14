#lang racket/base

(provide
  count-chaps
  ;; (-> Void)
)

(require (only-in ffi/unsafe _int get-ffi-obj))

(define (count-chaps)
  (displayln "Chaperone Report:\n===")
  (printf "proc_makes   : ~a\n" (get-ffi-obj 'proc_makes #f _int))
  (printf "proc_apps    : ~a\n" (get-ffi-obj 'proc_apps #f _int))
  (printf "proc_wraps   : ~a\n" (get-ffi-obj 'proc_wraps #f _int))
  (printf "struct_makes : ~a\n" (get-ffi-obj 'struct_makes #f _int))
  (printf "struct_apps  : ~a\n" (get-ffi-obj 'struct_apps #f _int))
  (printf "struct_wraps : ~a\n" (get-ffi-obj 'struct_apps #f _int))
  (printf "vec_makes    : ~a\n" (get-ffi-obj 'vec_makes #f _int))
  (printf "vec_apps     : ~a\n" (get-ffi-obj 'vec_apps #f _int)))
