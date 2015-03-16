#lang racket/base
(require "ukkonen2.rkt"
         "label.rkt"
         "structs.rkt")

;;  (define tree (new-suffix-tree))
;;  (suffix-tree-add! tree (make-label "00000100$"))

(enable-ukkonen-debug-messages)

(let-values (((tree) (new-suffix-tree))
             ((label-1 label-2) (values (make-label "a$")
                                        (make-label "a!"))))
  (suffix-tree-add! tree label-1)
  (suffix-tree-add! tree label-2))



;;     (let-values (((tree) (new-suffix-tree))
;;                  ((label-1 label-2) (values (make-label "aaazzz$")
;;                                             (make-label "aaazza!"))))
;;       (suffix-tree-add! tree label-1)
;;       (suffix-tree-add! tree label-2))


; ;; Quick and dirty code to get at a new root and tree.
; (define (root-and-tree)
;   (let* ((tree (new-suffix-tree))
;          (root (suffix-tree-root tree)))
;     (values root tree)))


;     (let-values (((root tree) (root-and-tree)))
;       (let ((leaf
;              (node-add-leaf! root (make-label "00$"))))
;         (let-values (((extension-node extension-offset i*)
;                       (find-next-extension-point root
;                                                  (make-label "001$")
;                                                  1 1)))
;           'ok)))
;;           (assert-eq? leaf extension-node)
;;           (assert-equal? 1 extension-offset)
;;           (assert-equal? 2 i*))))
