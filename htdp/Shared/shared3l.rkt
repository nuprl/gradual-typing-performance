#lang at-exp racket/base 

(require
  scribble/manual
  (for-label (except-in lang/htdp-intermediate-lambda length)
             (except-in teachpack/2htdp/universe left right)
	     teachpack/2htdp/batch-io
	     (except-in teachpack/2htdp/image #;image=? image?)))

(provide
 (for-label
  (all-from-out lang/htdp-intermediate-lambda
                teachpack/2htdp/universe
                teachpack/2htdp/batch-io
		teachpack/2htdp/image)))

(define (mt) @racket['()])

(define Maybe   @tech[#:tag-prefixes '("sim-dd")]{Maybe})
(define List-of @tech[#:tag-prefixes '("sim-dd")]{List-of})
(define List    @tech{List})
(define CP      @tech[#:tag-prefixes '("sim-dd")]{CP})
(define Lon     @tech[#:tag-prefixes '("sim-dd")]{Lon})
(define Los     @tech[#:tag-prefixes '("sim-dd")]{Los})

(provide mt Maybe List-of List CP Lon Los)

