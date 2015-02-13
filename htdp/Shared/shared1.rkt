#lang scheme/base 

(require 
  (for-label (except-in lang/htdp-beginner e)
             teachpack/2htdp/universe
	     teachpack/2htdp/batch-io
	      (except-in 2htdp/image image?)
	     #;
	     (except-in teachpack/htdp/image image=? image?)))

(provide
 (for-label
  (all-from-out lang/htdp-beginner
                teachpack/2htdp/universe
                teachpack/2htdp/batch-io
                2htdp/image)))
