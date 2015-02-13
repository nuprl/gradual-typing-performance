#lang scheme/base 

(require 
  (for-label lang/htdp-beginner-abbr
             teachpack/2htdp/universe
	     teachpack/2htdp/batch-io
	     (except-in teachpack/htdp/image image=? image?)))

(provide
 (for-label
  (all-from-out lang/htdp-beginner-abbr
                teachpack/2htdp/universe
                teachpack/2htdp/batch-io
                teachpack/htdp/image)))
