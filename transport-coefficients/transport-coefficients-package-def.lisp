;;;; package.lisp

(defpackage #:transport-coefficients
  (:nicknames #:tc)
  (:use #:cl #:lisp-unit
	#:defgeneric+default
	#:molecular-potentials
	#:omega-xx)
  (:export :mu1 :mu1-1
	   :lambda1 :lambda1-1
	   :d12 :d12-1
	   :alpha-t :alpha-t-1))

