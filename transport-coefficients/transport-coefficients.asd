;;;; transport-coefficients.asd

(asdf:defsystem #:transport-coefficients
  :serial t
  :depends-on (#:alexandria
               #:lisp-unit
               #:physics-constants
	       #:molecular-potentials
	       #:collision-integrals
	       #:defgeneric+default)
  :components
  ((:module "package-setup"
	    :pathname #P"./"
	    :components ((:file "transport-coefficients-package-def")))
   (:module "setup"
	    :pathname #P"./"
	    :components ((:file "fundamental-constants")
			 (:file "generic-functions")))
   (:module "transport-coefficients"
	    :pathname #P"./"
	    :components ((:file "cck-transport-coefficients")))))


