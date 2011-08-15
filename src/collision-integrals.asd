(asdf:defsystem :collision-integrals
  :components
  ((:file "collision-integrals-package-def")
   (:file "lj-collision-integrals"
	  :depends-on ("collision-integrals-package-def"))
   (:file "hs-collision-integrals"
	  :depends-on ("collision-integrals-package-def"))
   (:file "collision-integrals-defaults"
	  :depends-on ("collision-integrals-package-def"))
   (:file "lennard-jones-coeffs"
	  :depends-on ("collision-integrals-package-def"))
   (:file "hard-sphere-coeffs"
	  :depends-on ("collision-integrals-package-def")))
  :depends-on (:lisp-unit
	       :alexandria
	       :my-utils
	       :physics-constants))
