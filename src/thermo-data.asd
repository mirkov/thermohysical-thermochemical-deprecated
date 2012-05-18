(asdf:defsystem :thermo-data
  :components
  ((:module "thermo-setup"
	    :pathname #P"./"
	    :components ((:file "package-def")
			 (:file "environment-setup"
				:depends-on ("package-def"))
			 (:file "external-coeffs"
				:depends-on ("package-def"))
			 (:file "constant-coeff"
                                :depends-on ("external-coeffs"))
			 (:file "spec-dep-method-setup")))
   (:module "thermodynamic-coefficients"
	    :pathname #P"./"
	    :depends-on ("thermo-setup")
	    :components ((:file "tc-external-interface")
			 (:file "constant-thermo-coeffs"
				:depends-on ("tc-external-interface"))
			 (:file "shomate-fits"
				:depends-on ("tc-external-interface"))
			 (:file "janaf-coefficients"
				:depends-on ("tc-external-interface"))))
   #|(:module "collision-parameters"
	    :pathname #P"./"
	    :depends-on ("thermo-setup")
	    :components ((:file "lennard-jones-coeffs")
			 (:file "collision-integrals")))|#
   (:module "transport-coefficients"
	    ;; coefficients depend on atomic parameter and transport
	    ;; models.  They are obtained by interpolating tables and
	    ;; using fitting formulae
	    :pathname #P"./"
	    :depends-on ("thermo-setup"
			 #|"collision-parameters"|#)
	    :components ((:file "transport-coeffs-external-interface")
			 (:file "lemmon-jacobsen-2004-coeffs")
			 (:file "assael-et-al-H2-coeffs")
			 (:file "kt-transport-coeffs")))
   (;; define defaults for a user-friendly defaults
    :module "user-setup"
	    :pathname #P"./"
	    :depends-on ("thermo-setup"
			 "transport-coefficients"
			 #|"collision-parameters"|#
			 "thermodynamic-coefficients")
	    :components (;; activate default methods
			 (:file "default-methods")))

   (:module "formulary"
	    ;; implements formulae (such as pv=nrt etc)
	    :pathname #P"./"
	    :depends-on ("thermo-setup")
	    :components ((:file "formulary"))))
  :depends-on ("cl-utilities"
	       "my-utils"
	       "anaphora"
	       :alexandria
	       :gsll
	       :mv-grid-utils
	       :molecular-potentials
	       :collision-integrals
	       "physics-constants"))

(asdf:defsystem :thermo-user
  :components
  ((:file "thermo-user-package-def")
   (:file "thermo-user-setup"
	  :depends-on ("thermo-user-package-def")))
  :depends-on (:thermo-data
	       :mv-grid-utils
	       :mv-gnuplot))