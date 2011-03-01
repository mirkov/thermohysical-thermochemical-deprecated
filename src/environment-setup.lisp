(in-package :thermo)

(export '(read-data-file
	  *lennard-jones-coeffs* read-LJ-coeffs lennard-jones-coeffs
	  *lemmon-jacobsen-table-iv*
	  +R+ +A+ +k+ +amu+))

(defparameter *data-directory*
  #P"/home/mv/my-software-add-ons/my-lisp/modeling/thermophysical+thermochemical/thermo-data/"
  "Path to the data directory (not including JANAF tables)")

(defparameter *janaf-directory*
  #P"/home/mv/my-software-add-ons/my-lisp/modeling/thermophysical+thermochemical/janaf/"
  "Path to the JANAF data directory")


(defun read-data-file (file &optional (directory *data-directory*))
  "Return contents of `file' in `directory'

Signal error if file not found"
  (with-input-from-file (stream
			 (merge-pathnames file
					  directory)
			 :if-does-not-exist :error)
    (read stream)))





(defparameter *lemmon-jacobsen-table-iv*
  (read-data-file "lemmon-jacobsen-table-iv.dat")
  "Coefficients from Lemmon & Jacobsen, 2004, table IV for the
residual fluid thermal conductivity equations")

(defparameter +R+ physics-constants:+gas-constant-sp+
  "Universal gas constant, 8.3147135 J/K mol")
(defparameter +A+ physics-constants:+avogadros-number-sp+
  "Avogadro's number, 6.0221e23/mol")
(defparameter +k+ physics-constants:+boltzmann-constant-sp+
  "Boltzmann's constant, 1.3807e-23 J/K")
(defparameter +amu+ physics-constants:+atomic-mass-unit-sp+
  "Atomic mass unit, 1.6605e-27 kg")