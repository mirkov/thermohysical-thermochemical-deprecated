(in-package :thermo)

;; variables for storage of lemmon-jacobsen fit coefficients for
;; viscosity and thermal conductivity calculations.  The fits are
;; valid from 100 to 1000K (see Figures in article)

(defclass lemmon-jacobsen-coeffs (external-species-data)
  ((table :reader table
	  :initarg :raw-data
	  :documentation "Species coefficients.

Coefficients of each row are stored in a structure.  The table stores
rows as vector elements.  To get N3, we retrieve row 3 and then
retrieve Ni from the row"))
  (:documentation "Store coefficients from Lemmon & Jacobsen"))

(defmethod print-object ((object lemmon-jacobsen-coeffs) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "for ~a" (species object))))

;; Coefficients in one table row are stored in a structure
(defstruct Lemmon-Jacobsen-04-row  N t d l)

;; Coefficient retreival is done like
;; (Ni table-entry 5) for N_5
(defmethod Ni ((entry lemmon-jacobsen-coeffs) index)
  (lemmon-jacobsen-04-row-n (aref (table entry) index)))

(defmethod ti ((entry lemmon-jacobsen-coeffs) index)
  (aif (lemmon-jacobsen-04-row-t (aref (table entry) index))
       it
       (error "t_~a is not defined" index)))

(defmethod di ((entry lemmon-jacobsen-coeffs) index)
  (aif (lemmon-jacobsen-04-row-d (aref (table entry) index))
       it
       (error "d_~a is not defined" index)))

(defmethod li ((entry lemmon-jacobsen-coeffs) index)
  (aif (lemmon-jacobsen-04-row-l (aref (table entry) index))
       it
       (error "l_~a is not defined" index)))

(defmethod initialize-instance :after
    ((container lemmon-jacobsen-coeffs) &key)
  (let* ((raw-data (data container))
	 (table-rows (length raw-data))
	 (table (make-array table-rows)))
    (loop for (index row-contents) in raw-data
       for i from 0
       do (let ((row-struct (make-lemmon-jacobsen-04-row)))
	    (destructuring-bind (Ni &optional Ti Di Li) row-contents
	      (setf (lemmon-jacobsen-04-row-n row-struct) Ni)
	      (when Ti
		(setf (lemmon-jacobsen-04-row-t row-struct) Ti))
	      (when Di
		(setf (lemmon-jacobsen-04-row-d row-struct) Di))
	      (when Li
		(setf (lemmon-jacobsen-04-row-l row-struct) Li))
	      (setf (aref table i)
		    row-struct))))
    (setf (slot-value container 'table) table)))

(defparameter *LJ04-lambda-coeffs*
  (let* ((file "lemmon-jacobsen-table-iv.dat")
	 (raw-table (read-data-file file)))
    (loop for (species . species-data) in raw-table
	 collect (cons species
		       (make-instance 'lemmon-jacobsen-coeffs
				      :species species
				      :data-source file
				      :data species-data
				      :min-T 100.0
				      :max-T 1000.0))))
  "Thermal conductivity coefficients from table IV")

(defparameter *LJ04-mu-coeffs*
  (let* ((file "lemmon-jacobsen-table-iii.dat")
	 (raw-table (read-data-file file)))
    (loop for (species . species-data) in raw-table
	 collect (cons species
		       (make-instance 'lemmon-jacobsen-coeffs
				      :species species
				      :data-source file
				      :data species-data
				      :min-T 100.0
				      :max-T 1000.0))))
  "Viscosity coefficients from table III")

(defun lj04-lambda-coeffs (species &optional (coeffs *LJ04-LAMBDA-COEFFS*))
  "Return `species' thermal conductivity coefficients for"
  (cdr (assoc species coeffs)))


(defun LJ04-dilute-thermal-conductivity
    (mu-0 Tc* N1 N2 N3 t2 t3)
  "Thermal conductivity (W/m K) in the limit of a dilute gas

 - mu-0 viscosity (Pa s)
 - Tc* -- critical temperature / temperature
 - N1,2,3 & t2,3 -- fitting coefficients (see table IV)

Lemmon & Jacobsen 2004 (3)"
  (* 1e-3 (+ (* N1 mu-0 1e6)
	     (* N2 (expt Tc* t2))
	     (* N3 (expt Tc* t3)))))

(defmethod lambda-0((container lemmon-jacobsen-coeffs) temperature)
  (let ((species (species container)))
    (let ((mu (mu-0 species temperature))
	  (Tc (/ (cadr
		  (assoc :Tc
			 (cdr
			  (assoc species *lennard-jones-coeffs*))))
		 temperature))
	  (N1 (ni container 0))
	  (N2 (ni container 1))
	  (N3 (ni container 2))
	  (t2 (ti container 1))
	  (t3 (ti container 2)))
      (lj04-dilute-thermal-conductivity mu Tc N1 N2 N3 t2 t3))))

