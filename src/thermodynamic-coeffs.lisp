(in-package :thermo)

(export '(Cp S0 H-rel
	  ;; make-thermo-data
	  coeffs-temperature-range))

(defclass shomate-coeffs ()
  ((shomate-coeffs :initarg :shomate-coeffs
		   :reader shomate-coeffs
		   :documentation
		   "A-H coefficients for the Shomate equation stored as a list of lists:

 (sublist1  sublist2  sublist3) 

Each sublist contains the fitting coefficients for a specific
temprature range.  The sublist format is

 ((t-min t-max) a b c d e f g h)

where t-min and t-max denote the valid temperature range (in Kelvin)
and a, ..., h are the fitting coefficients
"))
  (:documentation "Store Shomate coefficients for a gas species"))

(defmethod get-shomate-coeffs ((coeffs shomate-coeffs) &key
			       temp index (all t))
  "Return shomate coefficients.  By default return all the stored
coefficients (also corresponds to keyword `all'

Other keywods (index or temp) specify a specific set of coefficients,
either by storage index or by valid temperature"
  (cond
    (index (nth index (shomate-coeffs coeffs)))
    (temp (loop for ((min-temp max-temp) . coeffs) in
	       (shomate-coeffs coeffs)
	       when (and (>= temp min-temp)
			 (<= temp max-temp))
	       return coeffs))
    (all (shomate-coeffs coeffs))
    (t "Incorrect keyword settings")))

(defmethod coeffs-temperature-range ((coeffs shomate-coeffs))
  "Return the temperature range over which the stored coefficients are
valid"
  (destructuring-bind (mins maxs)
      (loop for ((min-temp max-temp) . coeffs) in (shomate-coeffs coeffs)
	 collect min-temp into mins
	 collect max-temp into maxs
	 finally (return (list mins maxs)))
    (list (apply #'min mins) (apply #'max maxs))))

(defclass thermo-data (shomate-coeffs)
  ((S0 :initarg :S0
       :reader S0-gas
       :documentation "S0"))
  (:documentation
   "Gas species thermophysical data, and coefficients for calculating
   them"))



(defun make-thermo-data (species)
  "Return an instance of `thermo-data' that provides thermophysical
data"
  (make-instance 'thermo-data
		 :shomate-coeffs (read-shomate-coeffs species)
		 :S0 (read-s0 species)))

(defun read-shomate-coeffs (species)
  "Read Shomate coefficients for the species"
  (with-input-from-file (stream
			     (merge-pathnames
			      (format nil "~a.shomate" species)
			      *data-directory*))
    (loop
       :for coeffs = (read stream nil nil)
       :while coeffs
       :collect coeffs)))

(defun read-S0 (species)
  "Read S0 for the species"
  (my-utils:with-input-file (stream
			     (merge-pathnames
			      (format nil "~a.S0" species)
			      *data-directory*))
    (read stream t)))



(define-test Cp/H-rel/S0
  (let ((XX (init-thermo-data 'XX))
	(*epsilon* 0.001))
    (let ((data '((Temp 100 C0 29.10 S0 159.8 H-rel -5.77)
		  (Temp 500 C0 29.58 S0 206.7 H-rel 5.91)))
	  (coeffs (rest (get-shomate-coeffs XX :index 0))))
      (assert-numerical-equal (getf (first data) 'C0)
			      (Cp 100 coeffs) 0)
      (assert-numerical-equal (getf (first data) 'S0)
			      (S0 100 coeffs) 0)
      (assert-numerical-equal (getf (first data) 'H-rel)
			      (H-rel 100 coeffs) 0)
      (assert-numerical-equal (getf (second data) 'C0)
			      (Cp 500 coeffs) 0)
      (assert-numerical-equal (getf (second data) 'S0)
			      (S0 500 coeffs) 0)
      (assert-numerical-equal (getf (second data) 'H-rel)
			      (H-rel 500 coeffs) 0))
    (let ((data '((Temp 500 C0 29.58 S0 206.7 H-rel 5.91)
		  (Temp 2000 C0 35.98 S0 252.1 H-rel 56.14)))
	  (coeffs (rest (get-shomate-coeffs XX :index 1))))
      (assert-numerical-equal (getf (first data) 'C0)
			      (Cp 500 coeffs) 1)
      (assert-numerical-equal (getf (first data) 'S0)
			      (S0 500 coeffs) 1)
      (assert-numerical-equal (getf (first data) 'H-rel)
			      (H-rel 500 coeffs) 1)
      (assert-numerical-equal (getf (second data) 'C0)
			      (Cp 2000 coeffs) 1)
      (assert-numerical-equal (getf (second data) 'S0)
			      (S0 2000 coeffs) 1)
      (assert-numerical-equal (getf (second data) 'H-rel)
			      (H-rel 2000 coeffs) 1))
    (let ((data '((Temp 2000 C0 35.97 S0 252.1 H-rel 56.14)
		  (Temp 6000 C0 38.27 S0 293 H-rel 205.8)))
	  (coeffs (rest (get-shomate-coeffs XX :index 2))))
      (assert-numerical-equal (getf (first data) 'C0)
			      (Cp 2000 coeffs) 2)
      (assert-numerical-equal (getf (first data) 'S0)
			      (S0 2000 coeffs) 2)
      (assert-numerical-equal (getf (first data) 'H-rel)
			      (H-rel 2000 coeffs) 2)
      (assert-numerical-equal (getf (second data) 'C0)
			      (Cp 6000 coeffs) 2)
      (assert-numerical-equal (getf (second data) 'S0)
			      (S0 6000 coeffs)2)
      (assert-numerical-equal (getf (second data) 'H-rel)
			      (H-rel 6000 coeffs) 2))))
  
				      

(defgeneric Cp (T-K coeffs)
  (:documentation "Calculate Cp for given temperature and using the
  coefficients.  The coefficients can be specified explicitly, or as a
  container class")
  (:method (T-K (coeffs cons))
    (let ((T-r (/ T-K 1000)))
      (destructuring-bind (A B C D E &rest rest) coeffs
	(declare (ignore rest))
	(+ A
	   (* T-r (+ B
		      (* T-r (+ C
				 (* T-r D)))))
	   (/ E (expt T-r 2))))))
  (:method (T-K (coeff-container shomate-coeffs))
    (aif (get-shomate-coeffs coeff-container :temp T-K)
	 (Cp T-K it)
	 (error "Temperature ~a is outside the allowed range ~a" T-K
		(coeffs-temperature-range coeff-container))))
  (:method (T-K (species symbol))
    (Cp T-K (make-thermo-data species))))


(defgeneric H-rel (T-K coeffs)
  (:documentation "Calculate relative enthalpy for given temperature.
  The coefficients are supplied either explicitly or as a container
  class")
  (:method (T-K (coeffs cons))
    (let ((T-r (/ T-K 1000)))
      (destructuring-bind (A B C D E F G H) coeffs
	(declare (ignore G))
	(+ (* T-r (+ A
		     (* T-r (+ (/ B 2)
			       (* T-r (+ (/ C 3)
					 (* T-r (/ D 4))))))))
	   (- (/ E T-r))
	   F
	   (- H)))))
  (:method (T-K (coeff-container shomate-coeffs))
    (aif (get-shomate-coeffs coeff-container :temp T-K)
	 (H-rel T-K it)
	 (error "Temperature ~a is outside the allowed range ~a" T-K
		(coeffs-temperature-range coeff-container))))
  (:method (T-K (species symbol))
    (H-rel T-K (make-thermo-data species))))
  

(defgeneric S0 (T-K coeffs)
  (:documentation "Calculate entropy for given temperature.
  The coefficients are supplied either explicitly or as a container
  class")
  (:method (T-K (coeffs cons))
    (let ((T-r (/ T-K 1000)))
      (destructuring-bind (A B C D E F G H) coeffs
	(declare (ignore F H))
	(+ (* A (log T-r))
	   (* T-r (+ B
		     (* T-r (+ (/ C 2)
			       (* T-r (/ D 3))))))
	   (- (/ E (* 2 (expt T-r 2))))
	   G))))
  (:method (T-K (coeff-container shomate-coeffs))
      (aif (get-shomate-coeffs coeff-container :temp T-K)
	 (S0 T-K it)
	 (error "Temperature ~a is outside the allowed range ~a" T-K
		(coeffs-temperature-range coeff-container))))
  (:method (T-K (species symbol))
    (S0 T-K (make-thermo-data species))))
				    
    