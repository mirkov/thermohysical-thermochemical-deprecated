(in-package :thermo)

(export '(make-shomate-coeffs))

(define-condition non-existant-shomate-table (error)
   ((species :reader species :initarg :species)
    (attempted-path :reader attempthed-path :initarg :attempted-path))
   (:report (lambda (condition stream)
              (format stream "shomate data table not found for species ~a" 
                      (species condition))))
   (:documentation "Error condition for non-existant Shomate file"))

(defun read-shomate-coeffs (species)
  "Read Shomate coefficients for the species"
  (let ((path
	 (probe-file
	 (merge-pathnames (format nil "~a.shomate" species)
			   *data-directory*))))
    (or path
	(error 'non-existant-shomate-table :species species
	       :attempted-path path))
    (let ((coeffs
	   (with-input-from-file (stream path)
	     (loop
		:for coeffs = (read stream nil nil)
		:while coeffs
		:collect coeffs))))
      (values coeffs path))))

(define-condition non-existant-S0-table (error)
   ((species :reader species :initarg :species)
    (attempted-path :reader attempthed-path :initarg :attempted-path))
   (:report (lambda (condition stream)
              (format stream "S0 data table not found for species ~a" 
                      (species condition))))
   (:documentation "Error condition for non-existant S0 file"))

(defun read-S0 (species)
  "Read S0 for the species"
  (let ((path
	 (probe-file
	  (merge-pathnames (format nil "~(~a~).S0" species)
			   *data-directory*))))
    (or path
	(error 'non-existant-S0-table :species species
	       :attempted-path path))
    (my-utils:with-input-file (stream path)
      (read stream t))))

(defclass shomate-coeffs (external-species-data)
  ()
  (:documentation "Store Shomate coefficients for a gas species

A-H coefficients for the Shomate equation stored as a list of lists:

 (sublist1  sublist2  sublist3) 

Each sublist contains the fitting coefficients for a specific
temprature range.  The sublist format is

 ((t-min t-max) a b c d e f g h)

where t-min and t-max denote the valid temperature range (in Kelvin)
and a, ..., h are the fitting coefficients
"))



(defmethod initialize-instance :after ((coefficients shomate-coeffs)
				 &key)
  "Initialize the `shomate-coefficients' class

 - read the data
 - store the raw data, data source and species
 - store T-min, T-max
"
  (multiple-value-bind (coeffs file)
      (read-shomate-coeffs (slot-value coefficients 'species))
    (setf (slot-value coefficients 'data) coeffs
	  (slot-value coefficients 'source) file)
    (multiple-value-bind (min-T max-T)
	(shomate-coeffs-temperature-range coeffs)
      (setf (slot-value coefficients 'min-T) min-T
	    (slot-value coefficients 'max-T) max-T))))

(defun make-shomate-coeffs (species)
  "Return an instance of `thermo-data' that provides thermophysical
data"
  (make-instance 'shomate-coeffs :species species))

(defun shomate-coeffs-temperature-range (coeffs)
  "Return the temperature range over which the stored coefficients are
valid"
  (destructuring-bind (mins maxs)
      (loop for ((min-temp max-temp) . coeffs) in coeffs
	 collect min-temp into mins
	 collect max-temp into maxs
	 finally (return (list mins maxs)))
    (list (apply #'min mins) (apply #'max maxs))))

(defmethod get-shomate-coeffs ((coeffs shomate-coeffs) &key
			       temp index (all t))
  "Return shomate coefficients.  By default return all the stored
coefficients (also corresponds to keyword `all'

Other keywods (index or temp) specify a specific set of coefficients,
either by storage index or by valid temperature"
  (cond
    (index (nth index (data coeffs)))
    (temp (let ((this-coeffs
		 (loop for ((min-temp max-temp) . coeffs) in
		      (data coeffs)
		    when (and (>= temp min-temp)
			      (<= temp max-temp))
		    return coeffs)))
	    (or this-coeffs
		(error "Cannot return ~a shomate coeffs for temperature ~a.  Check tempeature range"
		       (species coeffs) temp))
	    this-coeffs))
    (all (data coeffs))
    (t "Incorrect keyword settings")))



(defclass thermo-data (shomate-coeffs)
  ((S0 :initarg :S0
       :reader S0-gas
       :documentation "S0"))
  (:documentation
   "Gas species thermophysical data, and coefficients for calculating
   them"))









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
  
				      

(defmethod Cp ((coeffs cons) T-K)
    (let ((T-r (/ T-K 1000)))
      (destructuring-bind (A B C D E &rest rest) coeffs
	(declare (ignore rest))
	(+ A
	   (* T-r (+ B
		      (* T-r (+ C
				 (* T-r D)))))
	   (/ E (expt T-r 2))))))
(defmethod Cp ((coeff-container shomate-coeffs) T-K)
  (Cp (get-shomate-coeffs coeff-container :temp T-K) T-K))


(defmethod H-rel ((coeffs cons) T-K)
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
(defmethod H-rel ((coeff-container shomate-coeffs) T-K)
    (H-rel (get-shomate-coeffs coeff-container :temp T-K) T-K))

  

(defmethod S ((coeffs cons) T-K)
  (let ((T-r (/ T-K 1000)))
    (destructuring-bind (A B C D E F G H) coeffs
      (declare (ignore F H))
      (+ (* A (log T-r))
	 (* T-r (+ B
		   (* T-r (+ (/ C 2)
			     (* T-r (/ D 3))))))
	 (- (/ E (* 2 (expt T-r 2))))
	 G))))
(defmethod S ((coeff-container shomate-coeffs) T-K)
  (S (get-shomate-coeffs coeff-container :temp T-K) T-K))
				    
    