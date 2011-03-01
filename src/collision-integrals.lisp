(in-package :thermo)

;; Calculations of collision integrals
(export '(omega-11 omega-22 *omega-calc-defaults*
	  omega-11% omega-22% 
	  default-omega-calc-method
	  make-lj04-omega-22-coeffs
	  make-kee-omega-11-coeffs
	  make-kee-omega-22-coeffs))

;; We offer the formulae from Kee and from Lemmon&Jacobsen

;;; setting and changing default methods
(defparameter *omega-calc-defaults*
  (list (list :omega-11)
	(list :omega-22)))


(define-test default-omega-calc-method
  (assert-equal 'kee-omega-11-coeffs
		(class-name (class-of
			     (default-omega-calc-method :omega-11)))))

(defun default-omega-calc-method (coefficient)
  (cdr (assoc coefficient *omega-calc-defaults*)))

(defun set-default-omega-calc-method (coefficient object)
  (setf (cdr (assoc coefficient *omega-calc-defaults*))
	object))

(defsetf default-omega-calc-method set-default-omega-calc-method)


;;; Coefficient containers

;; utilities
(defclass omega-fit-coeffs ()
  ((coeffs-vector :initarg :coeffs-vector
	   :reader coeffs-vector)
   (doc :initarg :doc
		  :reader doc
		  :documentation "Document the coeffs"))
  (:documentation "Base class for storing coefficients of Omega.

It is inherited by classes that store specific coefficients.
The coefficients are stored in a vector"))

;; 2-term exponent formulae for Omega from Kee et al
(defun omega-2-term-exponent-fit (T* c1 c2 c3 c4)
  "omega fitting function Kee et al, (12.6,7)"
  (+ (* c1 (expt T* (- c2)))
     (expt (+ T* c3) (- c4))))


(defclass Kee-omega-11-coeffs (omega-fit-coeffs)
  ())
(defmethod initialize-instance :after ((container  Kee-omega-11-coeffs) &key)
  (setf 
   (slot-value container 'doc)
   "Collision integral 11

Kee et al, (12.6) and table 12.1"
   (slot-value container 'coeffs-vector)
   #(1.0548 0.15504 0.55909 2.1705)))


(defun make-kee-omega-11-coeffs ()
  (make-instance 'Kee-omega-11-coeffs))

(defclass Kee-omega-22-coeffs (omega-fit-coeffs)
  ())
(defmethod initialize-instance :after ((container  Kee-omega-22-coeffs) &key)
  (setf 
   (slot-value container 'doc)
   "Collision integral 22

Kee et al, (12.6) and table 12.1"
   (slot-value container 'coeffs-vector)
   #(1.0413 0.11930 0.43628 1.6041)))

(defun make-kee-omega-22-coeffs ()
  (make-instance 'kee-omega-22-coeffs))

;; 4-term log/exp expansion from Lemmon & Jacobsen
(defclass LJ04-omega-22-coeffs (omega-fit-coeffs)
  ())
(defmethod initialize-instance :after ((container LJ04-omega-22-coeffs) &key)
  (setf (slot-value container 'coeffs-vector)
	#(0.431 -0.4623 0.08406 0.005341 -0.00331)
	(slot-value container 'doc)
	"Lemon & Jacobsen, 2004

Agreement with the two exponent expansion breaks down for T* < 0.2 and
T* > 4"))

(defun make-lj04-omega-22-coeffs ()
  (make-instance 'lj04-omega-22-coeffs))


;;; Methods for invoking the calculation

(defgeneric omega-11% (model reduced-temperature)
  (:documentation "Evaluate the omega-11 `model' at `reduced-temperature'
If model is specified as `t', use default model")
  (:method ((model (eql t)) reduced-temperature)
    (funcall #'omega-11% (default-omega-calc-method :omega-11)
	     reduced-temperature))
  (:method ((coeff-container Kee-omega-11-coeffs)
	    T*)
    (with-slots (coeffs-vector) coeff-container
      (omega-2-term-exponent-fit T* 
				 (aref coeffs-vector 0) (aref coeffs-vector 1)
				 (aref coeffs-vector 2) (aref coeffs-vector 3)))))

(defun omega-11 (T*)
  "Evaluate omega-11 at T* using the default method"
  (omega-11% t T*))

(defgeneric omega-22% (model reduced-temperature)
  (:documentation "Evaluate the omega-22 `model' at `reduced-temperature'
If model is specified as `t', use default model")
  (:method ((model (eql t)) reduced-temperature)
    (funcall #'omega-22% (default-omega-calc-method :omega-22)
	     reduced-temperature))
  (:method ((coeff-container Kee-omega-22-coeffs)
	    T*)
    (with-slots (coeffs-vector) coeff-container
      (omega-2-term-exponent-fit T* 
				 (aref coeffs-vector 0) (aref coeffs-vector 1)
				 (aref coeffs-vector 2) (aref coeffs-vector 3))))
  (:method ((coeff-container lj04-omega-22-coeffs) T*)
    (let ((logT* (log T*)))
      (exp (polyeval logT* (slot-value coeff-container 'coeffs-vector))))))

(defun omega-22 (T*)
  "Evaluate omega-22 at T* using the default method"
  (omega-22% t T*))


