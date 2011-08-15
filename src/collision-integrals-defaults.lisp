;; Mirko Vukovic
;; Time-stamp: <2011-08-15 11:14:43 collision-integrals-defaults.lisp>
;; 
;; Copyright 2011 Mirko Vukovic
;; Distributed under the terms of the GNU General Public License
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :omega-xx)

;; This file deals with the user interface

;; omega-xx% are methods that calculate the reduced integral using a
;; model and temperature.  omega-xx* use the default method.  The
;; latter ones are intended as more user friendly
(export '(*omega-calc-defaults* omega-11% omega-22%))


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

(defun omega-11* (T*)
  "Evaluate omega-11 at T* using the default method"
  (omega-11% t T*))

;;; Methods for invoking the calculation


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

(defun omega-22* (T*)
  "Evaluate omega-22 at T* using the default method"
  (omega-22% t T*))




;; Setup default methods
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


(setf (default-omega-calc-method :omega-11) (make-kee-omega-11-coeffs)
      (default-omega-calc-method :omega-22) (make-kee-omega-22-coeffs))