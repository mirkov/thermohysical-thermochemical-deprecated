;; Mirko Vukovic
;; Time-stamp: <2011-02-10 06:41:07 mv-gpl-header.txt>
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

(in-package :thermo)
(export '(make-assael-table-2))

;; H2 thermal conductivity from Assael et al, J.Phys.Chem.Ref.Data,
;; 1986

(defclass assael-86 (external-species-data)
  ((a-coeffs :reader a-coeffs
	     :documentation "a coefficients from table 2")
   (b-coeffs :reader b-coeffs
	     :documentation "b coefficients from table 2")
   (d-coeffs :reader d-coeffs
	     :documentation "D coefficients from table 2")
   (c-coeffs :reader c-coeffs
	     :documentation "C coefficients from table 2"))
  (:documentation "Coefficients for H2 thermal conductivity and viscosity


Annotated contents of tables 2 of Assael et al,
J.Phys.Chem.Ref.Data, Vol 15, No.4, 1986"))

(defmethod initialize-instance :after
    ((container assael-86) &key )
  (destructuring-bind (ii ai bi ci di)
      (mapcar #'(lambda (raw-column)
		  (coerce (remove nil raw-column) 'vector))
	      (loop for (i a b c d) in (data container)
		 collect i into ii
		 collect a into ai
		 collect b into bi
		 when c
		 collect c into ci
		 when d
		 collect d into di
		 finally (return (list ii ai bi ci di))))
    (declare (ignore ii))
    (setf (slot-value container 'a-coeffs) ai
	  (slot-value container 'b-coeffs) bi
	  (slot-value container 'c-coeffs) ci
	  (slot-value container 'd-coeffs) di)))


(defun make-assael-table-2 ()
  (make-instance 'assael-86
		 :data-source "hydrogen-visocity&conductivity-coeffs.dat"
		 :data (read-data-file "hydrogen-visocity&conductivity-coeffs.dat")
		 :species :H2
		 :min-T 100.0
		 :max-T 400.0))

(defmethod lambda-0 ((container assael-86) temperature)
  "Thermal conductivity using the fit formulae of Assael et al, 1986

Comparison iwith data on table 4 gives satisfactory agreement.  It
falls off at low temperature"
  ;; the calculation is done in dimensionless quantities.  Thus, as
  ;; long as I am consistant, such as using Cp and R in the same
  ;; units, I am OK.
  (let* ((T* (/ temperature 33.3))
	 (M (* (cadr (assoc :M (lennard-jones-coeffs :H2)))
	       1e-3))		 ;physics-constants:+proton-mass-sp+))
	 (R physics-constants:+gas-constant-sp+)
	 (rhoD/nu (polyeval T*
			    (d-coeffs container))) ;; (12)
	 (xi (my-utils:polyeval T* (c-coeffs container))) ;; (11)
	 (Cp/R (/ (Cp :H2 temperature) ;; use external calculation
		  R))
	 (Cvint/R (- Cp/R 2.5)) ;; (10)
	 (DELTA (let ((2/pi*xi (/ 2 (* pi xi))))
		  (/ (* 2/pi*xi Cvint/R
			(- 2.5 rhoD/nu))
		     (1+ (* 2/pi*xi (+ (* 5 Cvint/R)
				       rhoD/nu)))))) ;; (5)
	 (4-lhs (+ (* 2.5 (- 1.5 DELTA))
		   (* rhoD/nu (- Cvint/R DELTA)))) ;; (4)
	 ) 
    (* 4-LHS (mu-0 :H2 temperature) R (/ M))))