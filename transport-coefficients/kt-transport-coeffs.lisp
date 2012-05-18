;; Mirko Vukovic
;; Time-stamp: <2012-05-15 13:55:05 kt-transport-coeffs.lisp>
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

(in-package :#tc)

;; transport coefficients formulae based on kinetic theory, omega,
;; epsilon and the collision integrals

(defmethod mu% ((model (eql 'kt))
		(coefficients lennard-jones-6/12-potential) temperature)
  "Calculate viscosity using kinetic theory (Kee et al, 12.100)

To prevent floating underflow in single-float the 1e-10^2 factor from
sigma^2 (sigma being in Angstrom) is moved into the square root in the
numerator, and is used to `normalize' +amu+ and +k+
"
  (declare (ignore model))
  (let ((coeff (* (/ 5 16)
		  (/ (expt 10 -1.5)
		     +NA20+)
		  (sqrt (/ +R+ +pi+)))))
    (with-slots (mass sigma epsilon/K) coefficients
      (* coeff
	 (/ (* mass temperature)
	    (* (expt sigma 2)
	       (omega*-22 (/ temperature epsilon/K))))))))

(define-test mu%-lj
  ;; Test of LJ viscosities vs tabulated values (CRC, p.6-14).  I use
  ;; a tolerance of 5%, because much lower than that the tests will
  ;; fail
  (let ((lisp-unit:*epsilon* 5e-2))
    (let ((coeffs (make-species-lennard-jones-6/12-potential :Ar)))
      (assert-number-equal 16e-6 (mu% coeffs 200.0))
      (assert-number-equal 22.9e-6 (mu% coeffs 300.0))
      (assert-number-equal 27.8e-6 (mu% coeffs 380.0)))
    (let ((coeffs (make-species-lennard-jones-6/12-potential :N2)))
      (assert-number-equal 12.9e-6 (mu% coeffs 200.0))
      (assert-number-equal 18e-6 (mu% coeffs 300.0))
      (assert-number-equal 29.5e-6 (mu% coeffs 600.0)))))
  

(defmethod mu% ((model (eql 'kt))
		(coefficients hard-sphere-potential) temperature)
  "Hard-sphere viscosity

Kee et al, (12.50)"
  (let ((coeff (* (/ 5 16)
		  (/ (expt 10 -1.5)
		     +NA20+)
		  (sqrt (/ +R+ +pi+)))))
    (with-slots (mass sigma epsilon/K) coefficients
      (* coeff
	 (/ (* mass temperature)
	    (* (expt sigma 2)))))))



(defmethod lambda% ((model (eql 'kt-ig))
		     (coefficients lennard-jones-6/12-potential) temperature)
  "Thermal conductivity of an ideal gas using the Lennard-Jones model

Kee et al, 12.101"
  (let ((coeff (* (/ 5 16)
		  (/ (expt 10 -1.5)
		     +NA20+)
		  (sqrt (/ (expt +R+ 3)
			   +pi+))
		  1e3
		  (/ 3 2)
		  )))
    (with-slots (species sigma epsilon/K mass) coefficients
      (* coeff
	 (/ (sqrt (/ temperature
		     mass))
	    (* (expt sigma 2)
	       (omega*-22 (/ temperature epsilon/K))))))))

(defmethod lambda% ((model (eql 'kt-ig))
		    (coefficients hard-sphere-potential) temperature)
  "Thermal conductivity of an ideal gas using the hard-sphere model

Kee et al, 12.57"
  (let ((coeff (* (/ 5 16)
		  (/ (expt 10 -1.5)
		     +NA20+)
		  (sqrt (/ (expt +R+ 3)
			   +pi+))
		  1e3
		  (/ 3 2)
		  )))
    (with-slots (species sigma epsilon/K mass) coefficients
      (* coeff
	 (/ (sqrt (/ temperature
		     mass))
	    (expt sigma 2))))))

(define-test lambda%-lj
  ;; Test of LJ thermal conductivities vs tabulated values (CRC,
  ;; p.6-14).  I use a tolerance of 5%, because much lower than that
  ;; the tests will fail
  (let ((lisp-unit:*epsilon* 5e-2))
    (let ((coeffs (make-species-lennard-jones-6/12-potential :Ar)))
      (assert-number-equal 12.5e-3 (lambda-0 coeffs 200.0))
      (assert-number-equal 17.9e-3 (lambda-0 coeffs 300.0))
      (assert-number-equal 21.7e-3 (lambda-0 coeffs 380.0)))
    ;; the thermal conductivity for N2, O2 fails
    (let ((coeffs (make-species-lennard-jones-6/12-potential :N2)))
      (assert-number-equal 18.4e-3 (lambda-0 coeffs 200.0))
      (assert-number-equal 25.8e-3 (lambda-0 coeffs 300.0))
      (assert-number-equal 44.5e-3 (lambda-0 coeffs 600.0)))
    (let ()
      (assert-number-equal 26.3e-3 (lambda-0 (make-species-lennard-jones-6/12-potential :O2) 300.0)))))

      