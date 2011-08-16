;; Mirko Vukovic
;; Time-stamp: <2011-08-16 13:48:57 kt-transport-coeffs.lisp>
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

;; transport coefficients formulae based on kinetic theory, omega,
;; epsilon and the collision integrals

(defmethod mu-0 ((coefficients lj-coeffs) temperature)
  "Calculate viscosity using kinetic theory (Kee et al, 12.100)

To prevent floating underflow in single-float the 1e-10^2 factor from
sigma^2 (sigma being in Angstrom) is moved into the square root in the
numerator, and is used to `normalize' +amu+ and +k+
"
  (with-slots (m sigma epsilon/K) coefficients
    (* (/ 5 16)
       (/ (sqrt (* +pi+ m (/ +amu+ 1e-20) (/ +k+ 1e-20) temperature))
	  (* +pi+ (expt sigma 2)
	     (omega-22* (/ temperature epsilon/K)))))))


(define-test mu-0-lj
  ;; Test of LJ viscosities vs tabulated values (CRC, p.6-14).  I use
  ;; a tolerance of 5%, because much lower than that the tests will
  ;; fail
  (let ((lisp-unit:*epsilon* 5e-2))
    (let ((coeffs (make-lj-coeffs :Ar)))
      (assert-number-equal 16e-6 (mu-0 coeffs 200.0))
      (assert-number-equal 22.9e-6 (mu-0 coeffs 300.0))
      (assert-number-equal 27.8e-6 (mu-0 coeffs 380.0)))
    (let ((coeffs (make-lj-coeffs :N2)))
      (assert-number-equal 12.9e-6 (mu-0 coeffs 200.0))
      (assert-number-equal 18e-6 (mu-0 coeffs 300.0))
      (assert-number-equal 29.5e-6 (mu-0 coeffs 600.0)))))
  

(defmethod mu-0 ((coefficients hs-coeffs) temperature)
  "Hard-sphere viscosity

Kee et al, (12.50)"
  (with-slots (m sigma) coefficients
    (* (/ 5 16)
       (/ (sqrt (* +pi+ m (/ +amu+ 1e-20) (/ +k+ 1e-20) temperature))
	  (* +pi+ (expt sigma 2))))))



(defmethod lambda-0 ((coefficients lj-coeffs) temperature)
  "Thermal conductivity using the Lennard-Jones model

Kee et al, 12.101"
  (with-slots (species sigma epsilon/K m) coefficients
    (let ((Cv (Cv species temperature)))
    (* 
       (/ 25d0
	  (* 32d0 (sqrt +pi+)))
       (sqrt (/ (* +k+ temperature)
		(* m +amu+)))
       (/ Cv
	  (* (expt (* sigma 1e-10) 2)
	     +A+
	     (omega-22* (/ temperature epsilon/K))))))))

(defmethod lambda-0 ((coefficients hs-coeffs) temperature)
  "Thermal conductivity using the hard-sphere model

Kee et al, 12.57"
  (with-slots (species sigma m) coefficients
    (let ((Cv (Cv species temperature)))
    (* (/ 25d0
	  (* 32d0 (sqrt +pi+)))
       (sqrt (/ (* +k+ temperature)
		(* m +amu+)))
       (/ Cv
	  (* (expt (* sigma 1e-10) 2)
	     +A+))))))

(define-test lambda-0-lj
  ;; Test of LJ thermal conductivities vs tabulated values (CRC,
  ;; p.6-14).  I use a tolerance of 5%, because much lower than that
  ;; the tests will fail
  (let ((lisp-unit:*epsilon* 5e-2))
    (let ((coeffs (make-lj-coeffs :Ar)))
      (assert-number-equal 12.5e-3 (lambda-0 coeffs 200.0))
      (assert-number-equal 17.9e-3 (lambda-0 coeffs 300.0))
      (assert-number-equal 21.7e-3 (lambda-0 coeffs 380.0)))
    (let ((coeffs (make-lj-coeffs :N2)))
      (assert-number-equal 18.4e-3 (lambda-0 coeffs 200.0))
      (assert-number-equal 25.8e-3 (lambda-0 coeffs 300.0))
      (assert-number-equal 44.5e-3 (lambda-0 coeffs 600.0)))))