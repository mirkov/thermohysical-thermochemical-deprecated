;; Mirko Vukovic
;; Time-stamp: <2012-05-15 13:25:29 transport-coeffs-external-interface.lisp>
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

(export '(mu-0 lambda-0 D-ij *transp-coeff-defaults*))



(defun make-transp-coeff-defaults-db ()
  (list (list :mu-0)
	(list :lambda-0)
	(list :D-ij)
	(list :alpha-T)))

(defparameter *transp-coeff-defaults* 
  (make-transp-coeff-defaults-db)
  "Specifies default calculation methods for mu-0, lambda-0, D-0

The db structure is as follows
 ((:mu-0 (:Ar . foo) (:He . bar))
  (:lambda-0 (:air . foobar))
  (:D-ij ))


Objects are used to store the calculation data.  So, for Ar viscosity,
we do

 (make-lj-coeffs :Ar) to create an object with Lennard-Jones coefficients

To use those coefficients to calculate Ar viscosity by default,
execute the following:

 (setf default-species-method *transp-coeff-defaults* :Mu-0 :Ar *)

")


(defgeneric mu-0 (species temperature)
  (:documentation "Return value of mu-0 at `temperature' for species

Species properties can be specified in several ways:
 - symbol
 - coefficients from the kinetic model
 - other

In case symbol is given, the method chosen is determined by the
contents of *transp-coeff-defaults*")
  (:method ((species symbol) temperature)
    (let ((method (default-species-method *transp-coeff-defaults* :mu-0 species)))
    (if method (funcall #'mu-0 method temperature)
	(error "Default method for calculating mu-0 undefined for ~a" species)))))

(defgeneric lambda-0 (species temperature)
  (:documentation "Return value of lambda-0 at `temperature' for species

Species properties can be specified in several ways:
 - symbol
 - janaf coefficients in form of a janaf-coefficients object
 - shomate-fit coefficients using the shomate-fit object

In case symbol is given, the method chosen is determined by the
contents of *transp-coeff-defaults*")
    (:method ((species symbol) temperature)
    (let ((method (default-species-method *transp-coeff-defaults* :lambda-0 species)))
    (if method (funcall #'lambda-0 method temperature)
	(error "Default method for calculating lambda-0 undefined for ~a" species)))))

(defgeneric D-0 (species temperature pressure)
  (:documentation "Return value of D-0 at `temperature' for species

Species properties can be specified in several ways:
 - symbol
 - janaf coefficients in form of a janaf-coefficients object
 - shomate-fit coefficients using the shomate-fit object

In case symbol is given, the method chosen is determined by the
contents of *transp-coeff-defaults*")
    (:method ((species symbol) temperature pressure)
    (let ((method (default-species-method *transp-coeff-defaults* :D-0 species)))
    (if method (funcall #'D-0 method temperature pressure)
	(error "Default method for calculating D-0 undefined for ~a" species)))))





