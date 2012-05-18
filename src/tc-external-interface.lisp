;; Mirko Vukovic
;; Time-stamp: <2011-08-25 22:02:30 tc-external-interface.lisp>
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

(export '(S Cp H-rel *thermo-defaults*))

;; See documentation in spec-dep-method-setup on how to view, set,
;; change defaults
(defun make-thermo-defaults-db ()
  "Initialize the defaults list"
  (list (list :S)
	(list :Cp)
	(list :H-rel)
	(list :Cv)))

(defparameter *thermo-defaults*
  (make-thermo-defaults-db)
  "Specifies default calculation methods for Cp, S, ...")

(defgeneric Cp (species temperature)
  (:documentation "Return value of Cp at `temperature' for species

Species properties can be specified in several ways:
 - symbol
 - janaf coefficients in form of a janaf-coefficients object
 - shomate-fit coefficients using the shomate-fit object
 - constant -- apropritate for ideal gases

In case symbol is given, the method chosen is determined by the
contents of *thermo-defaults*.")
  (:method ((species symbol) temperature)
    (let ((cp-method (default-species-method *thermo-defaults* :Cp species)))
      (if cp-method
	  (funcall #'Cp cp-method temperature)
	  (error "Default method for calculating Cp undefined for ~a" species)))))

(defgeneric Cv (species temperature)
  (:documentation "Return value of Cv at `temperature' for species

Species properties can be specified in several ways:
 - symbol
 - janaf coefficients in form of a janaf-coefficients object
 - shomate-fit coefficients using the shomate-fit object
 - constant -- apropritate for ideal gases

In case symbol is given, the method chosen is determined by the
contents of *thermo-defaults*.  If no default method is found, Cp is
calculated using the law for ideal gases: Cp = Cv + Nk (Kee et al,
8.126)")
  (:method ((species symbol) temperature)
    (let ((method (default-species-method *thermo-defaults* :Cv species)))
    (if method (funcall #'Cv method temperature)
	  (let ((cp-method (default-species-method *thermo-defaults* :Cp species)))
	    (if cp-method
		(let ((cp (funcall #'Cp cp-method temperature)))
		  (- cp (* +A+ +k+)))
		(error "Default method for calculating Cv undefined for ~a" species)))))))

(define-test Cv&Cp
  ;; compare against CRC values (79th ed) using default methods.
  ;; This is not a
  ;; very exhaustive test
  (let ((lisp-unit:*epsilon* 1e-2))
    ;; N2: (CRC 79th Ed., p.6-14)
    (assert-number-equal 20.8 (Cv :N2 200))
    (assert-number-equal 21.8 (Cv :N2 600))
    (assert-number-equal 26.4 (Cv :N2 1500))
    (assert-number-equal 29.2 (Cp :N2 200))
    (assert-number-equal 30.1 (Cp :N2 600))
    (assert-number-equal 34.7 (Cp :N2 1500))
    ;; Ar: (CRC 79th Ed., p.6-18)
    (assert-number-equal 12.5 (Cv :Ar 200))
    (assert-number-equal 12.5 (Cv :Ar 300))
    (assert-number-equal 12.5 (Cv :Ar 380))
    (assert-number-equal 20.9 (Cp :Ar 200))
    (assert-number-equal 20.8 (Cp :Ar 300))
    (assert-number-equal 20.8 (Cp :Ar 380))
    ;; O2: (CRC 79th Ed., p. 6-15)
    (assert-number-equal 20.8 (Cv :O2 200))
    (assert-number-equal 21.1 (Cv :O2 300))
    (assert-number-equal 21.6 (Cv :O2 380))
    (assert-number-equal 29.3 (Cp :O2 200))
    (assert-number-equal 29.4 (Cp :O2 300))
    (assert-number-equal 30.0 (Cp :O2 380))))

  
  



(defgeneric S (species temperature)
  (:documentation "Return value of S at `temperature' for species

Species properties can be specified in several ways:
 - symbol
 - janaf coefficients in form of a janaf-coefficients object
 - shomate-fit coefficients using the shomate-fit object

In case symbol is given, the method chosen is determined by the
contents of *thermo-defaults*")
    (:method ((species symbol) temperature)
    (let ((method (default-species-method *thermo-defaults* :S species)))
    (if method (funcall #'S method temperature)
	(error "Default method for calculating S undefined for ~a" species)))))

(defgeneric H-rel (species temperature)
  (:documentation "Return value of H-rel at `temperature' for species

Species properties can be specified in several ways:
 - symbol
 - janaf coefficients in form of a janaf-coefficients object
 - shomate-fit coefficients using the shomate-fit object

In case symbol is given, the method chosen is determined by the
contents of *thermo-defaults*")
    (:method ((species symbol) temperature)
    (let ((method (default-species-method *thermo-defaults* :H-rel species)))
    (if method (funcall #'H-rel method temperature)
	(error "Default method for calculating H-rel undefined for ~a" species)))))





