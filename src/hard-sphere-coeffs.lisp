;; Mirko Vukovic
;; Time-stamp: <2011-08-15 10:26:34 hard-sphere-coeffs.lisp>
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


;; Hard-Sphere coefficients for calculating the Omega-xx integrals
;; Unlike the Lennard-Jones coefficients, these is no tabulated data.
;; An object storing the coefficients as objects is created using
;; (make-hs-coeffs species mass sigma &optional doc).  Such an object
;; can be used as an argument to the transport coefficient
;; calculations (mu-0, etc)

(export '(make-HS-coeffs hs-coeffs mass sigma doc))

(defclass hs-coeffs ()
  ((species :initarg :species
	    :accessor species)
   (m :initarg :mass
      :accessor mass
      :documentation "Species mass in AMU")
   (sigma :initarg :sigma
	  :accessor sigma
	  :documentation "Species cross-section in Angstrom")
   (doc :initarg :doc
	:accessor doc
	:documentation "Coefficient source and comments"))
  (:documentation "Lennard-Jones coefficients for a species

Units are NOT SI:
- mass in AMU
- sigma in Angstrom"))

(defmethod print-object ((self hs-coeffs) stream)
  (print-unreadable-object (self stream :type t :identity t)))

(defmethod describe-object ((self hs-coeffs) stream)
  (format stream "Hard sphere coefficients for ~a"
	    (species self)))

(defun make-hs-coeffs (species mass-amu sigma-angstrom
		       &optional documentation)
  (make-instance 'hs-coeffs
		 :species species
		 :mass mass-amu
		 :sigma sigma-angstrom
		 :doc (or documentation "")))