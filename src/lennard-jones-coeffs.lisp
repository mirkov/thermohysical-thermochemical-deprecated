;; Mirko Vukovic
;; Time-stamp: <2011-08-16 09:38:00 lennard-jones-coeffs.lisp>
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

;; Lenard-Jones coefficients for calculating the Omega-xx integrals
;; are read from a file.  They can be created as objects using
;; (make-lj-coeffs species)

(export '(make-LJ-coeffs *lennard-jones-coeffs* lennard-jones-coeffs
	  lj-coeffs species m mass sigma epsilon/k doc))

;; Kludge: I copied code from thermo//environment-setup.lisp for the data reading.
(defparameter *data-directory*
  (merge-pathnames
   #P"my-software-add-ons/my-lisp/modeling/thermophysical+thermochemical/thermo-data/"
   #+WTEHCFMXYP1 #p"/home/977315/"
   #+CYSSHD1 #P"/home/mv/")
  "Path to the data directory (not including JANAF tables)")

(defun read-data-file (file &optional (directory *data-directory*))
  "Return contents of `file' in `directory'

Signal error if file not found"
  (with-input-from-file (stream
			 (merge-pathnames file
					  directory)
			 :if-does-not-exist :error)
    (read stream)))
(defun read-LJ-coeffs (&optional (file "lennard-jones-coeffs.dat"))
  "Read Shomate coefficients for the species"
  (read-data-file file))


(defparameter *lennard-jones-coeffs* (read-LJ-coeffs)
  "Lennard Jones coefficients")

(defun lennard-jones-coeffs (species
			     &optional (db *lennard-jones-coeffs*))
  "return Lennard-Jones coefficients for `species'"
  (cdr (assoc species db)))


(defclass lj-coeffs ()
  ((species :initarg :species
	    :accessor species)
   (m :initarg :mass
      :accessor mass
      :documentation "Species mass in AMU")
   (sigma :initarg :sigma
	  :accessor sigma
	  :documentation "Species cross-section in Angstrom")
   (epsilon/K :initarg :epsilon/K
	      :accessor epsilon/K
	      :documentation "Potential-well depth, dimensionless")
   (doc :initarg :doc
	:accessor doc
	:documentation "Document data origin and other comments"))
  (:documentation "Lennard-Jones coefficients for a species

Units are NOT SI:
- mass in AMU
- sigma in Angstrom"))

(defmethod print-object ((self lj-coeffs) stream)
  (print-unreadable-object (self stream :type t :identity t)))

(defmethod describe-object ((self lj-coeffs) stream)
  (format stream "Lennard-Jones coefficients for ~a"
	    (species self)))

(defun make-lj-coeffs (species)
  (let ((coeffs (lennard-jones-coeffs species)))
    (make-instance 'lj-coeffs
		   :species species
		   :mass (cadr (assoc :m coeffs))
		   :sigma (cadr (assoc :sigma coeffs))
		   :epsilon/k (cadr (assoc :epsilon/k coeffs))
		   :doc "Bird, Stewart & Lightfoot")))