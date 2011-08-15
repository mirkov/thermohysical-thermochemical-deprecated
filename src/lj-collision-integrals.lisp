;; Mirko Vukovic
;; Time-stamp: <2011-08-14 22:04:09 collision-integrals.lisp>
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

;; Calculations of collision integrals
(export '(
	  default-omega-calc-method
	  make-lj04-omega-22-coeffs
	  make-kee-omega-11-coeffs
	  make-kee-omega-22-coeffs))

;; We offer the formulae from Kee and from Lemmon&Jacobsen



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

(defmethod print-object ((self omega-fit-coeffs) stream)
  (print-unreadable-object (self stream :type t :identity t)))

(defmethod describe-object ((self omega-fit-coeffs) stream)
  (format stream (slot-value self 'doc)))


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
   "A container of coefficients for calculating the Omega-11 collision integral using the double exponent formula of Kee et al (12.6) and table 12.1"
   (slot-value container 'coeffs-vector)
   #(1.0548 0.15504 0.55909 2.1705)))

(defun make-kee-omega-11-coeffs ()
  (make-instance 'Kee-omega-11-coeffs))

(defclass Kee-omega-22-coeffs (omega-fit-coeffs)
  ())
(defmethod initialize-instance :after ((container  Kee-omega-22-coeffs) &key)
  (setf 
   (slot-value container 'doc)
   "A container of coefficients for calculating the Omega-22 collision integral using the double exponent formula of Kee et al, (12.6) and table 12.1"
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
	"A container of coefficients for calculating the Omega-22 collision integral using the log/polynomial expansion of Lemon & Jacobsen, 2004

Agreement with the two exponent expansion breaks down for T* < 0.2 and T* > 4"))


(defun make-lj04-omega-22-coeffs ()
  (make-instance 'lj04-omega-22-coeffs))


