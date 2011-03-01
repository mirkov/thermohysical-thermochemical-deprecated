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



;; Object to store a constant coefficient.

(in-package :thermo)

(export '(make-constant-coeff))

(defclass constant-coefficient (external-species-data)
 ()
  (:documentation "Store a constant"))


#|(defmethod print-object ((obj constant-coefficient) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "Table of JANAF coefficients for ~a~%"
	    (species janaf))))
|#

(defmethod describe-object ((obj constant-coefficient) stream)
  (with-slots (species data min-T max-T) obj
    (format stream "Constant coefficient for ~a
It's value is ~a
It is valid in the temperature range ~a - ~a ~%"
	    species data min-T max-T)))

(defmethod initialize-instance :after ((obj constant-coefficient)
				 &key)
  (with-slots (data) obj
    (unless data
      (error "Must provide :data initarg to make-instance"))))



(defun make-constant-coeff (species value source min-T max-T)
  "Create a constant coefficients for `species'"
  (make-instance 'constant-coefficient :species species :data value
		 :data-source source :min-T min-T :max-T max-T))
  
  
