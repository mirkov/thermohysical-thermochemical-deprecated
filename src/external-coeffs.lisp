;; Mirko Vukovic
;; Time-stamp: <2011-08-28 11:24:13 external-coeffs.lisp>
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

(export '(species source data min-T max-T))

;; utilities for storing data from external sources in an object that
;; stores other useful information

(defclass external-species-data ()
  ((species :reader species
	    :initarg :species
	    :documentation "Species for which we store coefficients")
   (source :reader data-source
	   :initarg :data-source
	   :documentation "Coefficients source")
   (data :initarg :data
	 :reader data
	 :documentation "data")
   (min-T :reader min-T
	  :initarg :min-T
	  :documentation "Minimum table temperature")
   (max-T :reader max-T
	  :initarg :max-T
	  :documentation "Maximum table temperature"))
  (:documentation "Store `data' for a `species' that is obtained from an
  external `source' (typically a file)

Also store some other common data, such as minimum & maximum
temperatures over which the data is valid"))

#|(defmethod print-object ((species-data external-species-data) stream)
  (print-unreadable-object (species-data stream :type t :identity t)
    (format stream "Data for species ~a obtained from ~a"
	    (species species-data) (data-source species-data))))|#

(defmethod describe-object  ((species-data external-species-data) stream)
  (format stream "Data for species ~a obtained from ~a%"
	  (species species-data) (data-source species-data))
  (format stream "Temperature range: ~a -- ~a~%"
	  (min-T species-data) (max-T species-data)))
