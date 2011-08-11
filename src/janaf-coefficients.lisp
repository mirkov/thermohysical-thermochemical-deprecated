;; Mirko Vukovic
;; Time-stamp: <2011-08-11 14:11:29 janaf-coefficients.lisp>
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



;; reading of janaf tables, and interpolation facilities

(in-package :thermo)

(export '(make-janaf-coeffs column-headers raw-column))

(defparameter *janaf-table-rows* 64 "Number of rows in JANAF tables,
neglecting row for T=0")


(defun read-janaf-table% (stream)
  "read janaf table from stream, including non-numerical values,
returning the data in an unspecialized CL-array (element-types set to
`t')"
  (file-position stream 0)
  (read-line stream t)
  (read-line stream t)
  (read-line stream t)
  ;;(read-grid `(,*janaf-table-rows* 8) stream 't t nil :type 't)
  (read-grid `(,*janaf-table-rows* 8) stream 't :eof-value :eof))
    

(define-condition non-existant-janaf-table (error)
   ((species :reader species :initarg :species)
    (attempted-path :reader attempthed-path :initarg :attempted-path))
   (:report (lambda (condition stream)
              (format stream "janaf data table not found for species ~a" 
                      (species condition))))
   (:documentation "Error condition for non-existant JANAF file"))


(defun janaf-table-file-name (species)
  "Return pathname to file with Janaf table.  If non-existant, return
nil"
  (let ((path
	 (probe-file
	  (merge-pathnames (format nil "~(~a~).dat" species)
			   *janaf-directory*))))
    (aif path it
	 (error 'non-existant-janaf-table :species species
		:attempted-path path))))


(defgeneric read-janaf-table (specification)
  (:documentation "Read the JANAF table for `species' and return it in
grid's format with all numbers as double-float

As second value, return the file from which the table was read

Table can be specified by
 - species name
 - path to table")
  (:method ((path pathname))
    (with-input-from-file (stream path)
      (values
       (progn
	 (grid-coerce
	  (grid-substitute most-positive-double-float
			   'INFINITE
			   (let ((*array-type* 'array))
			     (read-janaf-table% stream))
			   :test #'equal)
	  'double-float))
       path)))
  (:method ((species symbol))
    (let ((file (janaf-table-file-name species)))
      (read-janaf-table file))))

(defclass janaf-coefficients (external-species-data)
  ((interpolation-data 
    :documentation "store interpolation data for all columns")
   (column-list :reader column-alist
		:initform '((T-K . 0)
			    (Cp . 1)
			    (S . 2)
			    (-G+H/T . 3)
			    (H-Hr . 4)
			    (delta_f-H . 5)
			    (delta_f-G . 6)
			    (log-K_f 7))
		:documentation "Alist between the column tables and
		the table-data columns"))
  (:documentation "Store JANAF coefficients and data to facilitate
  interpolation.  Note that the last three columns may be filled with zeros."))

(defmethod print-object ((janaf janaf-coefficients) stream)
  (print-unreadable-object (janaf stream :type t :identity t)
    (format stream "JANAF coefficients for ~a~%"
	    (species janaf))))


(defmethod initialize-instance :after ((coefficients janaf-coefficients)
				 &key)
  "Initialize the `janaf-coefficients' class

 - read the data
 - store the raw data, data source and species
 - store T-min, T-max
 - initialize interpolation tables

The JANAF table is read as a double-float foreign-array since it will
be passed to GSLL's spline interpolate for evaluation."
  ;; mv-grid-utils uses the native array type by default.  The native
  ;; type is OK on sbcl, but not on clisp.  Thus, I need to explicitly
  ;; declare the array type
  (let ((*array-type* 'foreign-array))
    (multiple-value-bind (table-data file)
	(read-janaf-table (slot-value coefficients 'species))
      (setf (slot-value coefficients 'data) table-data
	    (slot-value coefficients 'source) file)
      (let ((temp-column (column table-data 0)))
	(setf (slot-value coefficients 'min-T)
	      (gref temp-column 0)
	      (slot-value coefficients 'max-T)
	      (gref temp-column (1- *janaf-table-rows*))
	      (slot-value coefficients 'interpolation-data)
	      (make-array 8))
	(dotimes (i 8)
	  (setf (aref (slot-value coefficients 'interpolation-data) i)
		(gsll:make-spline gsll:+cubic-spline-interpolation+
				  temp-column (column table-data i))))))))



(defun make-janaf-coeffs (species)
  "Return an object of janaf coefficients for `species' and perform
all initializations on it"
  (make-instance 'janaf-coefficients :species species))
  
  

(defmethod column-headers ((coeffs janaf-coefficients))
  (format t "header~t20number~%")
  (mapcar #'(lambda (entry)
	      (format t "~a~t20~a~%" (car entry)
		      (cdr entry)))
	  (slot-value coeffs 'column-list)))


(defgeneric raw-column (coeffs column)
  (:documentation "Return the raw column data

The column data is specified either by index or by the column header")
  (:method  ((coeffs janaf-coefficients) (index number))
    (column (data coeffs) index))
  (:method ((coeffs janaf-coefficients) (column-specifier symbol))
    (let ((column (cdr (assoc column-specifier (slot-value coeffs 'column-list)))))
      (if column
	  (raw-column coeffs column)
	  (error "Incorrect column specifier ~a,
valid ones are ~{~a ~}" column-specifier
		 (mapcar #'car (slot-value coeffs 'column-list)))))))

(defgeneric janaf-coeff (coeffs column temperature)
  (:documentation "Return the value of the coefficient from column at temperature")
  (:method ((coeffs janaf-coefficients) (column number)
	    (temperature #+sbcl double-float #+clisp number))
    (gsll:evaluate (aref (slot-value coeffs 'interpolation-data) column)
	      temperature))
  (:method ((coeffs janaf-coefficients) (column-specifier symbol) (temperature number))
    (let ((column (cdr (assoc column-specifier (slot-value coeffs 'column-list)))))
      (if column
	  (janaf-coeff coeffs column (coerce temperature 'double-float))
	  (error "Incorrect column specifier ~a,
valid ones are ~{~a ~}" column-specifier
               (mapcar #'car (slot-value coeffs 'column-list)))))))

(defmethod Cp ((coeff-table janaf-coefficients) temperature)
  (janaf-coeff coeff-table 'Cp temperature))

(defmethod S ((coeff-table janaf-coefficients) temperature)
  (janaf-coeff coeff-table 'S temperature))

(defmethod H-rel ((coeff-table janaf-coefficients) temperature)
  (janaf-coeff coeff-table 'H-Hr temperature))

