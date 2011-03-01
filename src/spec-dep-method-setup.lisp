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

(in-package :thermo)

;; setup database to store default calculation methods for
;; coefficients that depend on species


;;; Public interface

(define-test default-species-method
  (assert-equal 'foo
		(default-species-method (test-db) :S :Ar)))

(defun default-species-method (db coefficient species)
  "Return the default method to calculate `coefficient' for `species'"
  (cdr (assoc species (default-coefficients-methods db coefficient))))

  
(define-test set-default-species-method
  (let ((db (test-db)))
    (setf (default-species-method db :S :Ar) 'bar)
    (assert-equal 'bar
		  (default-species-method db :S :Ar))
    (setf (default-species-method db :S :ZZ) 'foobar)
    (assert-equal 'foobar
		  (default-species-method db :S :ZZ))))

(defun set-default-species-method (db coefficient species method)
  "Set the default method to calculate `coefficient' for `species' to
`method'"
  (if (default-species-method
	  db coefficient species)
      (setf
       (cdr (assoc species
		   (default-coefficients-methods db coefficient)))
       method)
      (push (cons species method)
	    (cdr (assoc coefficient db)))))

(defsetf default-species-method set-default-species-method)

;;; private and used for testing.

(defun test-db ()
  (list (list :S (cons :Ar  'foo) (cons :xx  'bar))
	(list :Cp  (cons :Ar  'janaf) (cons :xx 'shomate))))

(define-test default-coefficients
  (assert-equal '(:S :Cp)
		(default-coefficients (test-db))))

(defun default-coefficients (db)
  (mapcar #'first db))


(define-test default-coefficients-methods
  (assert-equal '((:Ar . foo) (:xx . bar))
		(default-coefficients-methods (test-db) :S)))

(defun default-coefficients-methods (db coefficient)
  (cdr (assoc coefficient db)))


