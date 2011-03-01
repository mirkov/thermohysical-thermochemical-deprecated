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

;; implementation of the Helium thermal conductivity to be evaluated
;; by using the `lambda-0' method on the class defined here:

(defclass kt-thermal-conductivity-coeffs (lj-coeffs)
  ((Cv :initarg :Cv
       :accessor Cv-coeff
       :documentation "heat capacity at constant volume in J/Mol"))
  (:documentation "Coefficients for thermal conductivity according to
  Kee, (12.101)"))

(defparameter 

