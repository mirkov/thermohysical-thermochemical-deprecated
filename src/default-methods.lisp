;; Mirko Vukovic
;; Time-stamp: <2011-10-20 13:36:28 default-methods.lisp>
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

;; now that all the calculation methods and objects are set-up, 
;; declare defaults

;; thermodynamic quantities from janaf tables
(mapcar
 #'(lambda (species)
     (setf (default-species-method *thermo-defaults* :cp species)
	   (make-janaf-coeffs species))
     (setf (default-species-method *thermo-defaults* :s species)
	   (make-janaf-coeffs species))
     (setf (default-species-method *thermo-defaults* :h-rel species)
	   (make-janaf-coeffs species)))
 '(:h2 :ar :he :n2 :o2))

(setf (default-species-method *thermo-defaults* :Cv :He)
      (make-constant-coeff :He 12.5 "CRC, 79th Ed., p. 6-17"
			   5.0 1500.0)
      (default-species-method *thermo-defaults* :Cv :Ar)
      (make-constant-coeff :Ar 12.5 "CRC, 79th Ed., p. 6-17"
			   160.0 380.0))

;; viscosities using lennard jones potentials
(mapcar
 #'(lambda (species)
     (setf (default-species-method *transp-coeff-defaults* :mu-0 species)
      (make-species-lennard-jones-6/12-potential species)))
 '(:ar :n2 :o2 :air :he :h2 :co2))

;; thermal conductivity using the lennard-jones potentials and kinetic
;; theory formulation
(mapcar
 #'(lambda (species)
     (setf (default-species-method *transp-coeff-defaults* :lambda-0 species)
      (make-species-lennard-jones-6/12-potential species)))
 '(:he))

;; thermal conductivity from Lemmon & Jacobsen based on review of
;; experimental data
(mapcar
 #'(lambda (species)
     (setf (default-species-method *transp-coeff-defaults* :lambda-0 species)
	   (cdr (assoc species *lj04-lambda-coeffs*))))
 '(:ar :n2 :o2 :air))

;; hydrogen thermal conductivity from Assael'86
(setf (default-species-method *transp-coeff-defaults* :lambda-0 :H2)
     (make-assael-table-2))

