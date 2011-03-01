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

;; tranposrt coefficients formulae based on kinetic theory, omega,
;; epsilon and the collision integrals

(defmethod mu-0 ((coefficients lj-coeffs) temperature)
  (with-slots (m sigma epsilon/K) coefficients
    (* (/ 5 16)
       (/ (sqrt (* pi m physics-constants:+proton-mass-sp+
		   physics-constants:+boltzmann-constant-sp+
		   temperature))
	  (* pi (expt (* sigma 1e-10)
		      2)
	     (omega-22 (/ temperature epsilon/K)))))))



(defmethod lambda-0 ((coefficients lj-coeffs) temperature)
  "Kee et al, 12.101"
  (with-slots (species sigma epsilon/K m Cv) coefficients
    (let ((Cv (Cv species temperature)))
    (* (/ 25d0
	  (* 32d0 (sqrt pi)))
       (sqrt (/ (* +R+ temperature)
		(* m 1e-3)))
       (/ Cv
	  (* (expt (* sigma 1e-10) 2)
	     +A+
	     (omega-22 (/ temperature epsilon/K))))))))