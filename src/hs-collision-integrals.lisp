;; Mirko Vukovic
;; Time-stamp: <2011-08-23 20:48:41 hs-collision-integrals.lisp>
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

;; Calculation of hard-sphere collision integrals.  Unlike the
;; Lennard-Jones collision integral calculation, which are scaled to
;; the value of the hard-sphere one, this one requires the the mass,
;; temperature and cross-section.
;;
;; Since this is a molecular interaction, I use SI units (kg, meters,
;; etc)

(export '(omega11-hs omega12-hs omega13-hs omega22-hs))

(defun Omega11-hs (Temp m-reduced sigma-coll)
  "Hard-sphere collision integral Omega-11 as function of
`temp' (Kelvin), `m-reduced' (kg), `sigma-coll' (meter')

For collisions between species a and b the arguments are:

m-reduced <- (/ (* m-a m-b)
                (+ m-a m-b))
sigma-coll <- (+ sigma-a sigma-b)

For a signle species, this translates into:
m-reduced <- (/ m 2)
sigma-coll <- (* 2 sigma)

In that case, the formula below reduces to something similar to Kee et
al, (12.95):
  (* (sqrt (/ (* +pi+ +kb+ Temp)
	      (* 2.0 m-reduced)))
     (^2 sigma))
The Kee formula is evidently a incomplete simplification with syntax errors"
  (* 0.25
     (sqrt (/ (* +pi+ +kb+ Temp)
	      (* 2.0 m-reduced)))
     (^2 sigma-coll)))

(define-test Omega11-hs
  (let ((temp (/ (* +pi+ +kb+))))
    (assert-number-equal 0.25  (Omega11-hs temp 0.5 1.0) "Basic")
    (assert-number-equal 0.125  (Omega11-hs temp 2.0 1.0) "Mass test")
    (assert-number-equal 1.0  (Omega11-hs temp 0.5 2.0) "Sigma test")
    (assert-number-equal 0.5 (Omega11-hs (* 4 temp) 0.5 1.0) "Temperature test")))

(defun Omega12-hs (Temp m sigma)
  "Hard-sphere collision integral Omega-12 as function of
`temp' (Kelvin), `m' (kg), `sigma' (meter')"
  (* 3.0 (Omega11-hs Temp m sigma)))

(defun Omega13-hs (Temp m sigma)
  "Hard-sphere collision integral Omega-13 as function of
`temp' (Kelvin), `m' (kg), `sigma' (meter')"
  (* 12.0 (Omega11-hs Temp m sigma)))

(defun  Omega22-hs (Temp m sigma)
  "Hard-sphere collision integral Omega-22 as function of
`temp' (Kelvin), `m' (kg), `sigma' (meter')"
  (* 2.0 (Omega11-hs Temp m sigma)))

(define-test Omegaxy-hs
  (let ((temp (/ (* +pi+ +kb+)))
	(m 0.5)
	(sigma 1.0))
    (assert-number-equal (* 3.0 (omega11-hs Temp m sigma))
			 (Omega12-hs  Temp m sigma))
    (assert-number-equal (* 12.0 (omega11-hs Temp m sigma))
			 (Omega13-hs  Temp m sigma))
    (assert-number-equal (* 2.0 (omega11-hs Temp m sigma))
			 (Omega22-hs  Temp m sigma))))

(defun omegaLS-hs* (l s)
  "Scale factor for the general hard-sphere collision integral

Indices limited below 6, for one reason to limit overflows in the
factorial calculation of (1+s)!

Kee et al, (12.95)"
  (assert (< s 6) () "Maximum value of s is 5")
  (assert (< l 6) () "Maximum value of l is 5")
  (let ((s+1! (loop for i from 1 to (1+ s)
		 with fact = 1
		 do (setf fact (* fact i))
		 finally (return fact))))
    (* (/ s+1!
	  2)
       (- 1 (* 1/2 (/ (+ 1 (expt -1 l))
		      (+ 1 l)))))))

(define-test omegaLS-hs*
  (assert-equal 1 (omegaLS-hs* 1 1))
  (assert-equal 3 (omegaLS-hs* 1 2))
  (assert-equal 12 (omegaLS-hs* 1 3))
  (assert-equal 2 (omegaLS-hs* 2 2)))
#|
(define-test Omega11-a
  "Simplest test where the argument of the root should be unity"
  (let ((temp (/ (* +pi+ +kb+)))
	(m-1 1.0)
	(m-2 1.0)
	(sigma-1 0.5)
	(sigma-2 0.5))
    (bind-m*-alpha-beta
      (bind-omega11-alpha-beta
	(assert-number-equal 0.25  Omega11-1-1)
	(assert-number-equal 0.25  Omega11-1-2)
	(assert-number-equal 0.25  Omega11-2-1)
	(assert-number-equal 0.25  Omega11-2-2)))))

(define-test Omega11-b
  "Test of temperature dependence
Other arguments result in unity and the root should equal to 4, canceling 0.25"
  (let ((temp (/ 16 (* +pi+ +kb+)))
	(m-1 1.0)
	(m-2 1.0)
	(sigma-1 0.5)
	(sigma-2 0.5))
    (bind-m*-alpha-beta
      (bind-omega11-alpha-beta
	(assert-number-equal 1.0  Omega11-1-1)
	(assert-number-equal 1.0  Omega11-1-2)
	(assert-number-equal 1.0  Omega11-2-1)
	(assert-number-equal 1.0  Omega11-2-2)))))
  
(define-test Omega11-c
  "Test of diameter dependence"
  (let ((temp (/ (* +pi+ +kb+)))
	(m-1 1.0)
	(m-2 1.0)
	(sigma-1 2.0)
	(sigma-2 2.0))
    (bind-m*-alpha-beta
      (bind-omega11-alpha-beta
	(assert-number-equal 4.0  Omega11-1-1)
	(assert-number-equal 4.0  Omega11-1-2)
	(assert-number-equal 4.0  Omega11-2-1)
	(assert-number-equal 4.0  Omega11-2-2)))))

(define-test Omega11-d
  "Test of mass dependence"
  (let ((temp (/ (* +pi+ +kb+)))
	(m-1 4.0)
	(m-2 4.0)
	(sigma-1 0.5)
	(sigma-2 0.5))
    (bind-m*-alpha-beta
      (bind-omega11-alpha-beta
	(assert-number-equal 0.125  Omega11-1-1)
	(assert-number-equal 0.125  Omega11-1-2)
	(assert-number-equal 0.125  Omega11-2-1)
	(assert-number-equal 0.125  Omega11-2-2)))))



(define-test Omega12&13&22
  (let ((temp (/ (* +pi+ +kb+)))
	(m-1 4.0)
	(m-2 4.0)
	(sigma-1 0.5)
	(sigma-2 0.5))
    (bind-m*-alpha-beta
      (bind-omega11-alpha-beta
	(bind-omega12-alpha-beta
	  (assert-number-equal (* 3.0 Omega11-1-1) Omega12-1-1)
	  (assert-number-equal (* 3.0 Omega11-1-2) Omega12-1-2)
	  (assert-number-equal (* 3.0 Omega11-2-1) Omega12-2-1)
	  (assert-number-equal (* 3.0 Omega11-2-2) Omega12-2-2))
	(bind-omega13-alpha-beta
	  (assert-number-equal (* 12.0 Omega11-1-1) Omega13-1-1)
	  (assert-number-equal (* 12.0 Omega11-1-2) Omega13-1-2)
	  (assert-number-equal (* 12.0 Omega11-2-1) Omega13-2-1)
	  (assert-number-equal (* 12.0 Omega11-2-2) Omega13-2-2))
	(bind-omega22-alpha-beta
	  (assert-number-equal (* 2.0 Omega11-1-1) Omega22-1-1)
	  (assert-number-equal (* 2.0 Omega11-1-2) Omega22-1-2)
	  (assert-number-equal (* 2.0 Omega11-2-1) Omega22-2-1)
	  (assert-number-equal (* 2.0 Omega11-2-2) Omega22-2-2))))))|#

