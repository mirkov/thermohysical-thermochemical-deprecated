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

(in-package :thermo-user)

#|

The package thermo-user is intended for the user to use the facilities
provided by the thermophysics&chemistry package.

The package :thermo is intended for the development of the package.


* General notes

** Default calculation methods

|#


#|

* Thermodynamic coefficients

The thermodynamic coefficients, such as S, Cp, H are provided by two
types of utilities, based on NIST's webbook: formulas using the
Shomate coefficients, or data interpolation tables.

One can use either the defaults or specify explicity which method.  This is illustrated below for the calculation of H2 specific heat.
|#

;;> Using default method
(Cp :H2 300) ;;->28.849000930786133d0
;;> Using the shomate fits
(Cp (make-shomate-coeffs :H2) 300) ;;->28.849483
;;> Using interpolation of the Janaf tables
(Cp (make-janaf-coeffs :H2) 300);;->28.849000930786133d0

#|

The shomate coefficients are stored in the thermo-data directory, in
files such as H2.shomate using a lisp format.

Janaf coefficients are stored in the janaf directory, in files such as
H2.dat using a txt format.
|#


#|

** Temperature ranges

For most coefficients, we have included the valid temperature range,
and if the requested temperature falls outside of it, a condition is
thrown

|#


(Cp (make-shomate-coeffs :H2) 100);;-> error

#|

* Plotting

The thermo-user package is build on top mv-grid-utils and mv-gnuplot.
This allows for simple plotting using the gcmap macro.

After creating a plot window,
|#

(new-window)

#| We can create a plot comparing the specific heat
calculated by the two methods: |#

(let ((temp (lseq 300 1000))
      (sh (make-shomate-coeffs :H2))
      (j (make-janaf-coeffs :H2)))
  (plot-xys temp
	    (list (list (gcmap (Cp sh @!temp) temp) :title "Shomate")
		  (list (gcmap (Cp j @!temp) temp) :title "Interpolation")))) ;;->plot in window

#|

This plot demonstrates the use of currying as applied to grids via
 `gcmap'.  It also shows that currently, the arguments of the curried
 function have to be atoms and not forms.

To save this plot to a file, we use the following facility from mv-gnuplot
|#

(with-ps-output ("shomate-vs-interpolation.ps" #P"./")
  (let ((temp (lseq 300 1000))
	(sh (make-shomate-coeffs :H2))
	(j (make-janaf-coeffs :H2)))
    (plot-xys temp
	      (list (list (gcmap (Cp sh @!temp) temp) :title "Shomate")
		    (list (gcmap (Cp j @!temp) temp) :title "Interpolation")))));;->output to file

#|

This will save the plot into a post-script file in the current working directory

There are currently no options for other output types.
|#

#| 

Similar plots can be made for S, and H-rel

|#

#|

** Collision parameters

The collision parameter module offers the following functionality:

 - Loading the Lennard-Jones collision parameters from
   lennard-jones-coeffs.lisp
 - Calculation of the Omega-11 and Omega-22 collision integrals

The Lennard-Jones parameters can be obtained in two ways:
|#
(make-lj-coeffs :H2);;->#<THERMO::LJ-COEFFS Lennard-Jones coefficients for H2 {BAE0059}>
(lennard-jones-coeffs :H2) ;;->((:M 2.016) (:SIGMA 2.915) (:EPSILON/K 38.0) (:TC 33.3))

#|

The collision integrals are calculated using the equations by Kee et
al by the following calls
|#
(omega-11 1.0);;->1.4361933
(omega-22 1.0);;->1.6007648

#|

The infrastructure for the collision integrals allows multiple methods
to be used to calculate the coefficients.

The individual methods are defined by classes:
 - Kee-omega-11-coeffs
 - Kee omega-22-coeffs
 - LJ04-omega-22-coeffs from the Lemmon&Jacobsen paper

So, instead of the defaults, we can choose the calculation method by
calling slightly different functions: |#

(omega-22% (make-kee-omega-22-coeffs) 1.0);;->1.6007648
(omega-22% (make-lj04-omega-22-coeffs) 1.0);;->1.5387956

#|

The Kee approximation is preferred to that of Lemmon & Jacobsen as these have a more restricted validity region:

|#

(let ((temp (gseq 0.1 100))
      (k (make-kee-omega-22-coeffs))
      (lj04 (make-lj04-omega-22-coeffs)))
  (set-to ((logscale :xy))
    (plot-xys temp
	      (list (list (gcmap (omega-22%  k @!temp) temp) :title "Kee")
		    (list (gcmap (omega-22% lj04 @!temp) temp) :title "LJ04")))));;->plot

#|

* Transport coefficients

 - viscosity
 - thermal conductivity
 - diffusion

Viscosity is calculated using the kinetic theory result and the Lennard-Jones parameters as follows:
|#
(mu-0 :H2 350) ;;->1.0010757183405921d-5

#| The story is more complicated for thermal conductivity, as it
depends on the internal degrees of freedom, encapsulated in Cv.

So, in practice, there will be many methods for different gases.

From the references found so far I have assembled two sets of methods:
 - Lemmon & Jacobsen for N2, Ar, O2 and Air
 - Assael et al for H2

These are captured in their objects and set as defaults, so that the following works nicely:
|#
(lambda-0 :H2 350);;->0.21095513871295823d0
(lambda-0 :O2 350);;->0.029757758335946592d0

#| As with all the other coefficients, these provide temperature checks.

One has to be careful with the thermal conductivity calculations, as
these formulations use the viscosity of the gas to calculate the
thermal conductivity.

|#

#|
Not implemente yet are 
 - hard-sphere models (of academic interest)
 - Sutherland model
 - transport coefficients for mixtures.

|#



