;; Mirko Vukovic
;; Time-stamp: <2011-08-14 21:49:54 collision-integrals-package-def.lisp>
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


(defpackage :collision-integrals
  (:nicknames :omega-xx)
  (:export :omega-11 :omega-22)
  (:use :cl  :lisp-unit)
  (:documentation "Formulas for collision integrals used in gas transport coefficient calculations"))
;; export is done in files in each module, either a setup file, or in
;; individual files




