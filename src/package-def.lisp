;; Mirko Vukovic
;; Time-stamp: <2011-10-18 09:22:27 package-def.lisp>
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

(defpackage :thermo
  (:use :cl :grid :mv-grid :lisp-unit :alexandria :collision-integrals
	:molecular-potentials)
  (:shadow :lisp-unit :norm)
  (:shadow :alexandria :set-equal)
  (:shadow :gsll :gamma ;; overwritten by gamma coefficient in formulary.lisp
	   )
  (:shadow :molecular-potentials :species) ;; conflicts with local symbol
  (:import-from :anaphora :aif :it)
  (:import-from :my-utils #|:with-input-from-file|#
		:polyeval :read-vectors))

;; export is done in files in each module, either a setup file, or in
;; individual files




