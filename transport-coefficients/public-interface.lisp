(in-package #:tc)

(defun mu (model species temperature &rest rest)
  "Viscosity for a pure gas

SPECIES - a symbol, or an object with collision parameters
TEMPERATURE - Temperature, in Kelvin
REST - other optional parameters"
  (mu% (default-mu-model species) species temperature &rest rest))

(defun mu-bgm (model species1 species2 concentration1 temperature
			   &rest rest)
  (:documentation "Binary gas mixture viscosity

MODEL - symbol, specifying the model to be used
SPECIES1,2 - symbols, objects with collision parameters
CONCENTRATION1 - relative concentration of species1.
TEMPERATURE - Temperature, in Kelvin
REST - other optional parameters"))

(defun lambda (model species temperature &rest rest)
  (:documentation "thermal conductivity for a pure gas

MODEL - symbol, specifying the model to be used
SPECIES - a symbol, or an object with collision parameters
TEMPERATURE - Temperature, in Kelvin
REST - other optional parameters"))

(defun lambda-bgm (model species1 species2 concentration1 temperature
			   &rest rest)
  (:documentation "Binary gas mixture thermal conductivity

MODEL - symbol, specifying the model to be used
SPECIES1,2 - symbols, objects with collision parameters
CONCENTRATION1 - relative concentration of species1.
TEMPERATURE - Temperature, in Kelvin
REST - other optional parameters"))

(defun D12 (model collision-params temperature &rest rest)
  (:documentation "Binary gas diffusion of a binary gas mixture

MODEL - symbol, specifying the model to be used
COLLISION-PARAMETERS - object with collision parameters
TEMPERATURE - Temperature, in Kelvin
REST - other optional parameters determined by MODEL"))

(defun alpha-T (model collision-params temperature &rest rest)
  (:documentation "Thermal diffusion coefficient of a binary gas mixture

MODEL - symbol, specifying the model to be used
COLLISION-PARAMETERS - object with collision parameters
TEMPERATURE - Temperature, in Kelvin
REST - other optional parameters determined by MODEL"))



