(in-package #:tc)


(defgeneric-1+caller mu1 (species temperature &rest rest)
  (:fun-doc "Viscosity of a pure gas

SPECIES - a keyword for gas, or a lennard-jones 6/12 object
TEMPERATURE - Kelvin
REST - Other arguments for the default calculation method")
  (:default-method 'cck)
  (:documentation
   "Viscosity for a pure gas

MODEL - symbol, specifying the model to be used
SPECIES - a symbol, or an object with collision parameters
TEMPERATURE - Temperature, in Kelvin
REST - other optional parameters")
  (:sv-doc "Holds the symbol specifying the default method
   for calculating mu"))

#|(defgeneric mu% (model species temperature &rest rest)
  (:documentation "Viscosity for a pure gas

MODEL - symbol, specifying the model to be used
SPECIES - a symbol, or an object with collision parameters
TEMPERATURE - Temperature, in Kelvin
REST - other optional parameters"))|#

(defgeneric-1+caller mu2 (species1 species2 concentration1 temperature
			   &rest rest)
  (:fun-doc "Viscosity of a binary gas mixture

SPECIES1,2 - symbols, objects with collision parameters
CONCENTRATION1 - relative concentration of species1.
TEMPERATURE - Temperature, in Kelvin
REST - other optional parameters that may be required by the default 
       method specified in *MU2-DEFAULT-METHOD*")
  (:default-method 'mcc)
   (:documentation
   "Binary gas mixture viscosity

MODEL - symbol, specifying the model to be used
SPECIES1,2 - symbols, objects with collision parameters
CONCENTRATION1 - relative concentration of species1.
TEMPERATURE - Temperature, in Kelvin
REST - other optional parameters")
   (:sv-doc "Holds the symbol specifying the default metod for
   calculating mu2"))

#|(defgeneric mu-bgm% (model species1 species2 concentration1 temperature
			   &rest rest)
  (:documentation "Binary gas mixture viscosity

MODEL - symbol, specifying the model to be used
SPECIES1,2 - symbols, objects with collision parameters
CONCENTRATION1 - relative concentration of species1.
TEMPERATURE - Temperature, in Kelvin
REST - other optional parameters"))|#


(defgeneric-1+caller lambda1 (species temperature &rest rest)
  (:fun-doc "Thermal conductivity for a pure gas

SPECIES - a symbol, or an object with collision parameters
TEMPERATURE - Temperature, in Kelvin
REST - other optional parameters")
  (:default-method 'mcc)
  (:documentation
   "Thermal conductivity for a pure gas

SPECIES - a symbol, or an object with collision parameters
TEMPERATURE - Temperature, in Kelvin
REST - other optional parameters
MODEL - symbol, specifying the model to be used")
  (:sv-doc "Holds the symbol specifying the default metod for
   calculating mu2"))

#|(defgeneric lambda% (model species temperature &rest rest)
  (:documentation "thermal conductivity for a pure gas

MODEL - symbol, specifying the model to be used
SPECIES - a symbol, or an object with collision parameters
TEMPERATURE - Temperature, in Kelvin
REST - other optional parameters"))||#


(defgeneric-1+caller lambda2 (model species1 species2 concentration1 temperature
		       &rest rest)
  (:fun-doc "Binary gas mixture thermal conductivity

SPECIES1,2 - symbols, objects with collision parameters
CONCENTRATION1 - relative concentration of species1.
TEMPERATURE - Temperature, in Kelvin
REST - other optional parameters that may be required by the default method")
  (:default-method 'mcc)
  (:documentation "Binary gas mixture thermal conductivity

MODEL - symbol, specifying the model to be used
SPECIES1,2 - symbols, objects with collision parameters
CONCENTRATION1 - relative concentration of species1.
TEMPERATURE - Temperature, in Kelvin
REST - other optional parameters"))


#|(defgeneric lambda-bgm% (model species1 species2 concentration1 temperature
			   &rest rest)
  (:documentation "Binary gas mixture thermal conductivity

MODEL - symbol, specifying the model to be used
SPECIES1,2 - symbols, objects with collision parameters
CONCENTRATION1 - relative concentration of species1.
TEMPERATURE - Temperature, in Kelvin
REST - other optional parameters"))|#

(defgeneric-1+caller D12 (collision-params temperature pressure &rest rest)
  (:fun-doc "Binary gas diffusion of a binary gas mixture

COLLISION-PARAMETERS - object with collision parameters
TEMPERATURE - Temperature, in Kelvin
PRESSURE - Pressure in Pa
REST - other optional parameters determined by MODEL")
  (:default-method 'cck)
  (:documentation "Binary gas diffusion of a binary gas mixture

MODEL - symbol, specifying the model to be used
COLLISION-PARAMETERS - object with collision parameters
TEMPERATURE - Temperature, in Kelvin
PRESSURE - Pressure in Pa
REST - other optional parameters determined by MODEL"))

(defgeneric-1+caller alpha-T (potential1 potential2 X1 temperature &rest rest)
  (:documentation "Thermal diffusion coefficient of a binary gas mixture

MODEL - symbol, specifying the model to be used
POTENTIAL1,2 - Potential parameters for species 1,2
X1 - fraction of species 1
TEMPERATURE - Temperature, in Kelvin
REST - other optional parameters determined by MODEL")
  (:fun-doc "Thermal diffusion coefficient of a binary gas mixture

POTENTIAL1,2 - Potential parameters for species 1,2
X1 - fraction of species 1
TEMPERATURE - Temperature, in Kelvin
REST - other optional parameters determined by MODEL")
  (:default-method 'cck))



