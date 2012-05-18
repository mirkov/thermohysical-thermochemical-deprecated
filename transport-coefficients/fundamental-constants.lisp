(in-package #:tc)

(defconstant +R+ physics-constants:+gas-constant-sp+
  "Universal gas constant, 8.3147135 J/K mol")
(defconstant +A+ physics-constants:+avogadros-number-sp+
  "Avogadro's number, 6.0221e23/mol")
(defconstant +k+ physics-constants:+boltzmann-constant-sp+
  "Boltzmann's constant, 1.3807e-23 J/K")
(defconstant +amu+ physics-constants:+atomic-mass-unit-sp+
  "Atomic mass unit, 1.6605e-27 kg")
(defconstant +pi+ (coerce pi 'single-float)
  "pi, single precision to prevent contagion with long numbers")
(defconstant +NA+ physics-constants:+avogadros-number-sp+
  "Avogadro's number")

(defconstant +NA20+ (/ +NA+ 1e20)
  "N_A/1e20")

