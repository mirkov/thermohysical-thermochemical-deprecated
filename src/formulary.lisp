(in-package :thermo)

(export '(gamma))

(defun gamma (Cp)
  "Gamma, calculated from Cp and the gas constant"
  (/ Cp
     (- Cp physics-constants:+gas-constant-sp+)))

(defun Cv-of-Cp (Cp)
  "Calculate Cv from Cp using the ideal gas law"
  (- Cp physics-constants:+gas-constant-sp+))



