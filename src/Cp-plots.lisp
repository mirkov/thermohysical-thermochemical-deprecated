(in-package :thermo-user)


(defmacro make-plot-body (what)
  `(let* ((gas (make-thermo-data species))
	  (temp (apply #'lseq
		       (append (coeffs-temperature-range gas)
			       `(,points)))))
     (plot-xy temp (gcmap (,what @!temp gas) temp))))

(defun plot-cp (species &optional (points 101))
  "`species' C-p plot vs temperature "
  (make-plot-body S0))

(defun plot-S0 (species &optional (points 101))
  "`species' H-rel plot vs temperature "
  (make-plot-body S0))

(defun plot-H-rel (species &optional (points 101))
  "`species' H-rel plot vs temperature "
  (make-plot-body H-rel))

