(in-package #:tcu)

(defun plot-he3/4-alpha-t ()
  "Replicate Fig. 10.8 of Ferziger & Kaper"
    (let* ((he4 (make-species-lennard-jones-6/12-potential :HE))
       (he3 (make-lennard-jones-6/12-potential
	     3.0 (sigma He4) (epsilon/k He4) :He3))
       (temp (lseq 10d0 10d0)))
  (set-to ((logscale :x))
    (plot-xy temp
	     (gcmap (alpha-t He4 He3 0.5 @!temp) temp)))))