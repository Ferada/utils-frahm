(in-package #:utils-frahm)

(defun conc (&rest strings)
  "Concatenates a number of STRINGS."
  (apply #'concatenate 'string strings))

(defun mkstr (&rest args)
  "Concatenates the string representations of ARGS using PRINC."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))
