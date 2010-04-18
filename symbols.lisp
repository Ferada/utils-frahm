(in-package #:utils-frahm)

(defun symb (&rest args)
  "Creates a new interned symbol by concatenating the string
representations of ARGS."
  (intern (apply #'mkstr args)))

(defun symbs/n (prefix from to)
  (loop
     for i from from to to
     collect (symb prefix i)))
