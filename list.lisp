(in-package #:utils-frahm)

(defun flatten (x)
  "Flattens a CONS tree."
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                       (car x)
                       (rec (cdr x) acc))))))
    (rec x nil)))

(defun listify (x)
  "Ensures X is either a list or wrapped in one."
  (if (not (listp x)) (list x) x))
