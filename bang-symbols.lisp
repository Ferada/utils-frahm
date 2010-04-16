(in-package #:utils-frahm)

(defmacro define-symbol-p (prefix &optional (function NIL function-p))
  "Defines a symbol prefix matcher.  It's named PREFIX-SYMBOL-P if
FUNCTION isn't specified.  PREFIX has to be either a STRING or a SYMBOL."
  (check-type prefix (or string symbol))
  (unless (stringp prefix)
    (setf prefix (mkstr prefix)))
  (unless function-p
    (setf function (symb prefix '#:-symbol-p)))
  (let ((length (length prefix)))
    `(defun ,function (symbol)
       (when (symbolp symbol)
	 (let ((name (symbol-name symbol)))
	   (and (> (length name) ,length)
		(string= name ,prefix :start1 0 :end1 ,length)))))))

(define-symbol-p #:G!)
(define-symbol-p #:O!)

(defun o!-symbol-to-g!-symbol (symbol)
  "Converts a O!FOO symbol to G!FOO."
  (symb "G!" (subseq (symbol-name symbol) 2)))

(defun !-symbol-p (symbol)
  "Checks if SYMBOL is a bang symbol (any symbol with a \"!\" as the
second character of the symbol name."
  (when (symbolp symbol)
    (let ((name (symbol-name symbol)))
      (and (> (length name) 2)
	   (string= name "!" :start1 1 :end1 2)))))

(defun any!-symbol-p (s &rest prefixes)
  "Returns T if the symbol S is a bang symbol with any of the PREFIXES."
  (when (symbolp s)
    (let ((name (symbol-name s)))
      (when (> (length name) 2)
	(let ((c0 (aref name 0)))
	  (and (char= #\! (aref name 1))
	       (some (lambda (prefix)
		       (char= c0 prefix))
		     prefixes)))))))

(defun !-symbol-to-symbol (s)
  "Creates a new interned symbol by stripping the bang prefix off the
symbol name."
  (symb (subseq (symbol-name s) 2)))

(defun recursive-!-symbol-to-symbol (list)
  "Sames as !-SYMBOL-TO-SYMBOL but recurses on lists."
  (do-mapcar ((x list))
    (cond
      ((and (symbolp x) (any!-symbol-p x #\O #\G)) (!-symbol-to-symbol x))
      ((listp x) (recursive-!-symbol-to-symbol x))
      (T x))))
