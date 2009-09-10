(in-package #:cl-user)

(defpackage :utils-frahm
  (:use #:cl #:anaphora)
  (:export #:defmacro/g! #:defmacro!
	   #:eqcond #:defvar* #:defconstant*
	   #:conc
	   #:make-locked-deque #:locked-deque-emptyp
	   #:enqueue
	   #:dequeue #:dequeue-all #:dequeue-if
	   #:dequeue-wait #:dequeue-wait-all #:dequeue-wait-if
	   #:make-rwlock #:with-rwlock-held #:with-rwlock-held*))

(in-package #:utils-frahm)

(defmacro λ (args &rest def)
  `(lambda ,args ,.def))

(defun conc (&rest strings)
  (apply #'concatenate 'string strings))

;;;; tools from let over lambda

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun flatten (x)
    (labels ((rec (x acc)
	       (cond ((null x) acc)
		     ((atom x) (cons x acc))
		     (t (rec
			 (car x)
			 (rec (cdr x) acc))))))
      (rec x nil))))

;;; defmacro* helpers

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s)))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun symb (&rest args)
    (values (intern (apply #'mkstr args)))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun g!-symbol-p (s)
    (and (symbolp s)
	 (> (length (symbol-name s)) 2)
	 (string= (symbol-name s)
		  "G!"
		  :start1 0
		  :end1 2))))

;; 20090730 - added docstring handling
(defmacro defmacro/g! (name args &rest body)
  (let* ((syms (remove-duplicates
		(remove-if-not #'g!-symbol-p
			       (flatten body))))
	 (docstringp (stringp (car body)))
	 (prebody (when docstringp
		    (list (car body))))
	 (body (if docstringp (cdr body) body)))
    `(defmacro ,name ,args
       ,@prebody
       (let ,(mapcar
	      (lambda (s)
		`(,s (gensym ,(subseq
			       (symbol-name s)
			       2))))
	      syms)
         ,@body))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun o!-symbol-p (s)
    (and (symbolp s)
	 (> (length (symbol-name s)) 2)
	 (string= (symbol-name s)
		  "O!"
		  :start1 0
		  :end1 2))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun o!-symbol-to-g!-symbol (s)
    (symb "G!" (subseq (symbol-name s) 2))))

;; 20090730 - added docstring handling
;; 20090515 - added flatten in front of args
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro defmacro! (name args &rest body)
    (let* ((os (remove-if-not #'o!-symbol-p (flatten args)))
	   (gs (mapcar #'o!-symbol-to-g!-symbol os))
	   (docstringp (stringp (car body)))
	   (prebody (when docstringp
		      (list (car body))))
	   (body (if docstringp (cdr body) body)))
      `(defmacro/g! ,name ,args
	 ,@prebody
	 `(let ,(mapcar #'list (list ,@gs) (list ,@os))
	    ,(progn ,@body))))))

;;;; my own tools

(defmacro! eqcond (o!equal o!keyform &body cases)
  "EQCASE Equal Keyform {({(Key*) | Key} Form*)}*
Evaluates the Forms in the first clause with a Key Equal to the value of
Keyform.  If a singleton key is T then the clause is a default clause.
If you really want to test for T, use (T) as Key."
  (let (newsyms newcases)
    (dolist (acase cases)
      (let ((car (car acase)))
	(if (listp car)
	    ;; splice the individual keys
	    (let ((valsym (gensym)))
	      (push (list valsym (cadr acase)) newsyms)
	      (dolist (key car)
		(push (cons key valsym) newcases)))
	    (push (cons (if (eq car T) g!T car) (cadr acase)) newcases))))
    (setf newsyms (nreverse newsyms)
	  newcases (nreverse newcases))
    (let ((result `(cond
		     ,.(mapcar (lambda (acase)
				 (if (eq (car acase) g!T)
				     `(T ,(cdr acase))
				     `((funcall ,g!equal ,g!keyform ,(car acase)) ,(cdr acase))))
			       newcases))))
      (if (null newsyms)
	  result
	  `(let (,@newsyms)
	     ,result)))))

;; (defmacro eqcase (equal keyform &body cases)
;;   "EQCASE Equal Keyform {({(Key*) | Key} Form*)}*
;; Evaluates the Forms in the first clause with a Key Equal to the value of
;; Keyform.  If a singleton key is T then the clause is a default clause.
;; If you really want to test for T, use (T) as Key."
;;   (let ((key-sym (gensym)))
;;     `(let ((,key-sym ,keyform))
;;        (declare (ignorable ,key-sym))
;;        (cond
;; 	 ,@(mapcar #'(lambda (a-case)
;; 		       (let ((vals (car a-case)) (handler (cdr a-case)))
;; 			 (if (listp vals)
;; 			     `((or ,@(mapcar #'(lambda (val) `(funcall ,equal ,key-sym ,val)) vals))
;; 			       ,@(cdr a-case))
;; 			     (if (eq vals T)
;; 				 `(T ,@handler)
;; 				 `((funcall ,equal ,key-sym ,vals) ,@handler)))))
;; 		   cases)))))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro defvar* (var doc)
  "Creates a new unbound variable with documentation."
  `(progn
     (defvar ,var)
     (setf (documentation ',var 'variable) ,doc)))

(defmacro defconstant* (name value &optional doc)
  "Defines a constant even if it's already bound."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,.(when doc (list doc))))

;; Compose macro
;; Usage: #M(abs /) ==> (lambda (a b) (abs (/ a b)))

(defmacro enable-compose-syntax (&optional (dispatch-character #\#) (sub-character #\M))
  `(eval-when (:compile-toplevel :execute)
     (setf *readtable* (copy-readtable *readtable*))
     (%enable-compose-reader ,dispatch-character ,sub-character)))

(defun %enable-compose-reader (&optional (dispatch-character #\#) (sub-character #\M))
  (set-dispatch-macro-character dispatch-character sub-character #'compose-reader *readtable*))

(defun compose-reader (stream sub-character infix-parameter)
  (when infix-parameter
    (error "#~a does not take an integer infix parameter."
	   sub-character))
  `(alexandria:compose
    ,@(loop for x in (read stream t nil t)
	 collect (typecase x
		   (function x)
		   (cons x)
		   (t `#',x)))))

;; Curry macro
;; Usage: #R(* 3) ==> (lambda (&rest r) (apply #'* 3 r))
#+(or)(set-dispatch-macro-character
       #\# #\R (lambda (stream sub-character infix-parameter)
		 (when infix-parameter
		   (error "#~a does not take an integer infix parameter."
			  sub-character))
		 (let ((expr (read stream t nil t)))
		   `(alexandria:curry
		     ,(typecase (first expr)
				(function (first expr))
				(cons (first expr))
				(t `#',(first expr)))
		     ,@(rest expr)))))

(defmacro alet (form &body body)
  `(let ((it ,form))
     ,.body))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun replace-bindings (bindings body)
    (aif (rassoc body bindings :key #'car :test #'eq)
	 (car it)
	 (if (atom body)
	     body
	     (mapcar (lambda (x) (replace-bindings bindings x)) body)))))

(defmacro rlet (bindings &body body)
  "(reader-let)
(rlet (#1=(list 1 2 3))
  (append #1# #1#))"
  (if bindings
      (let ((bindings (mapcar (lambda (binding) `(,(gensym) ,binding))
			      bindings)))
	`(let ,bindings
	   ,.(replace-bindings bindings body)))
      `(progn ,.body)))
