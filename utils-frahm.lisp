(in-package #:cl-user)

(defpackage :utils-frahm
  (:use #:cl #:anaphora)
  (:export #:defmacro/g! #:defmacro!
	   #:eqcond #:defvar* #:defconstant*
	   #:conc
	   #:make-locked-deque #:locked-deque-emptyp
	   #:enqueue
	   #:dequeue #:dequeue-all #:dequeue-match
	   #:dequeue-wait #:dequeue-wait-all #:dequeue-wait-match
	   #:make-rwlock #:with-rwlock-held #:with-rwlock-held*))

(in-package #:utils-frahm)

(defmacro λ (args &rest def)
  `(lambda ,args ,@def))

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

;;; history mechanism

(defun history-augment-setq (form fun)
  (if (and (listp form)
	   (not (null form)))
      (let ((augmented-form (cons (car form) (mapcar (lambda (form) (history-augment-setq form fun))
						     (cdr form)))))
	(if (eq (car augmented-form) 'SETQ)
	    (funcall fun (cadr augmented-form) (caddr augmented-form))
	    augmented-form))
      form))

(defun history-variables (form)
  (let ((vars))
    (sb-cltl2:macroexpand-all
     `(macrolet ((max/hist (var)
		   (funcall ,(lambda (x) (pushnew x vars)) var)
		   var))
	,@form))
    vars))

(defmacro! with-hist (&body body)
  ;; identify invocations of history macros
  ;; summarize the referenced variables (whats with symbol macros?)
  ;; add accumulator code
  (let ((vars (mapcar (lambda (var) (cons var (gensym))) (history-variables body))))
    `(let (,@(mapcar (lambda (var) `(,(cdr var) ,(car var))) vars))
       (macrolet ((max/hist (var)
		    (cdr (assoc var ',vars))))
	 ,@(mapcar (lambda (form)
		     (format T "augmenting part ~A~%" form)
		     (history-augment-setq (sb-cltl2:macroexpand-all form)
					   (lambda (var expr)
					     (aif (assoc var vars)
						  `(let ((,g!foo ,expr))
						     (setq ,(cdr it) (max ,(cdr it) ,g!foo)
							   ,var ,g!foo))
						  `(setq ,var ,expr)))))
		   body)))))

(defvar test-1
  '(defun test-1 (&optional (list '(1 2 3 4 5 6)))
    (let (x)
      (with-hist
	(loop for i in list
	   do (setq x i))
	(format T "max of i is ~A~%" (max/hist x))))))

(defun test-1 (&optional (list '(1 2 3 4 5 6)))
  (let ((x (car list)) (y (- (car list))))
    (with-hist
      (loop for i in list
	 do (setq x i)
	 do (setq y (- i)))
      (format T "max of i is ~A~%" (max/hist x))
      (format T "max of (- i) is ~A~%" (max/hist y)))))

;; TODO: make this extensible using a registry of expanders
;; TODO: wanted is value, location, timestamp

;; operators
;; value: max, list
;; location: times
;; timestamp: when
