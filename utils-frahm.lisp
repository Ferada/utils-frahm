(in-package #:utils-frahm)

;; (defmacro λ (args &rest def)
;;   `(lambda ,args ,.def))

;;;; my own tools

(defmacro! %eqcond (quote-p o!test o!keyform &body cases)
  (check-type quote-p boolean)
  (when cases
    (flet ((expand (value)
	     `(funcall ,g!test ,g!keyform ,(if quote-p `(quote ,value) value)))
	   (default-p (form)
	     (or (eq T form) (eq 'otherwise form))))
      (values
       `(cond
	  ,.(do (case last-p result)
		((null cases) (nreverse result))
	      (setf case (pop cases)
		    last-p (null cases))
	      (unless (typep case 'list)
		(error "%EQCOND clause is not a list: ~S" case))
	      (let ((car (car case))
		    (cdr (cdr case)))
		(when (and last-p (null result) (default-p car))
		  (format T "returning~%")
		  (return-from %eqcond
		    (values (when cdr
			      (if (= 1 (length cdr))
				  (car cdr)
				  `(progn ,.cdr)))
			    T T)))
		(push (if (and last-p (default-p car))
			  `(T ,.cdr)
			  `((or ,.(mapcar #'expand (listify car))) ,.(if cdr cdr '(NIL))))
		      result))))
       NIL T))))

(defmacro eqcond (test keyform &body cases)
  "EQCOND Test Keyform {({(Key*) | Key} Form*)}*
Evaluates the Forms in the first clause with a evaluted Key equal (using
Test) to the value of Keyform.  If the last singleton key is T or OTHERWISE
then the clause is the default clause.  If you really want to test for T,
use (T) as Key."
  `(%eqcond NIL ,test ,keyform ,.cases))

(defmacro eqcase (test keyform &body cases)
  "EQCASE Test Keyform {({(Key*) | Key} Form*)}*
Evaluates the Forms in the first clause with a quoted Key equal (using
Test) to the value of Keyform.  If the last singleton key is T or
OTHERWISE then the clause is the default clause."
  `(%eqcond T ,test ,keyform ,.cases))

(defmacro defvar* (var doc)
  "Creates a new unbound variable with documentation."
  (check-type var symbol)
  `(progn
     (defvar ,var)
     (setf (documentation ',var 'variable) ,doc)))

(defmacro defconstant* (name value &optional doc)
  "Defines a constant even if it's already bound."
  (check-type name symbol)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,.(when doc (list doc))))

;; Compose macro
;; Usage: #M(abs /) ==> (lambda (a b) (abs (/ a b)))

#+(or)
(defmacro enable-compose-syntax (&optional (dispatch-character #\#) (sub-character #\M))
  `(eval-when (:compile-toplevel :execute)
     (setf *readtable* (copy-readtable *readtable*))
     (%enable-compose-reader ,dispatch-character ,sub-character)))

#+(or)
(defun %enable-compose-reader (&optional (dispatch-character #\#) (sub-character #\M))
  (set-dispatch-macro-character dispatch-character sub-character #'compose-reader *readtable*))

#+(or)
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
#+(or)
(set-dispatch-macro-character
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

;; (eval-when (:load-toplevel :compile-toplevel :execute)
;;   (defun replace-bindings (bindings body)
;;     (aif (rassoc body bindings :key #'car :test #'eq)
;; 	 (car it)
;; 	 (if (atom body)
;; 	     body
;; 	     (mapcar (lambda (x) (replace-bindings bindings x)) body)))))

;; (defmacro! rif (test then &optional else)
;;   (let ((bindings `((,g!test ,test))))
;;     `(let ,bindings
;;        (if ,g!test
;; 	   ,(replace-bindings bindings then)
;; 	   ,(when else
;; 		  (replace-bindings bindings else))))))

;; (defmacro! rwhen (test &body forms)
;;   (let ((bindings `((,g!test ,test))))
;;     `(let ,bindings
;;        (when ,g!test
;; 	 ,.(replace-bindings bindings forms)))))

;; (defmacro! runless (test &body forms)
;;   (let ((bindings `((,g!test ,test))))
;;     `(let ,bindings
;;        (unless ,g!test
;; 	 ,.(replace-bindings bindings forms)))))

;; (defmacro rlet (bindings &body body)
;;   "(reader-let)
;; (rlet (#1=(list 1 2 3))
;;   (append #1# #1#))"
;;   (if bindings
;;       (let ((bindings (mapcar (lambda (binding) `(,(gensym) ,binding))
;; 			      bindings)))
;; 	`(let ,bindings
;; 	   ,.(replace-bindings bindings body)))
;;       `(progn ,.body)))

;;; TODO: doesn't work as expected, so needs more thought

;; (defmacro with-symbols-from (package (&rest symbols) &body body)
;;   "Temporarily using external symbols from another package without
;; actually importing them into the current one."
;;   (setf package (find-package package))
;;   (when (eq package *package*)
;;     ;; why should we import from ourselves?
;;     (return-from with-symbols-from body))
;;   (let (macrolets)
;;     (dolist (symbol symbols)
;;       (multiple-value-bind (import visible)
;; 	  (find-symbol (symbol-name symbol) package)
;; 	(ecase visible
;; 	  ((nil)
;; 	   (error "symbol ~A is not present in package ~A"
;; 		  symbol package))
;; 	  ((:internal :inherited)
;; 	   (error "symbol ~A is internal in package ~A"
;; 		  import package))
;; 	  ((:external) (push `(,symbol ,import) macrolets)))))
;;     `(symbol-macrolet ,(nreverse macrolets)
;;        ,.body)))

(defun symbs/n (prefix from to)
  (loop
     for i from from to to
     collect (symb prefix i)))

;; TODO: different accumulation strategies (MAPCON, MAPCAN, MAPINTO?)
(defmacro define-mapcar/values-n (n &optional doc n-doc &aux (prefix '#:%mapcar/values-))
  `(progn
     ,.(loop
	  for i from 2 to n
	  as lists = (symbs/n '#:list- 1 i)
	  and results = (symbs/n '#:result- 1 i)
	  and args = (symbs/n '#:arg- 1 i)
	  and mvbs = (symbs/n '#:mvb- 1 i)
	  collect `(defun ,(symb prefix i) (function ,.lists)
		     (let (,.results)
		      (loop
			 ,.(loop
			      for list in lists
			      for arg in args
			      nconc `(for ,arg in ,list))
			 do (multiple-value-bind (,.mvbs)
				(funcall function ,.args)
			      .,(loop
				   for result in results
				   for mvb in mvbs
				   collect `(push ,mvb ,result)))
			 finally
			 (return (values ,.(loop
					      for result in results
					      collect `(nreverse ,result))))))))
     ;; %mapcar/values-n
     (defun ,(symb prefix '#:n) (naccs function &rest lists &aux (length (length lists)))
       (let ((lists (make-array length :element-type 'list :initial-contents lists))
	     (accs (make-array naccs :element-type 'list :initial-element NIL)))
	 (flet ((pop-args ()
		  (loop
		     for i from 0 below length
		     as list = (aref lists i)
		     unless list return NIL
		     collect (pop (aref lists i)))))
	  (loop
	     as args = (pop-args)
	     while args
	     do (let ((results (multiple-value-list (apply function args))))
		  (loop
		     for i from 0 below naccs
		     for result in results
		     do (push result (aref accs i))))
	     finally (loop
			for i from 0 below naccs
			collect (nreverse (aref accs i)) into values
			finally (return-from ,(symb prefix '#:n) (values-list values)))))))
     (defun mapcar/values-n (n function list &rest more-lists)
       ,n-doc
       (apply #',(symb prefix '#:n) n function list more-lists))
     (defun mapcar/values (function list &rest more-lists &aux (length (length more-lists)))
       ,doc
       (case length
	 (0 (mapcar function list))
	 ,.(loop
	      for i from 1 to (1- n)
	      collect `(,i (apply #',(symb prefix (1+ i)) function list more-lists)))
	 (T (apply #',(symb prefix '#:n) (1+ length) function list more-lists))))
     (define-compiler-macro mapcar/values (function list &rest more-lists &aux (length (length more-lists)))
       (case length
	 (0 `(mapcar ,function ,list))
	 ,.(loop
	      for i from 1 to (1- n)
	      collect `(,i `(,',(symb prefix (1+ i)) ,function ,list ,.more-lists)))
	 (T `(,',(symb prefix '#:n) ,(1+ length) ,function ,list ,.more-lists))))))

;;; choose a reasonable value for n, e.g. 3 or so
(define-mapcar/values-n 3
    "Like MAPCAR but accumulates N multiple return values (efficiently)
where N is the number of input lists."
  "Like MAPCAR but accumulates N multiple return values (efficiently).
N has to be specified in advance to allow for efficient accumulation.")

#+(or)
(defmacro logv/type (&rest args)
  `(progn
     ,.(do-mapcar ((arg args))
	 (alet (gensym)
	   `(let ((,it ,arg))
	      (format-log "~S [~S] -> ~S" ',arg (type-of ,it) ,it)
	      ,it)))))
