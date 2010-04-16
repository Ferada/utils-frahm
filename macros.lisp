(in-package #:utils-frahm)

;;;; tools from let over lambda

;;; defmacro! helpers

(defmacro defmacro/g! (name args &rest body)
  (multiple-value-bind (forms decls doc)
      #-sbcl (parse-body body)
      #+sbcl (sb-int:parse-body body)
    (let ((syms (remove-duplicates (remove-if-not #'g!-symbol-p (flatten body)))))
      `(defmacro ,name ,args
	 ,.(when doc (list doc))
	 ,.decls
	 (let ,(mapcar (lambda (s) `(,s (gensym ,(subseq (symbol-name s) 2)))) syms)
	   ,.forms)))))

(defmacro defmacro! (name args &rest body)
  "Defines a macro like the standard DEFMACRO except that all arguments
starting with \"O!\" are evaluated only once via LET and than are
available via a \"G!\" prefix.  Symbols with a \"G!\" prefix generate
a shared new uninterned symbol via GENSYM.  If a macro defined with this
returns multiple values, the second is an alist with possible members
:LET-P and :IGNORABLE-P which are enabled by default and may disable the
use of the LET bound variables and the added IGNORABLE declaration."
  (multiple-value-bind (forms decls doc)
      #-sbcl (parse-body body)
      #+sbcl (sb-int:parse-body body)
    (let* ((os (remove-if-not #'o!-symbol-p (flatten args)))
	   (gs (mapcar #'o!-symbol-to-g!-symbol os))
	   (result (gensym "RESULT"))
	   (options (gensym "OPTIONS")))
      `(defmacro/g! ,name ,(recursive-!-symbol-to-symbol args)
	 ,.(when doc (list doc))
	 ,.decls
	 (block ,(symb '#:outer- name)
	   (multiple-value-bind (,result ,options)
	       (block ,name ,.forms)
	     (when ,result
	       ;; if let-p isn't present (assoc returns NIL) or if its associated
	       ;; value isn't NIL, that is, T, LET-bind those variables
	       (if (alet (assoc :let-p ,options :test #'eq)
		     (if it (cdr it) T))
		   `(let ,(mapcar #'list (list ,.gs) (list ,.(mapcar #'!-symbol-to-symbol os)))
		      ,.(when (alet (assoc :ignorable-p ,options)
				(and (if it (cdr it) T) ,(not (null gs))))
			  `((declare (ignorable ,.(list ,.gs)))))
		      ,,result)
		   ,result))))))))

;; (defmacro defmacro! (name args &rest body)
;;   "Defines a macro like the standard DEFMACRO except that all arguments
;; starting with \"O!\" are evaluated only once via LET and than are
;; available via a \"G!\" prefix.  Symbols with a \"G!\" prefix generate
;; a shared new uninterned symbol via GENSYM.  If a macro defined with this
;; returns multiple values, the second determines if "
;;   (multiple-value-bind (forms decls doc)
;;       #-sbcl (parse-body body)
;;       #+sbcl (sb-int:parse-body body)
;;     (let* ((os (remove-if-not #'o!-symbol-p (flatten args)))
;; 	   (gs (mapcar #'o!-symbol-to-g!-symbol os))
;; 	   (result (gensym "RESULT"))
;; 	   (no-let-p (gensym "NO-LET-P"))
;; 	   (no-ignorable-p (gensym "NO-IGNORABLE-P")))
;;       `(defmacro/g! ,name ,(recursive-!-symbol-to-symbol args)
;; 	 ,.(when doc (list doc))
;; 	 ,.decls
;; 	 (block ,(symb '#:outer- name)
;; 	   (multiple-value-bind (,result ,no-let-p ,no-ignorable-p)
;; 	       (block ,name ,.forms)
;; 	     (when ,result
;; 	       (if (not ,no-let-p)
;; 		   (values `(let ,(mapcar #'list (list ,.gs) (list ,.(mapcar #'!-symbol-to-symbol os)))
;; 			      ,.(when (and (not ,no-ignorable-p) ,(not (null gs)))
;; 				  `((declare (ignorable ,.(list ,.gs)))))
;; 			      ,,result)
;; 			   ,no-let-p ,no-ignorable-p)
;; 		   (values ,result ,no-let-p ,no-ignorable-p)))))))))

;; (defmacro defmacro! (name args &rest body)
;;   "Defines a macro like the standard DEFMACRO except that all arguments
;; starting with \"O!\" are evaluated only once via LET and than are
;; available via a \"G!\" prefix.  Symbols with a \"G!\" prefix generate
;; a shared new uninterned symbol via GENSYM."
;;   (multiple-value-bind (forms decls doc)
;;       #-sbcl (parse-body body)
;;       #+sbcl (sb-int:parse-body body)
;;     (let* ((os (remove-if-not #'o!-symbol-p (flatten args)))
;; 	   (gs (mapcar #'o!-symbol-to-g!-symbol os))
;; 	   (result (gensym "RESULT")))
;;       `(defmacro/g! ,name ,(recursive-!-symbol-to-symbol args)
;; 	 ,.(when doc (list doc))
;; 	 ,.decls
;; 	 (let ((,result (progn ,.forms)))
;; 	   (when ,result
;; 	     `(let ,(mapcar #'list (list ,.gs) (list ,.(mapcar #'!-symbol-to-symbol os)))
;; 		,.(unless ,(null gs) `((declare (ignorable ,.(list ,.gs)))))
;; 		,,result)))))))

;; (defmacro defmacro! (name args &rest body)
;;   "Defines a macro like the standard DEFMACRO except that all arguments
;; starting with \"O!\" are evaluated only once via LET and than are
;; available via a \"G!\" prefix.  Symbols with a \"G!\" prefix generate
;; a shared new uninterned symbol via GENSYM."
;;   (multiple-value-bind (forms decls doc)
;;       #-sbcl (parse-body body)
;;       #+sbcl (sb-int:parse-body body)
;;     (let* ((os (remove-if-not #'o!-symbol-p (flatten args)))
;; 	   (gs (mapcar #'o!-symbol-to-g!-symbol os)))
;;       `(defmacro/g! ,name ,(recursive-!-symbol-to-symbol args)
;; 	 ,.(when doc (list doc))
;; 	 ,.decls
;; 	 `(let ,(mapcar #'list (list ,.gs) (list ,.(mapcar #'!-symbol-to-symbol os)))
;; 	    ,.(unless ,(null gs) `((declare (ignorable ,.(list ,.gs)))))
;; 	    ,(progn ,.forms))))))
