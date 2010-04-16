(in-package #:utils-frahm)

;;; modified from sbcl src/code/parse-body.lisp

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(defun parse-body (body &key (doc-string-allowed t) toplevel)
  (let (reversed-decls doc)
    ;; Since we don't have macros like AND, OR, and NOT yet, it's hard
    ;; to express these tests clearly. Giving them names seems to help
    ;; a little bit.
    (flet ((doc-string-p (x remaining-forms)
	     (when (and (stringp x) doc-string-allowed remaining-forms)
	       (if doc (error "duplicate doc string ~S" x) T)))
           (declaration-p (x)
             (when (consp x)
	       (case (car x)
		 (declare T)
		 (declaim
		  (unless toplevel
		    ;; technically legal, but rather unlikely to
		    ;; be what the user meant to do...
		    #+sbcl (sb-int:style-warn "DECLAIM where DECLARE was probably intended")
		    #-sbcl (warn "DECLAIM where DECLARE was probably intended")))
		 (T NIL)))))
      (do () ((null body))
	(let ((form1 (first body))
	      (rest (rest body)))
	  (cond
	    ((doc-string-p form1 rest) (setf doc form1))
	    ((declaration-p form1) (push form1 reversed-decls))
	    (T (return)))
	  (setf body rest)))
      (values body (nreverse reversed-decls) doc))))
