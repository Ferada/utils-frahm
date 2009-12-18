(in-package #:cl-user)

(defpackage utils-frahm
  (:use #:cl #:anaphora)
  (:export ;; utils-frahm.lisp
           #:it
	   #:defmacro!
	   #:eqcond #:defvar* #:defconstant*
	   #:alet #:rlet
	   #:conc
	   ;; #:with-symbols-from

	   ;;; locked-deque.lisp
	   #:make-locked-deque #:locked-deque-emptyp
	   #:enqueue
	   #:dequeue #:dequeue-all #:dequeue-if
	   #:dequeue-wait #:dequeue-wait-all #:dequeue-wait-if
	   #:dequeue-wait-timeout #:dequeue-wait-if-timeout
	   #:dequeue-wait-all-timeout
	   #:make-rwlock #:with-rwlock-held #:with-rwlock-held*

	   ;;; html.lisp
	   #:+dtd-xhtml-11-public+ #:+dtd-xhtml-11-system+
	   #:+dtd-xhtml-11+ #:+decl-xml-10+ #:+xmlns-xhtml+
	   #:+prologue-xhtml-11+))
