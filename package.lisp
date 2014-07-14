(in-package #:cl-user)

(defpackage utils-frahm
  (:use #:cl #:anaphora)
  (:export

   ;;; list.lisp
   ;; listify and flatten are available for example in alexandria and iterate

   ;;; parse-body.lisp
   ;; since this used only internally, it's not exported (but not
   ;; subject to change anytime soon, since I intend to keep it in
   ;; sync with sb-int:parse-body)

   ;;; do-mapper.lisp
   #:do-mapper #:do-mapcar

   ;;; strings.lisp
   #:conc ;; also available from CL-WHO
   #:mkstr

   ;;; symbols.lisp
   #:symb

   ;;; bang-symbols.lisp
   #:define-symbol-p
   #:g!-symbol-p #:o!-symbol-p
   #:!-symbol-p
   #:any!-symbol-p
   #:!-symbol-to-symbol
   #:recursive-!-symbol-to-symbol

   ;;; macros.lisp
   #:defmacro!

   ;;; utils-frahm.lisp
   #:eqcond #:eqcase
   #:defvar* #:defconstant*
   #:mapcar/values
   #:mapcar/values-n

   ;;; locked-deque.lisp
   #:locked-deque
   #:make-locked-deque #:locked-deque-emptyp
   #:enqueue
   #:dequeue #:dequeue-all #:dequeue-if
   #:dequeue-wait #:dequeue-wait-all #:dequeue-wait-if
   #:dequeue-wait-timeout #:dequeue-wait-if-timeout
   #:dequeue-wait-all-timeout

   ;;; rwlock.lisp
   #:make-rwlock #:with-rwlock-held #:with-rwlock-held*
   #:downgrade #:upgrade

   ;;; html.lisp
   ;; #:+dtd-xhtml-11-public+ #:+dtd-xhtml-11-system+
   ;; #:+dtd-xhtml-11+ #:+decl-xml-10+ #:+xmlns-xhtml+
   ;; #:+prologue-xhtml-11+
   ))
