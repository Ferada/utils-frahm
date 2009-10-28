(in-package #:utils-frahm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-doctype (root public system)
    (conc "<!DOCTYPE " root " PUBLIC \"" public "\" \"" system "\">")))

(eval-when (:compile-toplevel)
  (defconstant +dtd-xhtml-11-public+ "-//W3C//DTD XHTML 1.1//EN")
  (defconstant +dtd-xhtml-11-system+ "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd")
  (defconstant +dtd-xhtml-11+ (make-doctype "html" +dtd-xhtml-11-public+ +dtd-xhtml-11-system+))

  (defconstant +decl-xml-10+ "<?xml version=\"1.0\" encoding=\"utf-8\"?>")

  (defconstant +xmlns-xhtml+ "http://www.w3.org/1999/xhtml")

  (defconstant +prologue-xhtml-11+ (conc +decl-xml-10+ +dtd-xhtml-11+)))
