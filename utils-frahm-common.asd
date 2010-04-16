(in-package #:cl-user)

(asdf:defsystem :utils-frahm-common
  :author "Olof-Joachim Frahm <Olof.Frahm@web.de>"
  :licence "GPL3"
  :depends-on (#:anaphora)
  :serial T
  :components ((:file "package")
	       (:file "list")
	       #-sbcl (:file "parse-body")
	       (:file "do-mapper")
	       (:file "strings")
	       (:file "bang-symbols")
	       (:file "symbols")
	       (:file "macros")
	       (:file "utils-frahm")
	       ;;(:file "html")
	       ))
