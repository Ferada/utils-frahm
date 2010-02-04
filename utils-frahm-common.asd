(in-package #:cl-user)

(asdf:defsystem :utils-frahm-common
  :author "Olof-Joachim Frahm <Olof.Frahm@web.de>"
  :licence "GPL3"
  :depends-on (#:anaphora)
  :serial T
  :components ((:file "package")
	       (:file "utils-frahm")
	       (:file "html")))
