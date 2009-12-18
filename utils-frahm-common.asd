(in-package #:cl-user)

(asdf:defsystem :utils-frahm-common
  :depends-on (#:anaphora)
  :serial T
  :components ((:file "package")
	       (:file "utils-frahm")
	       (:file "html")))
