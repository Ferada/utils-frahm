(in-package #:cl-user)

(asdf:defsystem :utils-frahm
  :depends-on (#:anaphora
	       #:alexandria
	       #:cl-walker
	       #:cl-syntax-sugar
	       #:bordeaux-threads
	       #:trivial-timeout)
  :serial T
  :components ((:file "package")
	       (:file "utils-frahm")
	       (:file "html")
	       (:file "locked-deque")))
