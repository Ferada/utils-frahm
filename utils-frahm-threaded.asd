(in-package #:cl-user)

(asdf:defsystem :utils-frahm-threaded
  :depends-on (#:utils-frahm-common
	       #:bordeaux-threads
	       #:trivial-timeout)
  :serial T
  :components ((:file "locked-deque")))
