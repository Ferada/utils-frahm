(in-package #:cl-user)

(asdf:defsystem :utils-frahm
  :depends-on (#:anaphora #:alexandria #:cl-walker #:cl-syntax-sugar #:bordeaux-threads)
  :serial T
  :components ((:file "utils-frahm")
	       (:file "locked-deque")))
