(in-package #:cl-user)

(asdf:defsystem :utils-frahm-threaded
  :author "Olof-Joachim Frahm <Olof.Frahm@web.de>"
  :licence "GPL3"
  :depends-on (#:utils-frahm-common
               #:bordeaux-threads
               #:trivial-timeout)
  :components ((:file "rwlock")
               (:file "locked-deque")))
