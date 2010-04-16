(in-package #:utils-frahm)

(defstruct (rwlock (:constructor create-rwlock))
  (readers 0)
  (writers 0)
  (requests 0)
  (lock NIL :read-only T)
  (free (bt:make-condition-variable) :read-only T))

(defun make-rwlock (&optional name)
  (create-rwlock :lock (bt:make-lock name)))

(defun lock-read (rwlock)
  (with-slots (readers writers requests lock free) rwlock
    (bt:with-lock-held (lock)
      (loop while (or (> writers 0) (> requests 0))
	 do (bt:condition-wait free lock))
      (incf readers))))

(defun unlock-read (rwlock)
  (with-slots (readers lock free) rwlock
    (bt:with-lock-held (lock)
      (decf readers)
      (bt:condition-notify free))))

(defun lock-write (rwlock)
  (with-slots (readers writers requests lock free) rwlock
    (bt:with-lock-held (lock)
      (incf requests)
      (loop while (or (> readers 0) (> writers 0))
	 do (bt:condition-wait free lock))
      (decf requests)
      (incf writers))))

(defun unlock-write (rwlock)
  (with-slots (writers lock free) rwlock
    (bt:with-lock-held (lock)
      (decf writers)
      (bt:condition-notify free))))

(defun lock-downgrade (rwlock)
  (with-slots (readers writers lock free) rwlock
    (bt:with-lock-held (lock)
      (decf writers)
      (incf readers)
      (bt:condition-notify free))))

(defun lock-upgrade (rwlock)
  (with-slots (readers writers requests lock free) rwlock
    (bt:with-lock-held (lock)
      (decf readers)
      (incf requests)
      (loop while (or (> readers 0) (> writers 0))
	 do (bt:condition-wait free lock))
      (decf requests)
      (incf writers))))

(defmacro! with-rwlock-held ((o!place &optional (mode :read)) &body body)
  `(progn
     (,(ecase mode
	 (:read 'lock-read)
	 (:write 'lock-write)) ,g!place)
     (unwind-protect (progn ,.body)
       (,(ecase mode
	   (:read 'unlock-read)
	   (:write 'unlock-write)) ,g!place))))

(defmacro! with-rwlock-held* ((o!place &optional (mode :read)) &body body)
  `(let ((,g!writer ,(eq mode :write)))
     (,(ecase mode
	 (:read 'lock-read)
	 (:write 'lock-write)) ,g!place)
     (macrolet ((upgrade ()
		  `(progn (lock-upgrade ,',g!place)
			  (setq ,',g!writer T)))
		(downgrade ()
		  `(progn (setq ,',g!writer NIL)
			  (lock-downgrade ,',g!place))))
       (unwind-protect (progn ,.body)
	 (if ,g!writer
	     (unlock-write ,g!place)
	     (unlock-read ,g!place))))))
