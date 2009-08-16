(in-package #:utils-frahm)

(defstruct (locked-deque (:constructor create-locked-deque))
  "A locked deque which allows access to both front and back.  Allows for
concurrent access using a lock and a condition."
  (deque (cons NIL NIL) :type cons :read-only T)
  (lock NIL :read-only T)
  (filled (bt:make-condition-variable) :read-only T))

(defun make-locked-deque (&optional name)
  (create-locked-deque :lock (bt:make-lock name)))

(defun locked-deque-emptyp (locked-deque)
  "Tests if the deque is empty.  Has to be called with lock held."
  (null (car (locked-deque-deque locked-deque))))

(defun %reset-deque (deque)
  (rplaca deque NIL)
  (rplacd deque NIL))

(defun %enqueue (locked-deque item)
  "Adds a item to the back of the deque.  Has to be called with lock held."
  (let ((cons (cons item NIL))
	(deque (locked-deque-deque locked-deque)))
    (if (null (car deque))
	(progn (rplaca deque cons)
	       (rplacd deque cons))
	(let ((last (cdr deque)))
	  (rplacd last cons)
	  (rplacd deque cons)))))

;;; eventually less efficient than the function above
;; (defun enqueue (locked-deque item)
;;   (let ((cons (cons item NIL))
;; 	(deque (locked-deque-deque locked-deque)))
;;     (ematch deque
;;       ((cons NIL _)
;;        (rplaca deque cons)
;;        (rplacd deque cons))
;;       ((cons _ last)
;;        (rplacd last cons)
;;        (rplacd deque cons)))))

(defun %dequeue (locked-deque)
  "Pops a item from the front of the deque.  Has to be called with lock held."
  (let ((deque (locked-deque-deque locked-deque)))
    (when (car deque)
      (let* ((first (car deque))
	     (next (cdr first)))
	(prog1 (car first)
	  (rplaca deque next)
	  (unless next
	    (rplacd deque next)))))))

(defun enqueue (locked-deque item)
  "Adds a item to the back of the deque and notifies waiting readers."
  (bt:with-lock-held ((locked-deque-lock locked-deque))
    (%enqueue locked-deque item)
    (bt:condition-notify (locked-deque-filled locked-deque))))

(defun dequeue (locked-deque)
  (bt:with-lock-held ((locked-deque-lock locked-deque))
    (%dequeue locked-deque)))

(defun dequeue-wait (locked-deque)
  (bt:with-lock-held ((locked-deque-lock locked-deque))
    (aif (%dequeue locked-deque)
	 it
	 (progn
	   (bt:condition-wait (locked-deque-filled locked-deque)
			      (locked-deque-lock locked-deque))
	   (%dequeue locked-deque)))))

(defun %dequeue-all (locked-deque)
  (let ((deque (locked-deque-deque locked-deque)))
    (prog1 (car deque)
      (%reset-deque deque))))

(defun dequeue-all (locked-deque)
  "Returns all items from the front of the deque and resets it."
  (bt:with-lock-held ((locked-deque-lock locked-deque))
    (%dequeue-all locked-deque)))

(defun dequeue-wait-all (locked-deque)
  (bt:with-lock-held ((locked-deque-lock locked-deque))
    (aif (%dequeue-all locked-deque)
	 it
	 (progn
	   (bt:condition-wait (locked-deque-filled locked-deque)
			      (locked-deque-lock locked-deque))
	   (%dequeue-all locked-deque)))))

(defun %dequeue-item (locked-deque item &optional (deque (locked-deque-deque locked-deque)))
  (let ((car (car deque)))
    (if (eq (cadr deque) item)
	(if (eq (caar deque) item)
	    (%reset-deque deque)
	    (let ((prelast (last car 2)))
	      (rplacd prelast NIL)
	      (rplacd deque prelast)))
	(rplaca deque (delete item car :test #'eq)))))

(defun %dequeue-if (locked-deque test)
  (let ((deque (locked-deque-deque locked-deque)))
    (dolist (item (car deque))
      (when (funcall test item)
	(%dequeue-item NIL item deque)
	(return item)))))

(defun dequeue-if (locked-deque test)
  (bt:with-lock-held ((locked-deque-lock locked-deque))
    (%dequeue-if locked-deque test)))

(defun dequeue-wait-if (locked-deque test)
  (bt:with-lock-held ((locked-deque-lock locked-deque))
    (loop
      (awhen (%dequeue-if locked-deque test)
	(return it))
      (bt:condition-wait (locked-deque-filled locked-deque)
			 (locked-deque-lock locked-deque)))))
