(in-package #:utils-frahm)

(defmacro do-mapper (mapper (list &rest more-lists) &body body)
  "Runs MAPPER on all lists with BODY spliced into a LAMBDA function
argument for it.  It is intended to be used as an expansion in other
macros like DO-MAPCAR, which uses the standard MAPCAR function as the
MAPPER function.

Each LIST argument can be either a form like SYMBOL,
which is used verbatim to name the argument for the BODY and as list
argument for the MAPPER, or a form like (SYMBOL LIST), which uses SYMBOL
as the BODY argument and passes LIST to the MAPPER function."
  (let (syms args)
    (dolist (list (cons list more-lists))
      (etypecase list
	(list (push (first list) syms)
	      (push (second list) args))
	(symbol (push list syms)
		(push list args))))
    ;; if its a symbol, put it in function position, otherwise FUNCALL it
    `(,.(if (symbolp mapper)
	    (list mapper)
	    `(funcall ,mapper))
	(lambda (,.(nreverse syms)) ,.body) ,.(nreverse args))))

(defmacro do-mapcar (&whole whole (list &rest more-lists) &body body)
  "Runs MAPCAR with BODY wrapped as LAMBDA for the function argument."
  (declare (ignore list more-lists body))
  `(do-mapper mapcar ,.(cdr whole)))
