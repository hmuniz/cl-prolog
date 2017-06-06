(in-package #:cl-prolog)
;; pattern-matching code

(defconstant fail nil
  "Indicates pat-match failure")


(defconstant no-bindings '((t . t))
  "Indicates pat-match success, with no variables.")


(defun variable-p (x)
  "Is x a variable (a symbol beginning with '?')?"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))


(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))


(defun extend-bindings (var val bindings)
  "Add a (var .value) pair to a binding list."
  (cons (cons var val)
	;; Once we add a "real " binding,
	;; we can get rid of the dummy no-bindings
	(if (and (eq bindings no-bindings))
	    nil
	    bindings)))


(defun match-variable (var input bindings)
  "Does VAR match input? Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
	  ((equal input (binding-val binding)) bindings)
	  (t fail))))
