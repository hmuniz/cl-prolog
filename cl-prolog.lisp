
;;;; cl-prolog.lisp

(in-package #:cl-prolog)

;;; "cl-prolog" goes here. Hacks and glory await!

;; Clauses are represented as (head . body) cons cells

(defun clause-head (clause)
  (car clause))

(defun clause-body (clause)
  (cdr clause))

;; Clauses are stored on the predicate's plist

(defun get-clauses (pred)
  (get pred 'clauses))

(defun predicate (relation)
  (first relation))

(defvar *db-predicates* nil
  "A list of all predicates stored in the database.")

(defmacro <- (&rest clause)
  "Add a clause to the data base."
   `(add-clause ',clause))

(defun add-clause (clause)
  "Add a clause to the data base, indexed by head's predicate."
  ;; The predicate must be a non-variable symbol.
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (setf (get pred 'clauses)
	  (nconc (get-clauses pred) (list clause)))
    pred))


(defun clear-db ()
  "Remove all clauses (for all predicates) from the data base."
  (mapc #'clear-predicate *db-predicates*))

(defun clear-predicate (predicate)
  "Remove the clauses for a single predicate."
  (setf (get predicate 'clauses) nil))
