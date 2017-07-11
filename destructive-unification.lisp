(in-package #:cl-prolog)


(defconstant unbound "Unbound")

;; (defstruct var name (binding unbound))

(defun bound-p (var) (not (eq (var-binding var) unbound)))


(defmacro deref (exp)
  "Fol1ow pointers for bound variables."
  `(progn (loop while (and (var-p ,exp) (bound-p ,exp))
		do (setf .exp (var-binding .exp)))
    ,exp))


(defun unify! (x y)
  "Destructively unify two expressions"
  (cond ((eql (deref x) (deref y)) t)
	((var-p x) (set-binding! x y)) ((var-p y) (set-binding! y x))
	((and (consp x) (consp y ) )
	 (and (unify! (first x) (first y))
	      (unify! (rest x) (rest y))))
	(t nil)))


;; (defun set-binding! (var value)
;;   "Set var's binding t o value. Always succeeds (returns t )."
;;   (setf (var-binding var) value)
;;   t)


;; (defstruct (var (:print-function print-var))
;; name (binding unbound))


(defun print-var (var stream depth)
  (if (or (and (numberp *print-level*)
	       (>= depth *print-level*))
	  (var-p (deref var)))
      (format stream "?~a" (var-name var))
      (write var :stream stream)))


(defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))


(defun set-binding! (var value)
  "Set var's binding t o value, after saving the variable in the trail. Always returns t."
  (unless (eq var value)
    (vector-push-extend var *trail*)
    (setf (var-binding var) value)) t)


(defun undo-bindings! (old-trail)
  "Undo all bindings back to a given point in the trail."
  (loop until (= (fill-pointer *trail*) old-trail)
	do (setf (var-binding (vector-pop *trail*)) unbound)))


(defvar *var-counter* 0)


(defstruct (var (:constructor ? ())
		(:print-function print-var))
  (name (incf *var-counter*))
  (binding unbound))
