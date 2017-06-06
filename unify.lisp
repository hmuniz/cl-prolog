(in-package #:cl-prolog)

(defun unify (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings."
  (cond ((eq bindings fail)   fail)
	((variable-p x) (unify-variable x y bindings))
	((variable-p y) (unify-variable y x bindings)) ;***
	((eql x y) bindings)
	((and (consp x) (consp y))
	 (unify (rest x) (rest y)
		(unify (first x) (first y) bindings)))
	(t fail)))

(defun unify-variable (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  ;; Warning - buggy version
  (if (get-binding var bindings)
      (unify (lookup var bindings) x bindings)
      (extend-bindings var x bindings)))
