(in-package #:cl-prolog)


(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))


(defun subst-bindings (bindings x)
  "Substitute the value of variables i n bindings into x, taking recursively bound variables into account. "
  (cond ((eq bindings fail) fail)
	((eq bindings no-bindings) x)
	((and (variable-p x) (get-binding x bindings))
	 (subst-bindings bindings (lookup x bindings)))
	((atom x) x)
	(t (reuse-cons (subst-bindings bindings (car x))
		       (subst-bindings bindings (cdr x)) x))))

(defun unifier (x y)
  "Return something that unifies with both x and y (or fail)."
  (subst-bindings (unify x y) x))
