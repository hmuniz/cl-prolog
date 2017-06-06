(in-package #:cl-prolog)

;; First unify function:

(defun buged-unify-variable-1 (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  ;; Warning - buggy version
  (if (get-binding var bindings)
      (buged-unify-1 (lookup var bindings) x bindings)
      (extend-bindings var x bindings)))


(defun buged-unify-1 (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings."
  (cond ((eq bindings fail)   fail)
	((variable-p x) (buged-unify-variable-1 x y bindings))
	((variable-p y) (buged-unify-variable-1 y x bindings)) ;***
	((eql x y) bindings)
	((and (consp x) (consp y))
	 (buged-unify-1 (rest x) (rest y)
		(buged-unify-1 (first x) (first y) bindings)))
	(t fail)))


;; Tests of first unify defined:
;; (cl-prolog:buged-unify '(?x + 1) '(2 + ?y))
;; (cl-prolog:buged-unify '?x '?y)
;; (cl-prolog:buged-unify '(?x ?x) '(?y ?y))
;; (cl-prolog:buged-unify '(?x ?x ?x) '(?y ?y ?y))
;; The last one shows a bug of this implematation. 

;; Second unify function

(defun buged-unify-variable-2 (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  ;; Warning - buggy version
  (if (get-binding var bindings)
      (buged-unify-2 (lookup var bindings) x bindings)
      (extend-bindings var x bindings)))


(defun buged-unify-2 (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings."
  (cond ((eq bindings fail)   fail)
	((eql x y) bindings)
	((variable-p x) (buged-unify-variable-2 x y bindings))
	((variable-p y) (buged-unify-variable-2 y x bindings)) ;***
	((and (consp x) (consp y))
	 (buged-unify-2 (rest x) (rest y)
		(buged-unify-2 (first x) (first y) bindings)))
	(t fail)))


;; Tests of second unify defined:
;; (cl-prolog:buged-unify-2 '(?x ?x) '(?y ?y))
;; (cl-prolog:buged-unify-2 '(?x ?x ?x) '(?y ?y ?y))
;; (cl-prolog:buged-unify-2 '(?x ?y) '(?y ?x))
;; (cl-prolog:buged-unify-2y '(?x ?y a) '(?y ?x ?x))

;; Third unify function

(defun unify-variable-3 (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  (cond ((get-binding var bindings)
	 (unify-3 (lookup var bindings) x bindings))
	((and (variable-p x) (get-binding x bindings)) ;***
	 (unify-3 var (lookup x bindings) bindings))   ;***
	(t (extend-bindings var x bindings))))

(defun unify-3 (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings."
  (cond ((eq bindings fail) fail)
	((eql x y) bindings)	;*** moved this line
	((variable-p x) (unify-variable-3 x y bindings))
	((variable-p y) (unify-variable-3 y x bindings))
	((and (consp x) (consp y))
	 (unify-3 (rest x) (rest y)
		(unify-3 (first x) (first y) bindings)))
	(t fail)))

;; Fourfy unify function

(defparameter *occurs-check* t "Should we do the occurs check?")

(defun unify-variable (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  (cond ((get-binding var bindings)
	 (unify (lookup var bindings) x bindings))
	((and (variable-p x) (get-binding x bindings))
	 (unify var (lookup x bindings) bindings))
	((and *occurs-check* (occurs-check var x bindings)) fail) ;***
	(t (extend-bindings var x bindings))))

(defun unify (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings."
  (cond ((eq bindings fail) fail)
	((eql x y) bindings)
	((variable-p x) (unify-variable x y bindings))
	((variable-p y) (unify-variable y x bindings))
	((and (consp x) (consp y))
	 (unify (rest x) (rest y)
		(unify (first x) (first y) bindings)))
	(t fail)))
  
(defun occurs-check (var x bindings)
  "Does var occur anywhere 'inside x?"
  (cond ((eq var x) t)
	((and (variable-p x) (get-binding x bindings))
	 (occurs-check var (lookup x bindings) bindings))
	((consp x) (or (occurs-check var (first x) bindings)
		       (occurs-check var (rest x) bindings)))
	(t nil)))
