(in-package #:cl-prolog)

;;Some people find the <- notation difficult to read. Define macros
;;rule and fact so that we can write:

;; (fact (likes Robin cats))
;; (rule (likes Sandy ?x) if (likes ?x cats))

(defmacro fact (&rest clause)
  `(if (not (null (cdr ',clause)))
      nil
      (add-clause ',clause)))

(defmacro rule (head if &rest body)
  `(add-clause (append (list ',head) ',body)))


; Tets

; (fact (likes Kim Robin))
; (fact (likes Sandy Lee))
; (fact (likes Sandy Kim))
; (fact (likes Robin cats))
; (rule (likes Sandy ?x) if  (likes ?x cats))
; (rule (likes Kim ?x) if  (likes ?x Lee) (likes ?x Kim))

