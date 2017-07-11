(in-package #:cl-prolog)

;(<- (prove ?goal) (prove-all (?goal)))

;(<- (prove-a11 nil))

;; (<- (prove-all (?goal . ?goals))
;;     (clause (<- ?goal . ?body))
;;     (concat ?body ?goals ?new-goals)
;;     (prove-all ?new-goals))

;; define the member relation

;(<- (clause (<- (mem ?x (?x . ?y)))))
;(<- (clause (<- (mem ?x (? . ?z)) (mem ?x ?z))))

;; test

;(?- (prove (mem ?x (1 2 3))))

