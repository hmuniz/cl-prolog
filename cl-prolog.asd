;;;; cl-prolog.asd

(asdf:defsystem #:cl-prolog
  :description "This is a implametation of chapter 11 of Paradigms of Artificial Intelligence Programming"
  :author "Henrique Muniz"
  :license "Specify license here"
  :serial t
  :components ((:file "package")
	       (:file "pattern-matching")
               (:file "b-prolog")
	       (:file "incremental-approach")   ;; This approach is from of chpater 11.3
	       ;(:file "batch-approach")        ;; This approach is from of chpater 11.2
	       (:file "unify")
	       (:file "unifier")
	       (:file "destructive-unification")
	       (:file "prolog-in-prolog")))


;; You can choose which approach you prefer to use. 
