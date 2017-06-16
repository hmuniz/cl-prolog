;;;; cl-prolog.asd

(asdf:defsystem #:cl-prolog
  :description "This is a implametation of chapter 11 of Paradigms of Artificial Intelligence Programming"
  :author "Henrique Muniz"
  :license "Specify license here"
  :serial t
  :components ((:file "package")
               (:file "prolog")
	       (:file "unify")
	       (:file "pattern-matching")
	       (:file "unifier")))

