;; A BibTeX re-implementation in Common Lisp - BST functions
;; Copyright 2001, 2002 Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de>
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of version 2.1 of the GNU Lesser 
;; General Public License as published by the Free Software 
;; Foundation or any later version, as clarified by the preamble 
;; found in COPYING-preamble.txt. This preamble is in the style
;; of the Franz Inc. preamble at http://opensource.franz.com/preamble.html
;; with names and copyright holders altered accordingly.

(in-package bibtex-compiler)

;;; BST functions

(defclass side-effects ()
  ((side-effects-p :accessor side-effects-side-effects-p
		   :initarg :side-effects-p :initform nil)
   ;; strings designating BST functions, symbols designating Lisp
   ;; variables (including lexical BST variables)
   (used-variables :accessor side-effects-used-variables
		   :initarg :used-variables :initform ())
   (assigned-variables :accessor side-effects-assigned-variables
		       :initarg :assigned-variables :initform ())
   ;; unconditionally assigned variables are always assigned in the
   ;; form; in fact, they are assigned before they are referenced
   (unconditionally-assigned-variables :accessor side-effects-unconditionally-assigned-variables
				       :initarg :unconditionally-assigned-variables
				       :initform ())
   (variables-used-before-assigned :accessor side-effects-variables-used-before-assigned
				   :initarg :variables-used-before-assigned
				   :initform ()))
  (:documentation
   "A description of the side-effects of a computation"))

(defvar null-side-effects
  (make-instance 'side-effects))

(defstruct bst-function
  name
  type
  ignore-redefinition-p  
  lisp-name
  argument-types
  result-types
  ;; For use in the BST compiler:
  pop-form-args
  lisp-form-maker
  (side-effects null-side-effects)
  setter-form-maker
  defun-form
  lexical-p
  constant-p
  (num-assignments 0)
  assigned-value-form
  ;; For use in the BST interpreter:
  value					; value as a variable
  body
  setter)

(defvar *builtin-bst-functions* (make-hash-table :size 30 :test 'equalp))

(defun register-bst-primitive (bst-name argument-types result-types lisp-function &key (ignore-redefinition nil) (side-effects-p nil))
  (setf (gethash (string bst-name) *builtin-bst-functions*)
	(make-bst-function :name (string bst-name)
			   :type 'built-in
			   :lisp-name lisp-function
			   :argument-types argument-types
			   :result-types result-types
			   :side-effects (make-instance 'side-effects :side-effects-p side-effects-p)
			   :ignore-redefinition-p ignore-redefinition)))

(defmacro define-bst-primitive (bst-name arglist result-types
				&key interpreted compiled
				(side-effects-p nil)
				(ignore-redefinition-p nil))
  `(setf (gethash ,(string bst-name) *builtin-bst-functions*)
    (make-bst-function :name ,(string bst-name)
     :type 'built-in
     :argument-types ',(mapcar #'cadr arglist)
     :pop-form-args ',(mapcar #'cddr arglist)
     :result-types ',result-types
     ,@(if interpreted
	   `(:lisp-name 
	     #'(lambda ,(mapcar #'car arglist)
		 ,@(if (symbolp bst-name)
		       `((block ,bst-name 
			   ,interpreted))
		       (list interpreted))))
	   ())
     ,@(if compiled
	   `(:lisp-form-maker
	     #'(lambda ,(mapcar #'car arglist)
		 ,compiled))
	   ())
     :side-effects (make-instance 'side-effects :side-effects-p ,side-effects-p)
     :ignore-redefinition-p ,ignore-redefinition-p)))
			       
(defun register-bst-entry (entry func-type type default-value hash-table)
  (setq entry (string entry))
  (setf (gethash entry hash-table)
	(make-bst-function :name entry
			   :lisp-name #'(lambda ()
					  (gethash entry *bib-entry* default-value))
			   :lisp-form-maker #'(lambda ()
						`(gethash ,entry *bib-entry* ,default-value))
			   :setter #'(lambda (value) (setf (gethash entry *bib-entry*)
							   value))
			   :setter-form-maker #'(lambda (value-form)
						  `(setf (gethash ,entry *bib-entry*)
						    ,value-form))
			   :side-effects (make-instance 'side-effects :used-variables (list entry)
							:variables-used-before-assigned (list entry))
			   :type func-type
			   :argument-types '()
			   :result-types (list type))))

(defun register-bst-global-var (variable lisp-name func-type type
				initial-value hash-table
				&key (constant-p nil))
  (let ((variable (string variable)))
    (setf (gethash variable hash-table)
	  (make-bst-function :name variable
			     :lisp-name lisp-name
			     :setter #'(lambda (value)
					 (setf (bst-function-value
						(gethash variable hash-table))
					       value))
			     :lisp-form-maker #'(lambda () lisp-name)
			     :setter-form-maker #'(lambda (value-form)
						    `(setq ,lisp-name ,value-form))
			     :side-effects (make-instance 'side-effects :used-variables (list variable)
							  :variables-used-before-assigned (list variable))
			     :assigned-value-form initial-value
			     :constant-p constant-p
			     :type func-type
			     :argument-types '()
			     :result-types (list type)
			     :value initial-value))))

(defvar *bst-functions* nil)

(defun builtin-bst-functions ()
  (let ((table (make-hash-table :test 'equalp)))
    (loop for key being each hash-key of *builtin-bst-functions*
	  do (setf (gethash key table)
		   (gethash key *builtin-bst-functions*)))
    table))

(defvar *bst-package* nil
  "A temporary package where the BST compiler puts the symbols
generated for the BibTeX style file")

(defun bst-name-to-lisp-name (bst-name &optional (type :function))
  (setq bst-name (substitute #\- #\. (string-upcase (string bst-name))))
  (ecase type
    ((:function :lexical)
     (if (string-equal bst-name "T")
	 (gentemp "TEMP" *bst-package*)
	 (bst-intern bst-name)))
    ((:constant)
     (bst-intern (concatenate 'string "+" bst-name "+")))
    ((:variable)
     (bst-intern (concatenate 'string "*" bst-name "*")))))

(defun bst-intern (name)
  "Intern NAME into *BST-PACKAGE*, shadowing imported symbols."
  (shadow name *bst-package*)
  (intern name *bst-package*))

(defun check-for-already-defined-function (name)
  (unless (symbolp name)
    (error "~A is not a valid identifier" name))
  (let ((function (gethash (string name) *bst-functions*)))
    (when function
      (unless (bst-function-ignore-redefinition-p function)
	(error "~A is already a ~A function name"
	       name (bst-function-type function))))
    function))

(defun get-bst-function-of-type (name &optional (type-list t))
  "Check whether NAME is the name of a BST function, whose type is one
contained in TYPE-LIST.  If so, return that function.  Otherwise,
signal an error and don't return."
  (let ((function (gethash (string name) *bst-functions*)))
    (unless function
      (error "~A is an unknown function" name))
    (when (and (not (eql type-list t))
	       (not (member (bst-function-type function)
			    type-list)))
      (error "~A has bad function type" name))
    function))  

;;; Local Variables:
;;; eval: (put 'define-bst-primitive 'lisp-indent-function 3)
;;; End:
