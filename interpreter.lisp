;; A BibTeX re-implementation in Common Lisp - the BST interpreter
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

;;; The BST interpreter

(defvar *literal-stack* nil
  "The stack of BibTeX literals during interpretation of a BST
program.")

(defvar *show-stack* nil)

(defun bst-execute-body (body)
  (dolist (form body)
    (when *show-stack*
      (format t "Stack: ~S Form: ~S~%" *literal-stack* form))
    (cond
      ((or (numberp form) (stringp form)) (push form *literal-stack*))
      ((and (consp form) (eql (car form) 'quote))
       (push (cadr form) *literal-stack*))
      ((listp form) (push form *literal-stack*))
      ((symbolp form) (bst-execute (get-bst-function-of-type form)))
      (t (error "Illegal form in BST function body: ~S" form)))))

(defun bst-execute-stack-literal (literal)
  (cond
    ((listp literal) (bst-execute-body literal))
    ((symbolp literal) (bst-execute (get-bst-function-of-type literal)))
    (t (error "Stack literal ~S is not a function" literal))))

#|(defun stack-literal-type (literal)
  (typecase literal
    (integer '(integer boolean))
    (string '(string))
    (symbol
     (cond ((null literal) '(missing))
	   (t '(symbol))))
    (cons '(body))
    (otherwise (error "Bad stack literal: ~S" literal))))|#

(defun bst-pop/coerce (type)
  (when (null *literal-stack*)
    (error "Literal stack empty"))
  (let ((literal (pop *literal-stack*)))
    (flet ((type-error ()
	     (error "Bad stack literal: ~S, expected type ~S"
		    literal type)))
      (typecase literal
	(integer
	 (cond ((equal type '(boolean))
		(return-from bst-pop/coerce (> literal 0)))
	       ((eql type t))
	       ((member 'integer type))
	       (t (type-error))))
	(string
	 (unless (or (eql type t)
		     (member 'string type))
	   (type-error)))
	(null	       ;ambiguous: "missing" or an empty function body
	 (unless (or (eql type t)
		     (member 'missing type)
		     (member 'body type))
	   (type-error)))
	(symbol
	 (unless (or (eql type t)
		     (member 'symbol type))
	   (type-error)))
	(cons
	 (unless (or (eql type t)
		     (member 'body type))
	   (type-error)))
	(otherwise (type-error))))
    literal))

(defun bst-coerce/push (value type)
  (flet ((type-error ()
	   (error "Bad value to push: ~S, expected type ~S"
		  value type)))
    (if (equal type '(boolean))
	(setf value (if value 1 0))
	(typecase value
	  (integer
	   (cond ((eql type t))
		 ((member 'integer type))
		 (t (type-error))))
	  (string
	   (unless (or (eql type t)
		       (member 'string type))
	     (type-error)))
	  (null	; ambiguous; could be "missing" or an empty function body
	   (unless (or (eql type t)
		       (member 'missing type)
		       (member 'body type))
	     (type-error)))
	  (symbol
	   (unless (or (eql type t)
		       (member 'symbol type))
	     (type-error)))
	  (cons
	   (unless (or (eql type t)
		       (member 'body type))
	     (type-error)))
	  (otherwise (type-error))))
    (push value *literal-stack*)))  

(defun bst-execute-stack-literal/pop (literal type)
  (bst-execute-stack-literal literal)
  (bst-pop/coerce type))

(defun bst-execute (bst-function)
  (cond
    ((bst-function-value bst-function)	; is a variable
     (push (bst-function-value bst-function) *literal-stack*))
    ((bst-function-body bst-function)	; is a wiz-defined function
     (bst-execute-body (bst-function-body bst-function)))
    ((bst-function-lisp-name bst-function) ; is a primitive, so call it
     (let* ((args (nreverse (mapcar #'bst-pop/coerce
				    (reverse (bst-function-argument-types bst-function)))))
	    (results
	     (multiple-value-list
	      (apply (bst-function-lisp-name bst-function) args))))
       (dolist (type (bst-function-result-types bst-function))
	 (bst-coerce/push (pop results) type))))
    (t (error "Don't know how to execute the function ~S" bst-function))))
    
