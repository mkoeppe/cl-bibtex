;;; A BibTeX re-implementation in Common Lisp - the BST->CL compiler
;;; Copr. 2001, 2002 Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de>
;;; This is free software, licensed under GNU GPL (see file COPYING)

;; TODO:
;; * if$ needs to be extended to all cases where
;;   #THEN-ARGS - #THEN-VALUES = #ELSE-ARGS - #ELSE-VALUES. 
;; * while$

;; The type system.  NIL means no value fits, T means all values fit.
;; A list of symbols means values of all listed types fit.

(defun type-intersection (a b)
  (cond
    ((eql a t) b)
    ((eql b t) a)
    (t (intersection a b))))

(defun type-union (a b)
  (cond
    ((eql a t) t)
    ((eql b t) t)
    (t (union a b))))

(defun type<= (a b)
  (cond
    ((eql b t) t)
    ((eql a t) nil)	         ; this assumes that a list of type
					; symbols is never complete
    (t (every (lambda (x) (member x b)) a))))

(defun type= (a b)
  (and (type<= a b)
       (type<= b a)))

(defun null-type (a)
  (null a))

;; side effects and bindings

(defstruct variable
  "A typed variable"
  name
  type)

(defstruct binding
  "A multiple-value binding frame"
  variables
  form)

(defstruct mvform
  "A multiple-values-delivering form on the stack"
  form					; a Lisp form OR
  literal				; a BST symbol or function body
  types
  side-effects-p)

(defvar *form-bindings* ()
  "A list of BINDINGs that keeps track of all forms that cannot be
collected functionally because they are side-effecting or their values
are needed more than one time.")

(defvar *borrowed-variables* ()
  "The list of VARIABLES borrowed from the stack.")

(defvar *form-stack* ()
  "The list of MVFORMs delivering the values on the stack.")  

(defun map2 (procedure &rest lists)
  "Return two lists that are collecting the two values of PROCEDURE,
applied to the parallel elements in LISTS."
  (do ((result1 ())
       (result2 ())
       (lists lists (mapcar #'cdr lists)))
      ((some #'null lists)
       (values (nreverse result1) (nreverse result2)))
    (multiple-value-bind (r1 r2)
	(apply procedure (mapcar #'car lists))
      (setq result1 (cons r1 result1)
	    result2 (cons r2 result2)))))

;;(map2 #'round '(5 6 7) '(2 2 2))

(define-condition bst-compiler-error ()
  ((message :initarg :message :reader bst-compiler-error-message)))

(defun bst-compile-error (format-string &rest args)
  (error (make-condition 'bst-compiler-error :message (apply 'format nil format-string args))))

(defun pop-form (type &key (need-variable nil) (when-empty :borrow))
  "Pop a Lisp form delivering a single value of given TYPE from
*FORM-STACK*.  If the stack is empty, borrow a variable instead.
Return two values: the Lisp form and the actual type of the delivered
value."
  (loop (when (null *form-stack*)
	  (case when-empty
	    ((nil) (return-from pop-form nil))
	    (:borrow 
	     ;; Borrow a variable
	     (let ((arg-symbol (gentemp "ARG")))
	       (push (make-variable :name arg-symbol :type type)
		     *borrowed-variables*)
	       (return-from pop-form (values arg-symbol type))))))
	(let ((top-mvform (pop *form-stack*)))
	  (cond
	    ((mvform-literal top-mvform)
	     (bst-compile-error "Expecting a form on the stack, got a literal ~S."
				(mvform-literal top-mvform)))
	    ((or (symbolp (mvform-form top-mvform)) ; Variable, delivering one value
		 (and (not need-variable) ; other form delivering one value w/o side-effect
		      (not (mvform-side-effects-p top-mvform))
		      (= (length (mvform-types top-mvform)) 1)))
	     ;; Exactly one value, so return the form
	     (let* ((available-type (car (mvform-types top-mvform)))
		    (effective-type (type-intersection type available-type)))
	       (when (null-type effective-type)
		 (bst-compile-error "Type mismatch: expecting ~A, got ~A."
				    type available-type))
	       (return-from pop-form (values (mvform-form top-mvform)
					     effective-type))))
	    (t
	     ;; Zero or more than two values, so make a binding frame
	     ;; and push single-value forms referring to the bound
	     ;; variables.  Then continue.
	     (multiple-value-bind (variables mvforms)
		 (map2 (lambda (type)
			 (let ((symbol (gentemp)))
			   (values (make-variable :name symbol :type type)
				   (make-mvform :form symbol :types (list type)))))
		       (mvform-types top-mvform))
	       ;;(format t "variables: ~A~%mvforms: ~A~%" variables mvforms)
	       (push (make-binding :variables variables
				   :form (mvform-form top-mvform))
		     *form-bindings*)
	       (setq *form-stack* (nconc mvforms *form-stack*))))))))

(defun pop-literal ()
  "Pop a literal from *FORM-STACK*."
  (when (null *form-stack*)
    (bst-compile-error "Empty form/literal stack"))
  (let ((mvform (pop *form-stack*)))
    (unless (mvform-literal mvform)
      (bst-compile-error "Expecting a literal on the stack, found a form ~S"
			 mvform))
    (mvform-literal mvform)))
  
(defun build-procedure-form (name)
  "Build a DEFUN NAME form from *FORM-BINDINGS*, *BORROWED-VARIABLES*
and *FORM-STACK*.  If NAME is nil, build a LAMBDA form instead.
Return four values: DEFUN-OR-LAMBDA-FORM, ARGUMENT-TYPES,
RESULT-TYPES, SIDE-EFFECTS-P."
  (let ((result-forms ())
	(result-types ()))
    (loop (multiple-value-bind (form type)
	      (pop-form t :when-empty nil)
	    (cond
	      (form
	       (push form result-forms)
	       (push type result-types))
	      (t
	       (setq result-forms (nreverse result-forms)
		     result-types (nreverse result-types))
	       (return)))))
    (let ((body (case (length result-forms)
		  (0 ())
		  (1 (list (car result-forms)))
		  (t (list `(values ,@result-forms))))))
      (dolist (binding *form-bindings*)
	(case (length (binding-variables binding))
	  (0 (setq body (cons (binding-form binding)
			      body)))
	  (1 (setq body
		   (list `(let ((,(variable-name (car (binding-variables binding)))
				 ,(binding-form binding)))
			   ,@body))))
	  (t (setq body
		   (list `(multiple-value-bind
			   ,(mapcar #'variable-name (binding-variables binding))
			   ,(binding-form binding)
			   ,@body))))))
      (values `(,@(if name `(defun ,name) `(lambda))
		,(mapcar #'variable-name *borrowed-variables*)
		,@body)
	      (mapcar #'variable-type *borrowed-variables*)
	      result-types
	      nil))))
  
;;; BST "special forms"

(defvar *bst-special-forms* (make-hash-table :test 'equalp)
  "A hashtable, mapping BST function symbols to thunks that implement
special forms by directly manipulating the current compiler data.")

(defmacro define-bst-special-form (bst-name &body body)
  `(setf (gethash ,bst-name *bst-special-forms*)
	 (lambda ()
	   ,@body)))

(define-bst-special-form "duplicate$"
    (multiple-value-bind (form type)
	(pop-form t :need-variable t)
      (push (make-mvform :form form :types (list type)) *form-stack*)
      (push (make-mvform :form form :types (list type)) *form-stack*)))

(define-bst-special-form "swap$"
    (multiple-value-bind (form1 type1) (pop-form t :need-variable t)
      (multiple-value-bind (form2 type2) (pop-form t :need-variable t)
	(push (make-mvform :form form1 :types (list type1)) *form-stack*)
	(push (make-mvform :form form2 :types (list type2)) *form-stack*))))

(define-bst-special-form ":="
    (let ((var (pop-literal)))
      (multiple-value-bind (value-form value-type) (pop-form t)
	;;(format t "var: ~S value: ~S~%" var value-form)
	(let* ((bst-function (get-bst-function var))
	       (setter-form-maker (bst-function-setter-form-maker bst-function))
	       (setter-form (funcall setter-form-maker value-form)))
	  (push (make-mvform :form setter-form :types ()) *form-stack*)))))

(defun get-bst-function (name)
  (let ((function (gethash (string name) *bst-functions*)))
    (unless function
      (bst-compile-error "~A is an unknown function" name))
    function))

(defun literal-operator-form (literal-mvform)
  "Build a Lisp operator form that corresponds to the BST function
designated by LITERAL-MVFORM, whose LITERAL is a symbol, designating a
BST function, or a list (a function body).  Return four values: a Lisp
OPERATOR-FORM (a symbol or a lambda list), ARGUMENT-TYPES,
RESULT-TYPES, and SIDE-EFFECTS-P."
  (let ((literal (mvform-literal literal-mvform)))
    (etypecase literal
      (null
       (bst-compile-error "MVFORM ~S is neither a quoted function nor a function body"))
      (symbol
       (let ((bst-function (get-bst-function literal)))
	 (values (bst-function-lisp-name bst-function)
		 (bst-function-argument-types bst-function)
		 (bst-function-result-types bst-function)
		 (bst-function-side-effects-p bst-function))))
      (cons
       (bst-compile-defun nil literal)))))
  
(define-bst-special-form "if$"
    (let* ((else-form (pop *form-stack*))
	   (then-form (pop *form-stack*))
	   (val-form (pop-form '(boolean))))
      ;;(format *error-output* "~S ~S ~S~%" val-form then-form else-form)
      (multiple-value-bind (else-op else-arg-types
				    else-res-types else-side-effects-p)
	  (literal-operator-form else-form)
	(multiple-value-bind (then-op then-arg-types
				      then-res-types then-side-effects-p)
	    (literal-operator-form then-form)
	  ;; Now we know the arity of both branches.
	  (unless (= (length then-arg-types) (length else-arg-types))
	    (bst-compile-error "THEN function ~S and ELSE function ~S take ~
different number of arguments: ~A vs. ~A"
		   (mvform-literal then-form)
		   (mvform-literal else-form)
		   then-arg-types else-arg-types))
	  (unless (= (length then-res-types) (length else-res-types))
	    (bst-compile-error "THEN function ~S and ELSE function ~S deliver ~
different number of values: ~A vs. ~A"
		   (mvform-literal then-form)
		   (mvform-literal else-form)
		   then-res-types else-res-types))
	  (let* ((arg-types (mapcar #'type-intersection then-arg-types else-arg-types))
		 (res-types (mapcar #'type-union then-res-types else-res-types))
		 (side-effects-p (or then-side-effects-p else-side-effects-p))
		 (arg-forms (nreverse
			     (mapcar (lambda (type)
				       (pop-form type :need-variable t))
				     (reverse arg-types)))))
	    (push (make-mvform :form `(if ,val-form
				       (,then-op ,@arg-forms)
				       (,else-op ,@arg-forms))
			       :types res-types
			       :side-effects-p side-effects-p)
		  *form-stack*))))))   

(define-bst-special-form "pop$"
    (pop-form t :need-variable t))
    
;;;

(defun bst-compile-body (body)
  (dolist (form body)
    (cond
      ((numberp form)
       (push (make-mvform :form form :types '((integer))) *form-stack*))
      ((stringp form)
       (push (make-mvform :form form :types '((string))) *form-stack*))
      ((symbolp form)			;function call
       (let ((special-form (gethash (string form) *bst-special-forms*)))
	 (if special-form
	     (funcall special-form)
	     (let* ((bst-function (get-bst-function form))
		    (arg-types (bst-function-argument-types bst-function))
		    (arg-forms (nreverse
				(mapcar #'pop-form (reverse arg-types))))
		    (result-types (bst-function-result-types bst-function)))
	       (cond
		 ((bst-function-lisp-form-maker bst-function)
		  (push (make-mvform :form (apply (bst-function-lisp-form-maker bst-function)
						  arg-forms)
				     :types result-types)
			*form-stack*))
		 (t 			; normal function call
		  (push (make-mvform :form (cons (bst-function-lisp-name bst-function)
						 arg-forms)
				     :types result-types)
			*form-stack*)))))))
      ((and (consp form) (eql (car form) 'quote)) ; quoted function
       (push (make-mvform :literal (cadr form)
			  :types '((symbol)))
	     *form-stack*))
      ((consp form)			; function body
       (push (make-mvform :literal form
			  :types '((body)))
	     *form-stack*))
      (t (bst-compile-error "Illegal form in BST function body: ~S" form)))))

(defun bst-compile-defun (name function-definition)
  "Compile a BST wizard-defined function of given NAME and
FUNCTION-DEFINITION.  If NAME is nil, build a lambda expression,
rather than a defun form.  Return four values: DEFUN-OR-LAMBDA-FORM,
ARGUMENT-TYPES, RESULT-TYPES, SIDE-EFFECTS-P."
  (let ((*borrowed-variables* ())
	(*form-bindings* ())
	(*form-stack* ()))
    (bst-compile-body function-definition)
    (build-procedure-form name)))

;;;

#|
(let ((*borrowed-variables* (list (make-variable :name 'X)))
      (*form-bindings* (list (make-binding :variables (list (make-variable :name 'Y))
					   :form '(* X 3))))
      (*form-stack* (list (make-mvform :form '(+ 1 X Y)))))
  (pprint 
   (build-procedure-form 'FOO)))


(let (*form-stack* *form-bindings* *borrowed-variables*
		   (*bst-functions* (builtin-bst-functions)))
  (bst-compile-body '(1 duplicate$ 2 + -))
  (pprint (build-procedure-form 'FOO)))
|#

(defun compile-bst-file (bst-file lisp-file)
  (let ((*bib-macros* (make-hash-table))
	(*bst-compiling* t))
;    (with-open-file (*lisp-stream* lisp-file :direction :output)
    (let ((*lisp-stream* *standard-output*))
      (with-open-file (bst-stream bst-file)
	(get-bst-commands-and-process bst-stream)))))

#|
(compile-bst-file (kpathsea:find-file "amsalpha.bst")
		  "/tmp/compiled-bst.lisp")

|#

