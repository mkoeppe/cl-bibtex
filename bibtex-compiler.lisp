;;; A BibTeX re-implementation in Common Lisp - the BST->CL compiler
;;; Copr. 2001, 2002 Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de>
;;; This is free software, licensed under GNU GPL (see file COPYING)

;; TODO:
;; * while$ must be extended to handle "tail recursions"
;; * order of side effects
;; * maybe beautify (if ... (progn ...) (progn ...)) ==> (cond ...)
;; * maybe special form for := that chooses = or string=
;; * propagate types when they get more specific:
;;   { $duplicate + } is of type (INTEGER) -> (INTEGER), not T -> (INTEGER)
;; * boolean/integer issues
;; * copy top-level comments...
;; * Handle the "tail-recursive" family of FORMAT.NAMES functions:
;;
;;FUNCTION {format.names}
;;{ 's :=
;;  #1 'nameptr :=
;;  s num.names$ 'numnames :=
;;  numnames 'namesleft :=
;;    { namesleft #0 > }
;;    { s nameptr "{ff~}{vv~}{ll}{, jj}" format.name$ 't :=
;;      nameptr #1 >
;;        { namesleft #1 >          ;; THEN function is (STRING) -> (STRING)
;;            { ", " * t * }
;;            { numnames #2 >
;;                { "," * }
;;                'skip$
;;              if$
;;              t "others" =
;;                { " et~al." * }
;;                { " and " * t * }
;;              if$
;;            }
;;          if$
;;        }
;;        't                         ;; ELSE function pushes the first name;
;;                                   ;; we get here with NAMEPTR=1
;;      if$
;;      nameptr #1 + 'nameptr :=
;;      namesleft #1 - 'namesleft :=
;;    }
;;  while$                          ;; WHILE BODY produces exactly one value
;;                                  ;; over all runs
;;}


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

;;; Capturing the computation in effect

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

;;; Packaging the computed data

(defun package-as-body ()
  "Build a Lisp body corresponding to the computation captured in
*FORM-BINDINGS* and *FORM-STACK*.  The Lisp body contains free
variables corresponding to *BORROWED-VARIABLES*.  Return four values:
BODY, ARGUMENT-TYPES, RESULT-TYPES, SIDE-EFFECTS-P."
  (let ((*form-stack* *form-stack*)
	(result-forms ())
	(result-types ()))
    (loop (multiple-value-bind (form type)
	      (pop-form t :when-empty nil) ; modifies the place *form-stack*
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
      (values body
	      (mapcar #'variable-type *borrowed-variables*)
	      result-types
	      nil))))

(defun package-as-form ()
  "Build a Lisp form corresponding to the computation captured in
*FORM-BINDINGS* and *FORM-STACK*.  The Lisp form contains free
variables corresponding to *BORROWED-VARIABLES*.  Return four values:
LISP-FORM, ARGUMENT-TYPES, RESULT-TYPES, SIDE-EFFECTS-P."
  (multiple-value-bind (body argument-types result-types side-effects-p)
      (package-as-body)
    (values (case (length body)
	      (0 `nil)
	      (1 (car body))
	      (t `(progn ,@body)))
	    argument-types result-types side-effects-p)))
 
(defun package-as-procedure (name)
  "Build a DEFUN NAME form from *FORM-BINDINGS*, *BORROWED-VARIABLES*
and *FORM-STACK*.  If NAME is nil, build a LAMBDA form instead.
Return four values: DEFUN-OR-LAMBDA-FORM, ARGUMENT-TYPES,
RESULT-TYPES, SIDE-EFFECTS-P."
  (multiple-value-bind (body argument-types result-types side-effects-p)
      (package-as-body)
    (values `(,@(if name `(defun ,name) `(lambda))
	      ,(mapcar #'variable-name *borrowed-variables*)
	      ,@body)
	    argument-types result-types side-effects-p)))

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

(defun bst-compile-literal (literal stack &key (borrowing-allowed t))
  "Compile a BST function LITERAL , which is a symbol, designating a
BST function, or a list (a function body).  Return four values: a Lisp
FORM, ARGUMENT-TYPES, RESULT-TYPES, and SIDE-EFFECTS-P."
  (let ((*form-stack* stack)
	(*borrowed-variables* ())
	(*form-bindings* ()))
    (etypecase literal
      (symbol (compile-funcall literal))
      (cons (compile-body literal)))
    (assert (or borrowing-allowed 
		(null *borrowed-variables*)))
    (package-as-form)))

(define-bst-special-form "if$"
    (let* ((else-literal (pop-literal))
	   (then-literal (pop-literal))
	   (val-form (pop-form '(boolean))))
      ;; First pass: compute the arity of both branches
      (multiple-value-bind (else-form else-arg-types else-res-types)
	  (bst-compile-literal else-literal ())
	;;(format t "~&;; else-form ~S is ~S --> ~S~%" else-literal else-arg-types else-res-types)
	(multiple-value-bind (then-form then-arg-types then-res-types)
	    (bst-compile-literal then-literal ())
	  ;;(format t "~&;; then-form ~S is ~S --> ~S~%" then-literal then-arg-types then-res-types)
	  ;; Now we know the arity of both branches.  We compute the
	  ;; arg types and fill up the shorter arg list.
	  (let ((arg-types ()))
	    (do ((then-arg-types then-arg-types (cdr then-arg-types))
		 (else-arg-types else-arg-types (cdr else-arg-types)))
		((and (null then-arg-types) (null else-arg-types))
		 (setq arg-types (nreverse arg-types)))
	      (push (type-intersection (if then-arg-types (car then-arg-types) t)
				       (if else-arg-types (car else-arg-types) t))
		    arg-types))
	    ;; Pop this many forms to build a temporary form stack
	    (let ((branch-stack
		   (nreverse (mapcar #'(lambda (req-type)
					 (multiple-value-bind (form type)
					     (pop-form req-type)
					   (make-mvform :form form
							:types (list type))))
				     (reverse arg-types)))))
	      ;;(format t "~%;; branch-stack-1: ~S~%" branch-stack)
	      (multiple-value-bind (else-op else-arg-types
					    else-res-types else-side-effects-p)
		  (bst-compile-literal else-literal branch-stack :borrowing-allowed nil)
		;;(format t "~%;; branch-stack-2: ~S~%" branch-stack)
		(multiple-value-bind (then-op then-arg-types
					      then-res-types then-side-effects-p)
		    (bst-compile-literal then-literal branch-stack :borrowing-allowed nil)
		  (unless (= (length then-res-types) (length else-res-types))
		    (bst-compile-error "THEN function ~S ~%== ~S and ELSE function ~S ~%== ~S deliver ~
different net number of values: ~%~A -> ~A vs. ~A"
				       then-literal then-op else-literal else-op
				       arg-types then-res-types else-res-types))
		  (let* ((res-types (mapcar #'type-union then-res-types else-res-types))
			 (side-effects-p (or then-side-effects-p else-side-effects-p)))
		    (push (make-mvform :form `(if ,val-form
					       ,then-op
					       ,else-op)
				       :types res-types
				       :side-effects-p side-effects-p)
			  *form-stack*))))))))))   

(define-bst-special-form "pop$"
    (pop-form t :need-variable t))

(define-bst-special-form "skip$"
    nil)

(define-bst-special-form "while$"
    (let* ((body-literal (pop-literal))
	   (pred-literal (pop-literal)))
      (multiple-value-bind (pred-form pred-arg-types
				      pred-res-types pred-side-effects-p)
	  (bst-compile-literal pred-literal ())
	(multiple-value-bind (body-form body-arg-types
					body-res-types body-side-effects-p)
	    (bst-compile-literal body-literal ())
	  (unless (null pred-arg-types)
	    (bst-compile-error "PREDICATE function ~S takes stack values: ~S"
			       pred-literal pred-arg-types))
	  (unless (and (= (length pred-res-types) 1)
		       (type= (car pred-res-types) '(boolean)))
	    (bst-compile-error "PREDICATE function ~S does not deliver exactly one boolean stack value: ~S"
			       pred-literal pred-res-types))
	  (unless (and (null body-arg-types) (null body-res-types))
	    (bst-compile-error "BODY function ~S takes or delivers stack values: ~S --> ~S"
			       body-literal body-arg-types body-res-types))
	  (push (make-mvform :form `(loop while ,pred-form ,body-form)
			     :types ()
			     :side-effects-p (or pred-side-effects-p body-side-effects-p))
		*form-stack*)))))    


;;;

(defun compile-funcall (function-name)
  (let ((special-form (gethash (string function-name) *bst-special-forms*)))
    (if special-form
	(funcall special-form)
	(let* ((bst-function (get-bst-function function-name))
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
	    (t				; normal function call
	     (push (make-mvform :form (cons (bst-function-lisp-name bst-function)
					    arg-forms)
				:types result-types)
		   *form-stack*)))))))

(defun compile-body (body)
  (dolist (form body)
    (cond
      ((numberp form)
       (push (make-mvform :form form :types '((integer))) *form-stack*))
      ((stringp form)
       (push (make-mvform :form form :types '((string))) *form-stack*))
      ((symbolp form)			;function call
       (compile-funcall form))
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
    (compile-body function-definition)
    (package-as-procedure name)))

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

