;;; A BibTeX re-implementation in Common Lisp - the BST->CL compiler
;;; Copr. 2001, 2002 Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de>
;;; This is free software, licensed under GNU GPL (see file COPYING)

(in-package bibtex-compiler)

(defvar *bst-compiling* nil
  "Non-nil if we are compiling a Common Lisp program from the BST
program, rather than interpreting the BST program.")

(defvar *bst-definition-sequence* '()
  "A list of strings (comments) and BST-FUNCTION structures.  It
records the definition order in the BST file.")

(defvar *main-lisp-body* ()
  "A list collecting the forms corresponding to EXECUTE, ITERATE,
READ, REVERSE, and SORT commands in reverse order.")

(defvar *bst-function-call-sequence* '()
  "A list of BST-FUNCTION structures.  It records the call order in
the BST file in reverse order.")

(defvar *bib-entries-symbol* nil
  "The name of the lexical variable in *main-lisp-body* that stores
the bib entries.")

(defvar *relaxed-compilation* t
  "If non-nil, try to compile the BST program even if it is slightly
broken.  In an IF$ with bad stack use, fill up one of the branches and
signal a runtime error from there.")

(defvar *silent-compilation* nil
  "If non-nil, don't issue compiler warnings.")

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

(defun type-union* (a b)
  (cond
    ((or (and (equal a '(integer))
	      (equal b '(boolean)))
	 (and (equal a '(boolean))
	      (equal b '(integer)))) '(integer))
    (t (type-union a b))))     

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
  "A typed Lisp variable"
  name
  type)

(defstruct binding
  "A multiple-value binding frame"
  variables
  mvform)

(defstruct mvform
  "A multiple-values-delivering form on the stack"
  literal-p
  form					; a Lisp form OR
  literal				; a BST symbol or function body
  types
  (side-effects null-side-effects))

(defvar *form-bindings* ()
  "A list of BINDINGs that keeps track of all forms that cannot be
collected functionally because they are side-effecting or their values
are needed more than one time.")

(defvar *borrowed-variables* ()
  "The list of VARIABLEs borrowed from the stack.")

(defvar *form-stack* ()
  "The list of MVFORMs delivering the values on the stack.")  

(defvar *lexicals* ()
  "A hack until detection of lexicals works.")

(defvar *lexical-variables* ()
  "An alist mapping strings designating BST variables to Lisp VARIABLEs.")  

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

(define-condition bst-compiler-warning ()
  ((message :initarg :message :reader bst-compiler-error-message))
  (:report (lambda (condition stream)
	     (princ (bst-compiler-error-message condition)
		    stream))))  

(define-condition bst-compiler-error (bst-compiler-warning) ())

(defvar *currently-compiled-function* nil
  "Only used for reporting errors.")

(defvar *currently-compiled-body* nil
  "Only used for reporting errors.")

(defvar *currently-compiled-body-rest* nil
  "Only used for reporting errors.")

(defun currently-compiled-body-with-markup ()
  (do ((body *currently-compiled-body* (cdr body))
       (front ()))
      ((or (null body)
	   (eq body *currently-compiled-body-rest*))
       (nconc (nreverse front) (cons '====> body)))
    (setq front (cons (car body) front))))
    

(defun bst-compile-warning (format-string &rest args)
  (unless *silent-compilation*
    (signal (make-condition 'bst-compiler-warning
			    :message
			    (concatenate 'string
					 (format nil " In BST body ~S:~%  "
						 (currently-compiled-body-with-markup))
					 (apply 'format nil format-string args))))))

(defun bst-compile-error (format-string &rest args)
  (error (make-condition 'bst-compiler-error
			 :message
			 (concatenate 'string
				      (format nil " In BST body ~S:~%  "
					      (currently-compiled-body-with-markup))
				      (apply 'format nil format-string args)))))

;;; Side effects

(defun any-side-effects-p (side-effects)
  (or (side-effects-side-effects-p side-effects)
      (not (null (side-effects-assigned-variables side-effects)))))

(defun max-side-effects (&rest effectss)
  "Compute the maximum of the side-effects EFFECTSS."
  (labels
      ((max-sfx (a b)
	 (make-instance 'side-effects
			:side-effects-p
			(or (side-effects-side-effects-p a)
			    (side-effects-side-effects-p b))
			:used-variables
			(union (side-effects-used-variables a)
			       (side-effects-used-variables b)
			       :test 'equalp)
			:assigned-variables
			(union (side-effects-assigned-variables a)
			       (side-effects-assigned-variables b)
			       :test 'equalp)
			:unconditionally-assigned-variables
			;; side effects of A come first, so if we
			;; have a reference to X in A and X is
			;; unconditionally assigned in B, then X
			;; is not unconditionally assigned in "A,
			;; B".
			(union (side-effects-unconditionally-assigned-variables a)
			       (set-difference
				(side-effects-unconditionally-assigned-variables b)
				(side-effects-used-variables a)
				:test 'equalp)
			       :test 'equalp)
			:variables-used-before-assigned
			(union (side-effects-variables-used-before-assigned a)
			       (set-difference
				(side-effects-variables-used-before-assigned b)
				(side-effects-unconditionally-assigned-variables a)
				:test 'equalp)
			       :test 'equalp))))
    (reduce #'max-sfx effectss :initial-value null-side-effects)))

(defun remove-variables-from-side-effects (variables side-effects)
  "VARIABLES is a list of strings or symbols to be removed from any
mention in SIDE-EFFECTS.  Return the resulting side effects."
  (make-instance 'side-effects
		 :side-effects-p (side-effects-side-effects-p side-effects)
		 :assigned-variables
		 (set-difference (side-effects-assigned-variables side-effects)
				 variables
				 :test 'equalp)
		 :unconditionally-assigned-variables
		 (set-difference (side-effects-unconditionally-assigned-variables side-effects)
				 variables
				 :test 'equalp)
		 :used-variables
		 (set-difference (side-effects-used-variables side-effects)
				 variables
				 :test 'equalp)
		 :variables-used-before-assigned
		 (set-difference (side-effects-variables-used-before-assigned side-effects)
				 variables
				 :test 'equalp)))

(defvar *bst-gentemp-counter* 0)

(defun bst-gentemp (prefix)
  (bst-intern (format nil "~A~A" prefix (incf *bst-gentemp-counter*))))  

(defun make-binding-and-push-variables (mvform)
  "Make a binding for all values delivered by MVFORM and push the
bound variables onto *FORM-STACK*."
  (multiple-value-bind (variables mvforms)
      (map2 (lambda (type)
	      (let ((symbol (bst-gentemp "T")))
		(values (make-variable :name symbol :type type)
			(make-mvform :form symbol :types (list type)))))
	    (mvform-types mvform))
    ;;(format t "variables: ~A~%mvforms: ~A~%" variables mvforms)
    (push (make-binding :variables variables
			:mvform mvform)
	  *form-bindings*)
    (setq *form-stack* (nconc (nreverse mvforms)
			      *form-stack*))))

(defun coerce-form (lisp-form available-type requested-type)
  "Return a Lisp form that computes the coercion of LISP-FORM from
AVAILABLE-TYPE to REQUESTED-TYPE.  As a secondary value, return the
effective type."
  (let* ((effective-type (type-intersection requested-type available-type)))
    ;; The handling of the special case boolean/integer is only a hack.
    ;; The type system should be improved instead.
    (cond
      ((and (type= available-type '(integer))
	    (type= requested-type '(boolean)))
       (values `(> ,lisp-form 0) '(boolean)))
      ((and (type= available-type '(boolean))
	    (type= requested-type '(integer)))
       (values (case lisp-form
		 ((nil) 0)
		 ((t)   1)
		 (t `(if ,lisp-form 1 0)))
	       '(integer)))
      ((null-type effective-type)
       (bst-compile-error "Type mismatch: expecting ~A, got ~A."
			  requested-type available-type))
      (t
       (values lisp-form effective-type)))))


(defun pop-form (type &key (need-variable nil) (when-empty :borrow)
		 (assigned-variables ()))
  "Pop a Lisp form delivering a single value of given TYPE from
*FORM-STACK*.  If the stack is empty, borrow a variable instead if
:WHEN-EMPTY is :BORROW, or return nil,nil,nil,t if :WHEN-EMPTY is nil.  If
:NEED-VARIABLE is nil, POP-FORM may return a side-effecting
single-value form \(which should only be called once, in order).  If
:NEED-VARIABLE is :IF-SIDE-EFFECTS, POP-FORM will introduce a variable
for side-effecting forms.  Otherwise, POP-FORM will introduce a
variable for all non-atom forms.  A variable will also be introduced
if the form uses one of the variables in the list :ASSIGNED-VARIABLES.
Return four values: the Lisp form,
the actual type of the delivered value, SIDE-EFFECTS, and EMPTY-P."
  (loop (when (null *form-stack*)
	  (ecase when-empty
	    ((nil) (return-from pop-form (values nil nil nil t)))
	    (:borrow 
	     ;; Borrow a variable
	     (let ((arg-symbol (bst-gentemp "ARG")))
	       (push (make-variable :name arg-symbol :type type)
		     *borrowed-variables*)
	       (return-from pop-form (values arg-symbol type null-side-effects))))))
	(let ((top-mvform (pop *form-stack*)))
	  (labels ((return-the-form ()
		     (let* ((available-type (car (mvform-types top-mvform)))
			    (lisp-form (mvform-form top-mvform))
			    (side-effects (mvform-side-effects top-mvform)))
		     (multiple-value-bind (coerced-form effective-type)
			 (coerce-form lisp-form available-type type)
		       (return-from pop-form
			 (values coerced-form effective-type side-effects))))))
	    (cond
	      ((not (null (intersection assigned-variables
					(side-effects-used-variables
					 (mvform-side-effects top-mvform))
					:test 'equalp)))
	       ;; Must make a binding because a used variable is
	       ;; affected by an assignment.
	       (make-binding-and-push-variables top-mvform))
	      ((mvform-literal top-mvform)
	       (bst-compile-error "Expecting a form on the stack, got a literal ~S."
				  (mvform-literal top-mvform)))
	      ((not (consp (mvform-form top-mvform))) ; Variable, string, or number, delivering one value
	       (return-the-form))
	      ((and (eql need-variable :if-side-effects)
		    (not (any-side-effects-p (mvform-side-effects top-mvform)))
		    (= (length (mvform-types top-mvform)) 1))
	       (return-the-form))
	      ((and (not need-variable)
		    (= (length (mvform-types top-mvform)) 1))
	       (return-the-form))
	      (t
	      ;; Zero or more than two values, so make a binding frame
	       ;; and push single-value forms referring to the bound
	       ;; variables.  Then continue.
	       (make-binding-and-push-variables top-mvform)))))))

(defun pop-single-value-form (&rest args)
  "Like pop-form, but package the return values in a MVFORM object."
  (multiple-value-bind (form type side-effects empty-p)
      (apply 'pop-form args)
    (if empty-p
	nil
	(make-mvform :form form :types (list type)
		     :side-effects side-effects))))

(defun push-form (mvform)
  "Push MVFORM onto *FORM-STACK*."
  ;; We first check if the last form has a side-effect.  If so, we
  ;; must turn it into a binding, or the order of side-effects will be
  ;; wrong.
  (when (and (not (null *form-stack*))
	     (any-side-effects-p (mvform-side-effects (car *form-stack*))))
    (let ((se-form (pop *form-stack*)))
      (make-binding-and-push-variables se-form)))
  ;; At this point everything on the stack is purely functional, so we
  ;; don't have to care about the order.
  (let ((ass-vars (side-effects-assigned-variables (mvform-side-effects mvform))))
    (when ass-vars
      ;; If the form to be pushed makes an assignment, it can render
      ;; even purely functional forms wrong if the order isn't fine.
      ;; Hence, convert every form on the value stack that uses the
      ;; assigned-to variables to bindings before proceeding.
      (loop for form = (pop-single-value-form t :need-variable nil
					      :assigned-variables ass-vars :when-empty nil)
	    while form
	    collect form into clean-form-stack
	    finally (setq *form-stack* clean-form-stack)))
    (push mvform *form-stack*)))

(defun push-mvform (&rest args)
  (push-form (apply 'make-mvform args)))

(defun pop-literal ()
  "Pop a literal from *FORM-STACK*."
  (when (null *form-stack*)
    (bst-compile-error "Empty form/literal stack"))
  (let ((mvform (pop *form-stack*)))
    (unless (mvform-literal-p mvform)
      (bst-compile-error "Expecting a literal on the stack, found a form ~S"
			 mvform))
    (mvform-literal mvform)))

;;; Packaging the computed data

(defun set-union (&rest lists)
  (reduce (lambda (a b)
	    (union a b :test 'equalp))
	  lists
	  :initial-value ()))

(defun package-as-body (&key (expected-result-types nil expected-result-types-p))
  "Build a Lisp body corresponding to the computation captured in
*FORM-BINDINGS* and *FORM-STACK*.  The Lisp body contains free
variables corresponding to *BORROWED-VARIABLES*.  Return five values:
BODY, ARGUMENT-TYPES, RESULT-TYPES, SIDE-EFFECTS, and
FREE-VARIABLES."
  (let ((*form-stack* *form-stack*)
	(*form-bindings* *form-bindings*)
	(result-mvforms ()))
    (loop as expected-result-type = (if expected-result-types-p
					(pop expected-result-types)
					t)
	  as mvform = (pop-single-value-form expected-result-type
					     :need-variable nil :when-empty nil)
					; modifies the place *form-stack*
	  while mvform
	  do (push mvform result-mvforms))
    (when expected-result-types-p
      (assert (null expected-result-types)))
    (let ((body (build-values-body (mapcar #'mvform-form result-mvforms)))
	  (side-effects (apply #'max-side-effects (mapcar #'mvform-side-effects result-mvforms)))
	  (let-bindings ()))
      (labels ((make-let* ()
		 (when (not (null let-bindings))
		   (cond
		     ((and (= (length body) 1) (consp (car body)) (eql (caar body) 'do))
	     ;; body is a single DO form, so we put our bindings there
		      (destructuring-bind (do do-bindings &rest do-body)
			  (car body)
			(declare (ignore do))
			(setq body (list `(do* ,(nconc let-bindings do-bindings)
					   ,@do-body))
			      let-bindings ())))
		     (t 
		      (case (length let-bindings)
			(0 nil)
			(1 (setq body (list `(let ,let-bindings
					      ,@body))
				 let-bindings ()))
			(t (setq body (list `(let* ,let-bindings
					      ,@body))
				 let-bindings ()))))))))
	(dolist (binding *form-bindings*)
	  ;; don't keep track of references and assignments to lexical
	  ;; variables outside their scope:
	  (setq side-effects
		(remove-variables-from-side-effects
		 (mapcar #'variable-name (binding-variables binding))
		 (max-side-effects (mvform-side-effects (binding-mvform binding)) side-effects)))
	  (case (length (binding-variables binding))
	    (0 (make-let*)
	       (setq body (cons (mvform-form (binding-mvform binding))
				body)))
	    (1 (let ((form (mvform-form (binding-mvform binding))))
		 (push (if (equal '(values) form)
			   ;; Lexical binding without useful values
			   (variable-name (car (binding-variables binding)))
			   ;; Lexical binding with useful values
			   `(,(variable-name (car (binding-variables binding)))
			     ,(mvform-form (binding-mvform binding))))
		       let-bindings)))
	    (t (make-let*)
	       (setq body
		     (list `(multiple-value-bind
			     ,(mapcar #'variable-name (binding-variables binding))
			     ,(mvform-form (binding-mvform binding))
			     ,@body))))))
	(make-let*)
	(values body
		(mapcar #'variable-type *borrowed-variables*)
		(mapcan #'mvform-types result-mvforms)
		side-effects
		(mapcar #'variable-name *borrowed-variables*))))))

(defun package-as-form (&rest args)
  "Build a Lisp form corresponding to the computation captured in
*FORM-BINDINGS* and *FORM-STACK*.  The Lisp form contains free
variables corresponding to *BORROWED-VARIABLES*.  Return five values:
LISP-FORM, ARGUMENT-TYPES, RESULT-TYPES, SIDE-EFFECTS, and FREE-VARIABLES."
  (multiple-value-bind (body argument-types result-types
			     side-effects free-variables)
      (apply #'package-as-body args)
    (values (build-progn-form body)
	    argument-types result-types side-effects
	    free-variables)))
 
(defun package-as-procedure (name &rest args)
  "Build a DEFUN NAME form from *FORM-BINDINGS*, *BORROWED-VARIABLES*
and *FORM-STACK*.  If NAME is nil, build a LAMBDA form instead.
Return four values: DEFUN-OR-LAMBDA-FORM, ARGUMENT-TYPES,
RESULT-TYPES, SIDE-EFFECTS."
  (multiple-value-bind (body argument-types result-types
			     side-effects free-variables)
      (apply #'package-as-body args)
    (values `(,@(if name `(defun ,name) `(lambda))
	      ,free-variables
	      ,@body)
	    argument-types result-types
	    (remove-variables-from-side-effects free-variables side-effects))))

(defun show-state ()
  (format t "~&;; *form-bindings*: ~S~%;; *form-stack*: ~S~%;; *borrowed-variables*: ~S~%;; procedure: ~:W~%"
	  *form-bindings*
	  *form-stack*
	  *borrowed-variables*
	  (package-as-procedure nil)))

;;; BST "special forms"

(defvar *bst-special-forms* (make-hash-table :test 'equalp)
  "A hashtable, mapping BST function symbols to thunks that implement
special forms by directly manipulating the current compiler data.")

(defmacro define-bst-special-form (bst-name &body body)
  `(setf (gethash ,bst-name *bst-special-forms*)
	 (lambda ()
	   ,@body)))

(define-bst-special-form "duplicate$"
    (let ((mvform (pop-single-value-form t :need-variable t)))
      (push-form mvform)
      (push-form mvform)))

(define-bst-special-form "swap$"
    (let* ((mvform-1 (pop-single-value-form t :need-variable :if-side-effects))
	   (mvform-2 (pop-single-value-form t :need-variable :if-side-effects)))
      (push-form mvform-1)
      (push-form mvform-2)))

(define-bst-special-form "="
    (multiple-value-bind (form-1 type-1 side-effects-1)
	(pop-form t :need-variable nil)
      (multiple-value-bind (form-2 type-2 side-effects-2)
	  (pop-form type-1 :need-variable nil)
	(let ((form (cond
		      ((type= type-2 '(boolean))
		       `(eql ,form-1 ,form-2)) ;FIXME: wrong
		      ((type= type-2 '(integer))
		       `(= ,form-1 ,form-2))
		      ((type= type-2 '(string))
		       `(string= ,form-1 ,form-2))
		      (t
		       `(equal ,form-1 ,form-2)))))
	  (push-mvform :form form
		       :types (list '(boolean))
		       :side-effects
		       (max-side-effects side-effects-1
					 side-effects-2))))))

(define-bst-special-form ":="
    (let ((var (pop-literal)))
      ;;(format t "var: ~S value: ~S~%" var value-form)
      (let* ((fun (get-bst-function var))
	     (name (bst-function-name fun))
	     type 
	     assoc)
	(unless (member (bst-function-type fun) '(int-global-var str-global-var
						  int-entry-var str-entry-var))
	  (bst-compile-error "Bad function to assign to: ~A is a ~A"
			     var (bst-function-type fun)))
	(setq type (car (bst-function-result-types fun)))
	(labels ((compute-side-effects (assigned-thing value-mvform)
		   (max-side-effects
		    (mvform-side-effects value-mvform)
		    (make-instance 'side-effects
				   :assigned-variables (list assigned-thing)
				   :unconditionally-assigned-variables (list assigned-thing)))))
	  (cond
	    ((bst-function-constant-p fun)
	     ;; do nothing
	     (pop-single-value-form type :need-variable :if-side-effects))
	    ((setq assoc (assoc name *lexical-variables* :test 'string-equal))
	     (let ((var-name (variable-name (cdr assoc)))
		   (value-mvform (pop-single-value-form type :need-variable nil)))
	       (push-mvform :form `(setq ,var-name
				    ,(mvform-form value-mvform))
			    :types ()
			    :side-effects (compute-side-effects var-name value-mvform))))
	    ((member (bst-function-name fun) *lexicals* :test 'string-equal)
	     (let ((var (make-variable :name (bst-name-to-lisp-name name)
				       :type (car (bst-function-result-types fun))))
		   (value-mvform (pop-single-value-form type :need-variable nil :when-empty nil)))
	       (if (not value-mvform)
		   ;; We have an assignment of a freshly popped formal
		   ;; argument to a lexical variable.  So simply use
		   ;; the lexical variable as the formal argument.
		   (push var *borrowed-variables*)
		   ;; Make a lexical binding.
		   (push (make-binding :variables (list var)
				       :mvform value-mvform) *form-bindings*))
	       (push (cons name var) *lexical-variables*)))
	    (t
	     (let* ((setter-form-maker (bst-function-setter-form-maker fun))
		    (value-mvform (pop-single-value-form type :need-variable nil))
		    (setter-form (funcall setter-form-maker (mvform-form value-mvform))))
	       (incf (bst-function-num-assignments fun))
	       (setf (bst-function-assigned-value-form fun) (mvform-form value-mvform))
	       (push-mvform
		:form setter-form :types ()
		:side-effects (compute-side-effects name value-mvform)))))))))

(defun get-bst-function (name)
  (let ((function (gethash (string name) *bst-functions*)))
    (unless function
      (bst-compile-error "~A is an unknown function" name))
    (when (eql (bst-function-type function) 'wiz-defined)
      (bst-compile-error "~A is a function that could not be compiled" name))
    function))

(defun compile-literal (literal)
  "Compile a BST function LITERAL, which is a symbol, designating a
BST function, or a list (a function body).  Record the computation in
the usual special variables."
  (etypecase literal
    (list (compile-body literal))
    (symbol (compile-funcall literal))))

(defun bst-compile-literal (literal stack &rest args &key (borrowing-allowed t) &allow-other-keys)
  "Compile a BST function LITERAL, which is a symbol, designating a
BST function, or a list (a function body).  Return five values: a Lisp
FORM, ARGUMENT-TYPES, RESULT-TYPES, SIDE-EFFECTS-P, and FREE-VARIABLES."
  (remf args :borrowing-allowed)
  (let ((*form-stack* stack)
	(*borrowed-variables* ())
	(*form-bindings* ()))
    (compile-literal literal)
    (assert (or borrowing-allowed 
		(null *borrowed-variables*)))
    (apply #'package-as-form args)))

(defvar *compiling-while-body* nil
  "True if compiling the body of a while$ function.")

(define-bst-special-form "if$"
    (let* ((else-literal (pop-literal))
	   (then-literal (pop-literal))
	   (val-mvform (pop-single-value-form '(boolean) :need-variable :if-side-effects)))
      ;; Side effects matter because our Lisp code beautifier reorders
      ;; the tested conditions to its liking.

      ;; First pass: compute the arity of both branches
      (multiple-value-bind (else-form else-arg-types else-res-types else-side-effects)
	  (let ((*lexicals* ())	; don't introduce local lexical bindings
		(*silent-compilation* t))
	    (bst-compile-literal else-literal ()))
	;;(format t "~&;; else-form ~S is ~S --> ~S~%" else-literal else-arg-types else-res-types)
	(multiple-value-bind (then-form then-arg-types then-res-types then-side-effects)
	    (let ((*lexicals* ()) ; don't introduce local lexical bindings
		  (*silent-compilation* t))
	      (bst-compile-literal then-literal ()))
	  ;;(format t "~&;; then-form ~S is ~S --> ~S~%" then-literal then-arg-types then-res-types)
	  ;; Introduce lexical binding for the union of all
	  ;; assigned-to variables in both branches.
	  (let ((assigned-variables
		 (set-union (side-effects-assigned-variables then-side-effects)
			    (side-effects-assigned-variables else-side-effects))))
	    (dolist (name assigned-variables)
	      (when (and (stringp name)
			 (member name *lexicals* :test 'string-equal)
			 (not (assoc name *lexical-variables*)))
		(let* ((fun (get-bst-function name))
		       (var (make-variable :name (bst-name-to-lisp-name name :lexical)
					   :type (car (bst-function-result-types fun)))))
		  (push (cons name var) *lexical-variables*)
		  (push (make-binding :variables (list var)
				      :mvform (make-mvform :form `(values) :types nil))
			*form-bindings*)))))
	  ;; Now we know the arity of both branches.  
	  (let ((arg-types ())
		(then-balance (- (length then-res-types) (length then-arg-types)))
		(else-balance (- (length else-res-types) (length else-arg-types)))
		(then-stack ())
		(else-stack ())
		(fixed-then-res-types then-res-types)
		(fixed-else-res-types else-res-types))
	    (flet ((build-stacks (&key then-exception else-exception)
		     (do ((then-arg-types (reverse then-arg-types) (cdr then-arg-types))
			  (else-arg-types (reverse else-arg-types) (cdr else-arg-types)))
			 ((and (null then-arg-types) (null else-arg-types))
			  (setq arg-types (nreverse arg-types)
				then-stack (nreverse then-stack)
				else-stack (nreverse else-stack)))
		       (let* ((type (type-intersection
				     (if then-arg-types (car then-arg-types) t)
				     (if else-arg-types (car else-arg-types) t)))
			      (form (pop-single-value-form type
							   :need-variable :if-side-effects)))
			 (push type arg-types)
			 (unless (and then-exception (null then-arg-types))
			   (push form then-stack)
			   (when (null then-arg-types)
			     (push (car (mvform-types form)) fixed-then-res-types)))
			 (unless (and else-exception (null else-arg-types))
			   (push form else-stack)
			   (when (null else-arg-types)
			     (push (car (mvform-types form)) fixed-else-res-types)))))))
	      (cond ((= then-balance else-balance)
		     ;; This is the regular case: We have the same
		     ;; net number of values pushed to the stack.
		     ;; We compute the arg types and fill up the
		     ;; shorter arg list.
		     (build-stacks))
		    ((and *compiling-while-body*
			  (> else-balance 0)
			  (= (length then-res-types) (length else-res-types)))
		     ;; This is a special hack that allows us to compile
		     ;; the FORMAT.NAMES function.  This function has a
		     ;; WHILE$ that pushes exactly one value over all
		     ;; runs; this is accomplished by an IF$ whose THEN
		     ;; branch is STRING->STRING and whose ELSE branch
		     ;; is NIL->STRING.
		     ;; FIXME: This could be easily generalized on a better day.
		     (bst-compile-warning " Warning: Employing the producer/modifier while loop trick.~%")
		     (build-stacks :else-exception t)
		     ;;(format *error-output* "~&ARG-TYPES: ~S~%THEN-STACK: ~S~%ELSE-STACK: ~S~%"
		     ;; arg-types then-stack else-stack))
		     (setq *compiling-while-body* else-balance))
		    (*relaxed-compilation*
		     (bst-compile-warning
		      "Warning: THEN and ELSE functions deliver different net number of values.~%~
                     ~&  THEN function ~:S~%  is ~:A -> ~:A~%~
                     ~&  and ELSE function ~:S~%  is ~:A -> ~:A.~%~
                     ~&  Because *RELAXED-COMPILATION* is true, we try to fix this~%~
                     ~&  by filling up the ~A function.  (This can cause errors later.)~%"
		      then-literal
		      then-arg-types then-res-types
		      else-literal
		      else-arg-types else-res-types
		      (if (< then-balance else-balance)
			  "THEN" "ELSE"))
		     (build-stacks)
		     (let ((dummy-items
			    (loop repeat (abs (- else-balance then-balance))
				  collect (make-mvform :form `(error "This is the result of a slightly broken BibTeX style file.")
						       :types nil ;(list t)
						       ;;:side-effects (make-instance 'side-effects :side-effects-p t)
						       )))
			   (dummy-types
			    (loop repeat (abs (- else-balance then-balance))
				  collect nil)))
		       (if (< then-balance else-balance)
			   (setf then-stack
				 (nconc then-stack dummy-items)
				 fixed-then-res-types
				 (nconc dummy-types fixed-then-res-types))
			   (setf else-stack
				 (nconc else-stack dummy-items)
				 fixed-else-res-types
				 (nconc dummy-types fixed-else-res-types)))))
		    (t
		     (bst-compile-error "THEN function ~:S ~%  == ~S~%  and ELSE function ~:S ~%  == ~S~%  deliver ~
different net number of values: ~%  ~:A -> ~:A vs. ~:A -> ~:A"
					then-literal then-form else-literal else-form
					then-arg-types then-res-types
					else-arg-types else-res-types))))
	    (let ((res-types (mapcar #'type-union* fixed-then-res-types fixed-else-res-types)))
	      (multiple-value-bind (else-form else-arg-types
					      else-res-types else-side-effects)
		  (bst-compile-literal else-literal else-stack :borrowing-allowed nil
				       :expected-result-types res-types)
		(declare (ignore else-arg-types else-res-types))
		(multiple-value-bind (then-form then-arg-types
						then-res-types then-side-effects)
		    (bst-compile-literal then-literal then-stack :borrowing-allowed nil
					 :expected-result-types res-types)
		  (declare (ignore then-arg-types then-res-types))
		  (let* ((then-or-else-side-effects
			  (max-side-effects then-side-effects else-side-effects)))
		    ;; MAX-SIDE-EFFECTS assumes a sequence of
		    ;; operations; for IF$, we need to fix some aspects
		    ;; of the computed side-effects.
		    (setf (side-effects-unconditionally-assigned-variables then-or-else-side-effects)
			  (intersection (side-effects-unconditionally-assigned-variables then-side-effects)
					(side-effects-unconditionally-assigned-variables else-side-effects)
					:test 'equalp)
			  (side-effects-variables-used-before-assigned then-or-else-side-effects)
			  (union (side-effects-variables-used-before-assigned then-side-effects)
				 (side-effects-variables-used-before-assigned else-side-effects)
				 :test 'equalp))
		    (let ((side-effects
			   (max-side-effects (mvform-side-effects val-mvform)
					     then-or-else-side-effects)))
		      (push-mvform :form (build-if-form (mvform-form val-mvform) then-form else-form)
				   :types res-types
				   :side-effects side-effects)))))))))))   

(define-bst-special-form "pop$"
    (pop-form t :need-variable :if-side-effects))

(define-bst-special-form "skip$"
    nil)

(defun assign-loop-variables ()
  "For every borrowed variable, pop a form off the stack.  Push a
PSETQ form that assigns the values of the popped forms to the borrowed
variables.  If there are not as many values on the stack as borrowed
variables, return :NOT-BALANCED; in this case the current computation
is left corrupted.  Otherwise return the number of assignments (this
can be lower than the number of borrowed variables)."
  (let ((mvforms
	 (mapcar (lambda (var)
		   (or (pop-single-value-form (variable-type var) :when-empty nil)
		       (return-from assign-loop-variables :not-balanced)))
		 (reverse *borrowed-variables*))))
    ;; See if there's anything left on the stack
    (let ((remaining-form (pop-single-value-form t :when-empty nil)))
      (when remaining-form
	(return-from assign-loop-variables :not-balanced)))
    (let* ((psetq-args (mapcan (lambda (var mvform)
				 (if (eql (mvform-form mvform)
					  (variable-name var))
				     ;; loop variable is unchanged; no
				     ;; assignment needed
				     '()
				     (list (variable-name var)
					   (mvform-form mvform))))
			       (reverse *borrowed-variables*)
			       mvforms))
	   (assigned-variables
	    (mapcar #'variable-name *borrowed-variables*))
	   (side-effects
	    (apply #'max-side-effects
		   (make-instance 'side-effects :assigned-variables assigned-variables)
		   (mapcar #'mvform-side-effects mvforms))))
      (case (length psetq-args)
	(0 nil)
	(2 (push-mvform :form `(setq ,@psetq-args)
			:types () 
			:side-effects side-effects))
	(t (push-mvform :form `(psetq ,@psetq-args)
			:types ()
			:side-effects side-effects)))
      (/ (length psetq-args) 2))))

(defun bst-compile-literal-as-while-body (literal loop-vars loop-var-types)
  "Compile a BST function LITERAL, which is a symbol, designating a
BST function, or a list (a function body).  Return five values: a Lisp
BODY, LOOP-VARS, LOOP-VAR-TYPES, INIT-TYPES and SIDE-EFFECTS."
  (let ((*form-stack*
	 (mapcar (lambda (var type)
		   (make-mvform :form var :types (list type)))
		 loop-vars loop-var-types))
	(*borrowed-variables*
	 (mapcar (lambda (var type)
		   (make-variable :name var :type type))
		 loop-vars loop-var-types))
	(*form-bindings* ())
	(*compiling-while-body* t))
    (compile-literal literal)
    (assign-loop-variables)
    (multiple-value-bind (body arg-types res-types
			       side-effects free-variables)
	(package-as-body)
      (unless (null res-types)
	;; Since the while-body packager eats as many values as the
	;; function takes, so we only check whether there remain
	;; values...
	(bst-compile-error "BODY function ~:S is not stack-balanced: ~:S --> ~:S"
			   literal arg-types res-types))
      (values body free-variables
	      arg-types
	      (if (numberp *compiling-while-body*)
		  ;; this many are uninitialized
		  (nbutlast arg-types *compiling-while-body*)
		  arg-types)
	      side-effects))))

(defun bst-compile-literal-as-while-predicate (pred-literal)
  (let ((*form-stack* ())
	(*borrowed-variables* ())
	(*form-bindings* ()))
    (compile-literal pred-literal)
    (let ((predicate-value-form
	   (pop-single-value-form '(boolean)
				  :when-empty nil
				  :assigned-variables
				  (mapcar #'variable-name *borrowed-variables*)
				  :need-variable :if-side-effects)))
      (unless predicate-value-form
	(bst-compile-error "PREDICATE function ~:S~%  does not deliver a (boolean or integer) stack value; we cannot handle this"
			   pred-literal))
      (let ((num-assignments (assign-loop-variables)))
	(when (eql num-assignments :not-balanced)
	  (throw 'complicated-loop :not-balanced))
	(unless (zerop num-assignments)
	  (throw 'complicated-loop :assignments-in-predicate))
	(push-form predicate-value-form)
	(multiple-value-bind (pred-form pred-arg-types pred-res-types
					pred-side-effects pred-free-variables)
	    (package-as-form)
	  (declare (ignore pred-res-types))
	  (setq pred-side-effects
		(remove-variables-from-side-effects pred-free-variables pred-side-effects))
	  (values pred-form pred-free-variables
		  pred-arg-types pred-side-effects))))))

(defun current-stack-as-values-list ()
  "Pop all value forms off the stack, collect them in a list suitable
for VALUES and push them again onto the stack.  Return two values: a
list of forms and a list of their types."
  (let ((result-mvforms '()))
    (loop as mvform = (pop-single-value-form t :need-variable t :when-empty nil)
	  while mvform
	  do (push mvform result-mvforms))
    (dolist (mvform result-mvforms)
      (push-form mvform))
    (values (mapcar #'mvform-form result-mvforms)
	    (mapcar (lambda (mvform)
		      (car (mvform-types mvform))) result-mvforms))))    
    
(define-bst-special-form "while$"
    (let* ((body-literal (pop-literal))
	   (pred-literal (pop-literal)))
      (flet ((make-init-clauses (loop-vars init-types)
	       (loop for var in loop-vars
		     as types = init-types then (cdr types)
		     collect (if (null types)
				 var
				 `(,var ,(pop-form (car types)))))))
	(when
	    (catch 'complicated-loop
	      ;; Try to compile the predicate as a Lisp form
	      (multiple-value-bind (pred-form pred-free-variables
					      pred-arg-types pred-side-effects)
		  (bst-compile-literal-as-while-predicate pred-literal)
		;; Compile body
		(multiple-value-bind (body loop-vars loop-var-types init-types
					   body-side-effects)
		    (bst-compile-literal-as-while-body body-literal
						       pred-free-variables
						       pred-arg-types)
		  ;; filter out locals from the list of variables assigned to
		  ;; in the body
		  (setq body-side-effects
			(remove-variables-from-side-effects loop-vars body-side-effects))
		  (let ((init-clauses (make-init-clauses loop-vars init-types))
			(values-body  (build-values-body loop-vars))
			(side-effects
			 (max-side-effects pred-side-effects body-side-effects)))
		    ;; Only pred is guaranteed to execute, so:
		    (setf (side-effects-unconditionally-assigned-variables side-effects)
			  (side-effects-unconditionally-assigned-variables pred-side-effects))
		    (push-mvform :form `(do ,init-clauses
					 (,(build-not-form pred-form) ,@values-body)
					 ,@body)
				 :types loop-var-types
				 :side-effects side-effects))))
	      nil)
	  ;; Complicated loop
	  (multiple-value-bind (loop-form loop-vars loop-var-types
					  return-types side-effects)
	      (let ((*form-stack* ())
		    (*borrowed-variables* ())
		    (*form-bindings* ()))
		(compile-literal pred-literal)
		;; Compute the results and the side effects of the predicate
		(multiple-value-bind (dummy-lisp-form dummy-arg-types dummy-res-types
						      predicate-side-effects)
		    (package-as-form)
		  (declare (ignore dummy-lisp-form dummy-arg-types dummy-res-types))
		  (let* ((predicate-form (pop-single-value-form '(boolean))) 
			 (borrowed-variables-at-return *borrowed-variables*)
			 (return-form (list 'return nil))) ; will be mutated later
		    (multiple-value-bind (values-list-at-return types-list-at-return)
			(current-stack-as-values-list)
		      (push-mvform :form `(when ,(build-not-form
						  (mvform-form predicate-form))
					   ,return-form)
				   :types '()
				   :side-effects (max-side-effects
						  (mvform-side-effects predicate-form)
						  (make-instance 'side-effects
								 :side-effects-p t)))
		      (compile-literal body-literal)
		      (assign-loop-variables)
		      (multiple-value-bind (body arg-types res-types side-effects free-vars)
			  (package-as-body)
			(declare (ignore res-types))
			;; Handle the variables that have been
			;; borrowed after emitting the RETURN form
			(let* ((newly-borrowed-variables
				(ldiff *borrowed-variables* borrowed-variables-at-return))
			       (values-list
				(nconc (mapcar #'variable-name newly-borrowed-variables)
				       values-list-at-return))
			       (returned-types
				(nconc (mapcar #'variable-type newly-borrowed-variables)
				       types-list-at-return)))
			  ;; Fix the return form
			  (setf (cdr return-form)
				(build-values-body values-list))
			  ;; Only the predicate is guaranteed to
			  ;; execute
			  (setf (side-effects-unconditionally-assigned-variables side-effects)
				(side-effects-unconditionally-assigned-variables predicate-side-effects))
			  (values `(loop ,@body)
				  free-vars
				  arg-types
				  returned-types
				  side-effects)))))))
	    (setq side-effects
		  (remove-variables-from-side-effects loop-vars side-effects))
	    (push-mvform :form `(let ,(make-init-clauses loop-vars loop-var-types)
				 ,loop-form)
			 :types return-types
			 :side-effects side-effects))))))

;;;

(defun compile-funcall (function-name)
  (let (it)
    (cond
      ((setq it (gethash (string function-name) *bst-special-forms*))
       (funcall it))
      ((setq it (assoc (string function-name) *lexical-variables* :test 'string-equal))
       (let ((var-name (variable-name (cdr it))))
       (push-mvform :form var-name
		    :types (list (variable-type (cdr it)))
		    :side-effects
		    (make-instance 'side-effects
				   :used-variables (list var-name)
				   :variables-used-before-assigned (list var-name)))))
      (t
       (let* ((bst-function (get-bst-function function-name))
	      (arg-types (bst-function-argument-types bst-function))
	      (reversed-pop-form-args-list
	       (reverse (bst-function-pop-form-args bst-function)))
					; may be shorter than arg-types
	      (arg-mvforms (nreverse
			    (mapcar (lambda (type)
				      (apply #'pop-single-value-form type
					     (pop reversed-pop-form-args-list)))
				    (reverse arg-types)))))
	 (push-mvform
	  :form (if (bst-function-lisp-form-maker bst-function)
		    (apply (bst-function-lisp-form-maker bst-function)
			   (mapcar #'mvform-form arg-mvforms))
		    (cons (bst-function-lisp-name bst-function)
			  (mapcar #'mvform-form arg-mvforms)))
	  :types (bst-function-result-types bst-function)
	  :side-effects (apply #'max-side-effects
			       (bst-function-side-effects bst-function)
			       (mapcar #'mvform-side-effects arg-mvforms))))))))

(defun compile-body (body)
  (let ((*currently-compiled-body* body))
    (do ((rest body (cdr rest)))
	((null rest))
      (let ((form (car rest))
	    (*currently-compiled-body-rest* rest))
	(cond
	  ((numberp form)
	   (push-mvform :form form :types '((integer))))
	  ((stringp form)
	   (push-mvform :form form :types '((string))))
	  ((and (consp form) (eql (car form) 'quote)) ; quoted function
	   (push-mvform :literal-p t
			:literal (cadr form) :types '((symbol))))
	  ((listp form)			; function body
	   (push-mvform :literal-p t
			:literal form :types '((body))))
	  ((symbolp form)		;function call
	   (compile-funcall form))
	  (t (bst-compile-error "Illegal form in BST function body: ~:S" form)))))))

(defun bst-compile-defun (name function-definition)
  "Compile a BST wizard-defined function of given NAME and
FUNCTION-DEFINITION.  If NAME is nil, build a lambda expression,
rather than a defun form.  Return four values: DEFUN-OR-LAMBDA-FORM,
ARGUMENT-TYPES, RESULT-TYPES, SIDE-EFFECTS."
  (let ((*borrowed-variables* ())
	(*form-bindings* ())
	(*form-stack* ())
	(*lexical-variables* ())
	(*bst-gentemp-counter* 0))
    (compile-body function-definition)
    (package-as-procedure name)))

(defun bst-compile-thunkcall (bst-name)
  "Build a Lisp form for calling the BST function named BST-NAME."
  (let ((*borrowed-variables* ())
	(*form-bindings* ())
	(*lexical-variables* ())
	(*form-stack* ()))
    (compile-body (list bst-name))
    (package-as-form)))	

(defun print-bst-function-info (bst-function stream)
  "Print information on arity and side effects of BST-FUNCTION to
STREAM as Lisp comments."
  (let ((side-effects (bst-function-side-effects bst-function)))
    #-clisp
    (format stream
	    "~%~<;; ~@;~:S --> ~:S ~:[~;with side-effects ~]~:[~;~%with assignment to~:*~{ ~S~}~]~:[~;~%with possible assignment to~:*~{ ~S~}~]~:[~;~%with reference-before-assignment to~:*~{ ~S~}~]~:[~;~%with reference to~:*~{ ~S~}~]~:>"
	    (list (bst-function-argument-types bst-function)
		  (bst-function-result-types bst-function)
		  (side-effects-side-effects-p side-effects)
		  (side-effects-unconditionally-assigned-variables side-effects)
		  (set-difference (side-effects-assigned-variables side-effects)
				  (side-effects-unconditionally-assigned-variables side-effects)
				  :test #'equalp)		
		  (side-effects-variables-used-before-assigned side-effects)
		  (set-difference (side-effects-used-variables side-effects)
				  (side-effects-variables-used-before-assigned side-effects)
				  :test #'equalp)))))  

(defun compile-bst-function (bst-function)
  (let* ((bst-name (bst-function-name bst-function))
	 (*currently-compiled-function* bst-name)
	 (lisp-name (bst-name-to-lisp-name bst-name :function)))
    (handler-case 
	(handler-bind ((bst-compiler-warning
			(lambda (condition)
			 (format *error-output*
				 "While compiling wizard-defined function `~A':~%~A~%"
				 bst-name (bst-compiler-error-message condition)))))
	  (multiple-value-bind (defun-form argument-types
				   result-types side-effects)
	      (bst-compile-defun lisp-name
				 (bst-function-body bst-function))
	    (setf (bst-function-lisp-name bst-function)
		  lisp-name
		  (bst-function-type bst-function)
		  'compiled-wiz-defined
		  (bst-function-argument-types bst-function)
		  argument-types
		  (bst-function-result-types bst-function)
		  result-types
		  (bst-function-side-effects bst-function)
		  side-effects
		  (bst-function-defun-form bst-function)
		  defun-form)))
      (bst-compiler-error (condition)
	;;(declare (ignore condition))
	(signal condition) 
	nil))))

(defun compile-bst-fun (definition &key int-vars str-vars)
  "A debugging aid."
  (let ((*bib-macros* (make-hash-table :test 'equalp))
	(*bst-compiling* t)
	(*bst-functions* (builtin-bst-functions)))
    (dolist (var int-vars)
      (register-bst-global-var var var 'int-global-var '(integer) 0 *bst-functions*))
    (dolist (var str-vars)
      (register-bst-global-var var var 'str-global-var '(string) "" *bst-functions*))
    (bst-compile-defun nil definition)))

(defun make-some-variables-lexical ()
  ;; If in every function where variable X is used, it is assigned
  ;; before any reference, then there cannot be inter-function data
  ;; flow through variable X.  Hence, we can make the variable lexical
  ;; in every function where it is used.
  (loop for bst-function being each hash-value in *bst-functions*
	when (and (member (bst-function-type bst-function)
			  '(str-global-var int-global-var))
		  (not (bst-function-constant-p bst-function)))
	do (setf (bst-function-lexical-p bst-function) t))
  (loop for bst-function being each hash-value in *bst-functions*
	when (eq (bst-function-type bst-function) 'compiled-wiz-defined)
	do (dolist (variable (side-effects-variables-used-before-assigned
			      (bst-function-side-effects bst-function)))
	     (let ((bst-function (get-bst-function variable)))
	       (setf (bst-function-lexical-p bst-function) nil))))
  (loop for bst-function being each hash-value in *bst-functions*
	when (and (member (bst-function-type bst-function)
			  '(str-global-var int-global-var))
		  (bst-function-lexical-p bst-function))
	do (unintern (bst-function-lisp-name bst-function) *bst-package*)
	and collect (bst-function-name bst-function)))

(defun constant-bst-variable-p (variable)
  (loop with assignment = nil and reference = nil
	for callee in (reverse *bst-function-call-sequence*) do
	(let ((side-effects (bst-function-side-effects variable)))
	  ;; We test for reference before we test for assignment.
	  ;; This way, we rule out the case of a function that assigns
	  ;; a variable and refers to it; here we cannot decide
	  ;; whether the variable is constant or not.
	  (when (member (bst-function-name variable)
			(side-effects-used-variables side-effects)
			:test #'string-equal)
	    (setq reference t))
	  ;; Test for assignment
	  (when (member (bst-function-name variable)
			(side-effects-assigned-variables side-effects)
			:test #'string-equal)
	    (when assignment
	      ;; another assignment -> not constant
	      (return-from constant-bst-variable-p nil))
	    (when reference
	      ;; reference to variable before assignment
	      ;; means reference to default value -> not constant
	      (return-from constant-bst-variable-p nil))
	    (setq assignment t)))
	finally (return-from constant-bst-variable-p t)))    

(defun make-some-variables-constant ()
  (loop for bst-function being each hash-value in *bst-functions*
	when (and (member (bst-function-type bst-function)
			  '(str-global-var int-global-var))
		  (not (bst-function-constant-p bst-function))
		  (< (bst-function-num-assignments bst-function) 2)
		  (typep (bst-function-assigned-value-form bst-function)
			 '(or integer string))
		  (constant-bst-variable-p bst-function))
	do (let ((new-lisp-name
		  (bst-name-to-lisp-name (bst-function-name bst-function)
					 :constant)))
	     (unintern (bst-function-lisp-name bst-function) *bst-package*)
	     (setf (bst-function-constant-p bst-function) t
		   (bst-function-lexical-p bst-function) nil
		   (bst-function-lisp-name bst-function) new-lisp-name
		   (bst-function-lisp-form-maker bst-function)
		   (lambda () new-lisp-name)
		   (bst-function-setter-form-maker bst-function)
		   (lambda () `(values))))
	and collect (bst-function-name bst-function)))  

#|

(compile-bst-fun '(1 duplicate$ + duplicate$ -))
(register-bst-primitive "side.effect" '((string)) '((string)) 'side-effect)
(register-bst-primitive "side.effect.2" '((string)) '((string)) 'side-effect-2)
(compile-bst-fun '("foo" side.effect "bar" side.effect.2))
(compile-bst-fun '((1 2 >) (5 + swap$ 7 + swap$) while$))
(compile-bst-fun '((1 2 >) (3 + swap$ "baz" * swap$) while$))
(compile-bst-fun '(1 1 (pop$ "et al" *) (pop$ "foo" *) if$))
(compile-bst-fun '(pop$ "et al" *))
(compile-bst-fun '("can't use both volume and number if series info is missing"
		   WARNING$ "in BibTeX entry type `" TYPE$ * "'" * TOP$))
(compile-bst-fun '("abc" top$))
(compile-bst-fun '(1 (1 'global.max$ |:=|) (newline$) if$) )

(with-input-from-string (*bst-stream* "{ booktitle empty$ { \"\" } { editor empty$ { booktitle } { booktitle add.space.if.necessary \"(\" * format.nonauthor.editors * \")\" * } if$ } if$ } ")
  (bst-read))
(compile-bst-fun '(crossref EMPTY$ ("")
 (crossref EMPTY$ (crossref)
  (crossref "A" "(" * 1 'global.max$ |:=| * ")" *)
  IF$)
 IF$))

(with-input-from-string (*bst-stream* "{ 's :=
  #1 'nameptr :=
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }
    { \"Foo\" 't :=
      nameptr #1 >
	{ \"x\" * } 
	't
      if$
      nameptr #1 + 'nameptr :=
    }
  while$
} ")
  (let ((f (bst-read)))
    (compile-bst-fun f :int-vars '(numnames nameptr namesleft) :str-vars '(s t))))
  
		

|#

