;;; A BibTeX re-implementation in Common Lisp - the BST->CL compiler
;;; Copr. 2001, 2002 Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de>
;;; This is free software, licensed under GNU GPL (see file COPYING)

;; TODO:
;; * macros
;; * most variables are in fact lexical variables (some, in fact, only
;;   store arguments for later use).  Keep track whether a variable is
;;   accessed before it is assigned in any function; if not, we can make
;;   it lexical in *all* functions.  This requires a second compiler pass.
;; * assignments only affect the forms using the variables assigned to,
;;   so keep lists of used and affected variables around.  this would help
;;   to get rid of PROG1-type constructions like
;;     (let ((t1 FORM)) (setq VAR FORM2) t1)
;;   where FORM does not depend on VAR
;; * maybe replace dots with dashes in function names
;; * don't name the temporary variables occuring in while$ "ARGnn"
;; * propagate types when they get more specific:
;;   { $duplicate + } is of type (INTEGER) -> (INTEGER), not T -> (INTEGER)
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
  "A typed Lisp variable"
  name
  type)

(defstruct binding
  "A multiple-value binding frame"
  variables
  form
  side-effects-p)

(defstruct mvform
  "A multiple-values-delivering form on the stack"
  form					; a Lisp form OR
  literal				; a BST symbol or function body
  types
  side-effects-p
  used-functions
  assigned-functions)

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
    

(defun bst-compile-error (format-string &rest args)
  (error (make-condition 'bst-compiler-error
			 :message
			 (concatenate 'string
				      (format nil " In BST body ~S:~%  "
					      (currently-compiled-body-with-markup))
				      (apply 'format nil format-string args)))))

(defvar *bst-gentemp-counter* 0)

(defun bst-gentemp (prefix)
  (intern (format nil "~A~A" prefix (incf *bst-gentemp-counter*))))  

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
			:form (mvform-form mvform)
			:side-effects-p (mvform-side-effects-p mvform))
	  *form-bindings*)
    (setq *form-stack* (nconc (nreverse mvforms)
			      *form-stack*))))

(defun pop-form (type &key (need-variable nil) (when-empty :borrow))
  "Pop a Lisp form delivering a single value of given TYPE from
*FORM-STACK*.  If the stack is empty, borrow a variable instead if
:WHEN-EMPTY is :BORROW, or return nil if :WHEN-EMPTY is nil.  If
:NEED-VARIABLE is nil, POP-FORM may return a side-effecting
single-value form \(it should only be called once, in order).  If
:NEED-VARIABLE is :IF-SIDE-EFFECTS, POP-FORM will introduce a variable
for side-effecting forms.  Otherwise, POP-FORM will introduce a
variable for all non-atom forms.  Return three values: the Lisp form,
the actual type of the delivered value, and SIDE-EFFECTS-P."
  (loop (when (null *form-stack*)
	  (ecase when-empty
	    ((nil) (return-from pop-form nil))
	    (:borrow 
	     ;; Borrow a variable
	     (let ((arg-symbol (bst-gentemp "ARG")))
	       (push (make-variable :name arg-symbol :type type)
		     *borrowed-variables*)
	       (return-from pop-form (values arg-symbol type))))))
	(let ((top-mvform (pop *form-stack*)))
	  (labels ((return-the-form ()
		     (let* ((available-type (car (mvform-types top-mvform)))
			    (effective-type (type-intersection type available-type))
			    (lisp-form (mvform-form top-mvform))
			    (side-effects-p (mvform-side-effects-p top-mvform)))
		       ;; The handling of the special case boolean/integer is only a hack.
		       ;; The type system should be improved instead.
		       (cond
			 ((and (type= available-type '(integer))
			       (type= type '(boolean)))
			  (return-from pop-form
			    (values `(> ,lisp-form 0) '(boolean) side-effects-p)))
			 ((and (type= available-type '(boolean))
			       (type= type '(integer)))
			  (return-from pop-form
			    (values `(if ,lisp-form 1 0) '(boolean) side-effects-p)))
			 ((null-type effective-type)
			  (bst-compile-error "Type mismatch: expecting ~A, got ~A."
					     type available-type))
			 (t
			  (return-from pop-form
			    (values lisp-form effective-type side-effects-p)))))))
	    (cond
	      ((mvform-literal top-mvform)
	       (bst-compile-error "Expecting a form on the stack, got a literal ~S."
				  (mvform-literal top-mvform)))
	      ((not (consp (mvform-form top-mvform))) ; Variable, string, or number, delivering one value
	       (return-the-form))
	      ((and (eql need-variable :if-side-effects)
		    (not (mvform-side-effects-p top-mvform))
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

(defun pop-single-value-form (type &key (need-variable nil)
			      (when-empty :borrow))
  "Like pop-form, but package the return values in a MVFORM object."
  (multiple-value-bind (form type side-effects-p)
      (pop-form type :need-variable need-variable
		:when-empty when-empty)
    (if form
	(make-mvform :form form :types (list type)
		     :side-effects-p side-effects-p)
	nil)))

(defun push-form (mvform)
  "Push MVFORM onto *FORM-STACK*."
  ;; We first check if the last form has a side-effect.  If so, we
  ;; must turn it into a binding, or the order of side-effects will be
  ;; wrong.
  (when (and (not (null *form-stack*))
	     (mvform-side-effects-p (car *form-stack*)))
    (let ((se-form (pop *form-stack*)))
      (make-binding-and-push-variables se-form)))
  (when (eql (mvform-side-effects-p mvform) :assignment)
    ;; An assignment is even worse because it can render even purely
    ;; functional forms wrong if the order isn't fine.  Hence, convert
    ;; everything on the value stack to bindings before proceeding.
    (loop for form = (pop-single-value-form t :need-variable t :when-empty nil)
	  while form
	  collect form into clean-form-stack
	  finally (setq *form-stack* clean-form-stack)))
  (push mvform *form-stack*))

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

(defun max-side-effects (a b)		;deprecated
  "Compute the maximum of the side-effects A and B."
  (case a
    ((nil) b)
    (:assignment :assignment)
    (t (if (eql b :assignment) :assignment a))))

;; TODO: we will use this function to consolidate the multiple-value
;; stuff and to keep track of used and assigned-to functions.  While
;; we're at it, make BINDING-FORM a MVFORM instead of a Lisp form and
;; get rid of BINDING-SIDE-EFFECTS-P.
(defun make-compound-mvform (&key form types side-effects-p used-functions assigned-functions
			     component-mvforms)
  "Make an MVFORM from the given keyword parameters but combine with
the SIDE-EFFECTS-P, USED-FUNCTIONS and ASSIGNED-FUNCTIONS of the
elements of the list COMPONENT-MVFORMS."
  (make-mvform :form form
	       :types types
	       :side-effects-p (or side-effects-p
				   (some #'mvform-side-effects-p component-mvforms))
	       :used-functions
	       (union used-functions
		      (reduce #'union (mapcar #'mvform-used-functions component-mvforms)))
	       :assigned-functions
	       (union assigned-functions
		      (reduce #'union (mapcar #'mvform-assigned-functions component-mvforms)))))

(defun package-as-body ()
  "Build a Lisp body corresponding to the computation captured in
*FORM-BINDINGS* and *FORM-STACK*.  The Lisp body contains free
variables corresponding to *BORROWED-VARIABLES*.  Return five values:
BODY, ARGUMENT-TYPES, RESULT-TYPES, SIDE-EFFECTS-P, and
FREE-VARIABLES."
  (let ((*form-stack* *form-stack*)
	(*form-bindings* *form-bindings*)
	(result-forms ())
	(result-types ())
	(any-side-effects nil))
    (loop (multiple-value-bind (form type side-effects-p)
	      (pop-form t :need-variable nil :when-empty nil) ; modifies the place *form-stack*
	    (setq any-side-effects
		  (max-side-effects any-side-effects side-effects-p))
	    (cond
	      (form
	       (push form result-forms)
	       (push type result-types))
	      (t
	       ;;(setq result-forms (nreverse result-forms)
	       ;; result-types (nreverse result-types))
	       (return)))))
    (let ((body (case (length result-forms)
		  (0 ())
		  (1 (list (car result-forms)))
		  (t (list `(values ,@result-forms))))))
      (dolist (binding *form-bindings*)
	(setq any-side-effects
	      (max-side-effects any-side-effects (binding-side-effects-p binding)))
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
	      any-side-effects
	      (mapcar #'variable-name *borrowed-variables*)))))

(defun package-as-form ()
  "Build a Lisp form corresponding to the computation captured in
*FORM-BINDINGS* and *FORM-STACK*.  The Lisp form contains free
variables corresponding to *BORROWED-VARIABLES*.  Return four values:
LISP-FORM, ARGUMENT-TYPES, RESULT-TYPES, SIDE-EFFECTS-P."
  (multiple-value-bind (body argument-types result-types
			     side-effects-p free-variables)
      (package-as-body)
    (values (case (length body)
	      (0 `(values))
	      (1 (car body))
	      (t `(progn ,@body)))
	    argument-types result-types side-effects-p
	    free-variables)))
 
(defun package-as-procedure (name)
  "Build a DEFUN NAME form from *FORM-BINDINGS*, *BORROWED-VARIABLES*
and *FORM-STACK*.  If NAME is nil, build a LAMBDA form instead.
Return four values: DEFUN-OR-LAMBDA-FORM, ARGUMENT-TYPES,
RESULT-TYPES, SIDE-EFFECTS-P."
  (multiple-value-bind (body argument-types result-types
			     side-effects-p free-variables)
      (package-as-body)
    (values `(,@(if name `(defun ,name) `(lambda))
	      ,free-variables
	      ,@body)
	    argument-types result-types side-effects-p)))

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
    (multiple-value-bind (form-1 type-1 side-effects-p-1)
	(pop-form t :need-variable nil)
      (multiple-value-bind (form-2 type-2 side-effects-p-2)
	  (pop-form type-1 :need-variable nil)
	(let ((form (cond
		      ((type= type-2 '(boolean))
		       `(eql ,form-1 ,form-2))
		      ((type= type-2 '(integer))
		       `(= ,form-1 ,form-2))
		      ((type= type-2 '(string))
		       `(string= ,form-1 ,form-2))
		      (t
		       `(equal ,form-1 ,form-2)))))
	  (push-form (make-mvform :form form
				  :types (list '(boolean))
				  :side-effects-p
				  (max-side-effects side-effects-p-1
						    side-effects-p-2)))))))
	
(define-bst-special-form ":="
    (let* ((var (pop-literal))
	   (value-form (pop-form t :need-variable nil)))
      ;;(format t "var: ~S value: ~S~%" var value-form)
      (let* ((bst-function (get-bst-function var))
	     (setter-form-maker (bst-function-setter-form-maker bst-function))
	     (setter-form (funcall setter-form-maker value-form)))
	(push-form (make-mvform :form setter-form :types ()
				:side-effects-p :assignment)))))

(defun get-bst-function (name)
  (let ((function (gethash (string name) *bst-functions*)))
    (unless function
      (bst-compile-error "~A is an unknown function" name))
    function))

(defun make-if-form (val-form then-form else-form)
  "Build a Lisp form equivalent to `(IF ,VAL-FORM ,THEN-FORM ,ELSE-FORM)
but try to beautify the result by using COND rather than IF in certain cases."
  (let ((then-operator (and (consp then-form) (car then-form)))
	(else-operator (and (consp else-form) (car else-form))))
    (cond
      ((equal then-form '(values)) ;; we have, in fact, an `unless'
       `(unless ,val-form
	 ,@(if (eql else-operator 'progn)
	       (cdr else-form)
	       (list else-form))))
      ((equal else-form '(values)) ;; we have, in fact, a `when'
       `(when ,val-form
	 ,@(if (eql then-operator 'progn)
	       (cdr then-form)
	       (list then-form))))
      ((or (eql then-operator 'progn)
	   (member else-operator '(if progn cond)))
       ;; beautify the condition using `cond'
       `(cond
	 (,val-form ,@(if (eql then-operator 'progn)
			  (cdr then-form)
			  (list then-form)))
	 ,@(case else-operator
		 (progn `((t ,@(cdr else-form))))
		 (if `((,(cadr else-form)
			,(caddr else-form))
		       (t
			,(cadddr else-form))))
		 (cond (cdr else-form))
		 (t `((t ,else-form))))))
      (t 
       ;; normal if
       `(if ,val-form
	 ,then-form
	 ,else-form)))))  

(defun bst-compile-literal (literal stack &key (borrowing-allowed t))
  "Compile a BST function LITERAL , which is a symbol, designating a
BST function, or a list (a function body).  Return five values: a Lisp
FORM, ARGUMENT-TYPES, RESULT-TYPES, SIDE-EFFECTS-P, and FREE-VARIABLES."
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
	   (val-form (pop-form '(boolean) :need-variable nil)))
      ;; First pass: compute the arity of both branches
      (multiple-value-bind (else-form else-arg-types)
	  (bst-compile-literal else-literal ())
	(declare (ignore else-form))
	;;(format t "~&;; else-form ~S is ~S --> ~S~%" else-literal else-arg-types else-res-types)
	(multiple-value-bind (then-form then-arg-types)
	    (bst-compile-literal then-literal ())
	  (declare (ignore then-form))
	  ;;(format t "~&;; then-form ~S is ~S --> ~S~%" then-literal then-arg-types then-res-types)
	  ;; Now we know the arity of both branches.  We compute the
	  ;; arg types and fill up the shorter arg list.
	  (let ((arg-types ()))
	    (do ((then-arg-types then-arg-types (cdr then-arg-types))
		 (else-arg-types else-arg-types (cdr else-arg-types)))
		((and (null then-arg-types) (null else-arg-types)))
	      (push (type-intersection (if then-arg-types (car then-arg-types) t)
				       (if else-arg-types (car else-arg-types) t))
		    arg-types))
	    ;; Pop this many forms to build a temporary form stack
	    (let ((branch-stack
		   (mapcar #'(lambda (req-type)
			       (multiple-value-bind (form type)
				   (pop-form req-type
					     :need-variable :if-side-effects)
				 (make-mvform :form form
					      :types (list type))))
			   arg-types)))
	      (multiple-value-bind (else-form else-arg-types
					    else-res-types else-side-effects-p)
		  (bst-compile-literal else-literal branch-stack :borrowing-allowed nil)
		(declare (ignore else-arg-types))
		(multiple-value-bind (then-form then-arg-types
					      then-res-types then-side-effects-p)
		    (bst-compile-literal then-literal branch-stack :borrowing-allowed nil)
		  (declare (ignore then-arg-types))
		  (unless (= (length then-res-types) (length else-res-types))
		    (bst-compile-error "THEN function ~S ~%== ~S and ELSE function ~S ~%== ~S deliver ~
different net number of values: ~%~A -> ~A vs. ~A"
				       then-literal then-form else-literal else-form
				       arg-types then-res-types else-res-types))
		  (let* ((res-types (mapcar #'type-union then-res-types else-res-types))
			 (side-effects-p
			  (max-side-effects then-side-effects-p else-side-effects-p)))
		    (push-form (make-mvform :form (make-if-form val-form then-form else-form)
					    :types res-types
					    :side-effects-p side-effects-p)))))))))))   

(define-bst-special-form "pop$"
    (pop-form t :need-variable :if-side-effects))

(define-bst-special-form "skip$"
    nil)

(defun bst-compile-literal-as-while-body (literal)
  "Compile a BST function LITERAL, which is a symbol, designating a
BST function, or a list (a function body).  Return five values: a Lisp
BODY, ARGUMENT-TYPES, RESULT-TYPES, SIDE-EFFECTS-P, and FREE-VARIABLES."
  (let ((*form-stack* ())
	(*borrowed-variables* ())
	(*form-bindings* ()))
    (etypecase literal
      (symbol (compile-funcall literal))
      (cons (compile-body literal)))
    (let ((psetq-args (mapcan (lambda (var)
				(list (variable-name var)
				      (pop-form (variable-type var))))
			      (reverse *borrowed-variables*))))
      (case (length *borrowed-variables*)
	(0 nil)
	(1 (push-form (make-mvform :form `(setq ,@psetq-args)
				   :types () 
				   :side-effects-p :assignment)))
	(t (push-form (make-mvform :form `(psetq ,@psetq-args)
				   :types ()
				   :side-effects-p :assignment)))))
    (package-as-body)))

(define-bst-special-form "while$"
    (let* ((body-literal (pop-literal))
	   (pred-literal (pop-literal)))
      (multiple-value-bind (pred-form pred-arg-types
				      pred-res-types pred-side-effects-p)
	  (bst-compile-literal pred-literal ())
	(unless (null pred-arg-types)
	  (bst-compile-error "PREDICATE function ~S takes stack values: ~S"
			     pred-literal pred-arg-types))
	(unless (and (= (length pred-res-types) 1)
		     (type= (car pred-res-types) '(boolean)))
	  (bst-compile-error "PREDICATE function ~S does not deliver exactly one boolean stack value: ~S"
			     pred-literal pred-res-types))
	(multiple-value-bind (body body-arg-types body-res-types
				   body-side-effects-p free-body-vars)
	    (bst-compile-literal-as-while-body body-literal)
	  (unless (null body-res-types)
	    ;; the while-body packager eats as many values as the
	    ;; function takes, so we only check whether there remain
	    ;; values...
	    ;; (= (length body-arg-types) (length body-res-types))
	    (bst-compile-error "BODY function ~S is not stack-balanced: ~S --> ~S"
			       body-literal body-arg-types body-res-types))
	  (let ((init-clauses (mapcar (lambda (var type)
					`(,var ,(pop-form type)))
				      free-body-vars body-arg-types))
		(values-body
		 (case (length body-arg-types)
		   (0 ())
		   (1 (list (car free-body-vars)))
		   (2 (list `(values ,@free-body-vars))))))
	    (push-form (make-mvform :form `(do ,init-clauses
					    ((not ,pred-form) ,@values-body)
					    ,@body)
				    :types body-arg-types
				    :side-effects-p (max-side-effects pred-side-effects-p body-side-effects-p)))))))) 


;;;

(defun compile-funcall (function-name)
  (let ((special-form (gethash (string function-name) *bst-special-forms*)))
    (if special-form
	(funcall special-form)
	(let* ((bst-function (get-bst-function function-name))
	       (arg-types (bst-function-argument-types bst-function))
	       (side-effects-p (bst-function-side-effects-p bst-function))
	       (arg-forms (nreverse
			   (mapcar (lambda (type)
				     (multiple-value-bind (form actual-type side-eff)
					 (pop-form type :need-variable nil)
				       (declare (ignore actual-type))
				       (setq side-effects-p
					     (max-side-effects side-effects-p side-eff))
				       form))
				   (reverse arg-types))))
	       (result-types (bst-function-result-types bst-function)))
	  (cond
	    ((bst-function-lisp-form-maker bst-function)
	     (push-form (make-mvform :form (apply (bst-function-lisp-form-maker bst-function)
						  arg-forms)
				     :types result-types
				     :side-effects-p side-effects-p)))
	    (t				; normal function call
	     (push-form (make-mvform :form (cons (bst-function-lisp-name bst-function)
						 arg-forms)
				     :types result-types
				     :side-effects-p (bst-function-side-effects-p bst-function)))))))))

(defun compile-body (body)
  (let ((*currently-compiled-body* body))
    (do ((rest body (cdr rest)))
	((null rest))
      (let ((form (car rest))
	    (*currently-compiled-body-rest* rest))
	(cond
	  ((numberp form)
	   (push-form (make-mvform :form form :types '((integer)))))
	  ((stringp form)
	   (push-form (make-mvform :form form :types '((string)))))
	  ((symbolp form)		;function call
	   (compile-funcall form))
	  ((and (consp form) (eql (car form) 'quote)) ; quoted function
	   (push-form (make-mvform :literal (cadr form) :types '((symbol)))))
	  ((consp form)			; function body
	   (push-form (make-mvform :literal form :types '((body)))))
	  (t (bst-compile-error "Illegal form in BST function body: ~S" form)))))))

(defun bst-compile-defun (name function-definition)
  "Compile a BST wizard-defined function of given NAME and
FUNCTION-DEFINITION.  If NAME is nil, build a lambda expression,
rather than a defun form.  Return four values: DEFUN-OR-LAMBDA-FORM,
ARGUMENT-TYPES, RESULT-TYPES, SIDE-EFFECTS-P."
  (let ((*borrowed-variables* ())
	(*form-bindings* ())
	(*form-stack* ())
	(*bst-gentemp-counter* 0))
    (compile-body function-definition)
    (package-as-procedure name)))

(defun bst-compile-thunkcall (bst-name)
  "Build a Lisp form for calling the BST function named BST-NAME."
  (let ((*borrowed-variables* ())
	(*form-bindings* ())
	(*form-stack* ()))
    (compile-body (list bst-name))
    (package-as-form)))	

(defun compile-bst-function (bst-name function-definition stream)
  (let ((lisp-name (bst-name-to-lisp-name bst-name)))
    (handler-case 
	(multiple-value-bind (defun-form argument-types
				 result-types side-effects-p)
	    (bst-compile-defun lisp-name function-definition)
	  (format stream
		  "~%;; ~S --> ~S ~A"
		  argument-types result-types
		  (case side-effects-p
		    ((nil) "")
		    (:assignment "with assignments")
		    (t "with side-effects")))
	  (lisp-write defun-form)
	  (setf (gethash (string bst-name) *bst-functions*)
		(make-bst-function :name (string bst-name)
				   :lisp-name lisp-name
				   :type 'compiled-wiz-defined
				   :argument-types argument-types
				   :result-types result-types
				   :side-effects-p side-effects-p)))
      (bst-compiler-error (condition)
	(format *error-output*
		"While compiling wizard-defined function `~S':~%~A~%"
		bst-name (bst-compiler-error-message condition))))))

(defun make-entry-type-function-alist ()
  (loop for fun being each hash-value in *bst-functions*
        when (and (member (bst-function-type fun)
                          '(wizard-defined compiled-wiz-defined))
                  (null (bst-function-argument-types fun))
                  (null (bst-function-result-types fun)))
	collect (cons (bst-function-name fun)
		      (bst-function-lisp-name fun))))
  
;;;

(defun compile-bst-file (bst-file lisp-file)
  (let ((*bib-macros* (make-hash-table))
	(*bst-compiling* t)
	(*main-lisp-body* ())
	(*bst-functions* (builtin-bst-functions)))
    (with-open-file (*lisp-stream* lisp-file :direction :output)
      (with-open-file (bst-stream bst-file)
	(format *lisp-stream*
		";;;; This is a -*- Common-Lisp -*- program, automatically translated~%;;;; from the BibTeX style file `~A'~%;;;; by the CL-BibTeX compiler ($Revision: 1.6 $).~%"
		bst-file)
	(get-bst-commands-and-process bst-stream)
	(lisp-write `(defun ,(intern (string-upcase (pathname-name bst-file))) ()
		      (let ((*bib-entry-type-functions*
			     ',(make-entry-type-function-alist)))
			,@(reverse *main-lisp-body*))))))))

(defun compile-bst-fun (definition)
  "A debugging aid."
  (let ((*bib-macros* (make-hash-table))
	(*bst-compiling* t)
	(*bst-functions* (builtin-bst-functions)))
    (bst-compile-defun nil definition)))

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

(progn (compile-bst-file (kpathsea:find-file "amsalpha-xx.bst")
			 "/tmp/compiled-bst.lisp")
       (load "/tmp/compiled-bst.lisp" :if-source-newer :compile)
       (cl-bibtex "ibm-theory" 'amsalpha-xx))

(compile-bst-file "test.bst"
		  "/tmp/compiled-bst.lisp")

|#

