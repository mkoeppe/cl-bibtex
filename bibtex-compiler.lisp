;;; A BibTeX re-implementation in Common Lisp - the BST->CL compiler
;;; Copr. 2001, 2002 Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de>
;;; This is free software, licensed under GNU GPL (see file COPYING)

;; TODO:
;; * Check that the pop/push order is fine in while$ bodys
;;   that are (INTEGER), (STRING) -> (INTEGER), (STRING) 
;; * order of side effects
;; * maybe beautify (if ... (progn ...) (progn ...)) ==> (cond ...)
;; * maybe special form for = that chooses = or string=
;; * don't name the temporary variables occuring in while$ "ARGnn"
;; * propagate types when they get more specific:
;;   { $duplicate + } is of type (INTEGER) -> (INTEGER), not T -> (INTEGER)
;; * boolean/integer issues
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
  form
  side-effects-p)

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

(defvar *bst-gentemp-counter* 0)

(defun bst-gentemp (prefix)
  (intern (format nil "~A~A" prefix (incf *bst-gentemp-counter*))))  

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
			    (effective-type (type-intersection type available-type)))
		       (when (null-type effective-type)
			 (bst-compile-error "Type mismatch: expecting ~A, got ~A."
					    type available-type))
		       (return-from pop-form (values (mvform-form top-mvform) effective-type (mvform-side-effects-p top-mvform))))))
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
	       (multiple-value-bind (variables mvforms)
		   (map2 (lambda (type)
			   (let ((symbol (bst-gentemp "T")))
			     (values (make-variable :name symbol :type type)
				     (make-mvform :form symbol :types (list type)))))
			 (mvform-types top-mvform))
	 ;;(format t "variables: ~A~%mvforms: ~A~%" variables mvforms)
		 (push (make-binding :variables variables
				     :form (mvform-form top-mvform)
				     :side-effects-p (mvform-side-effects-p top-mvform))
		       *form-bindings*)
		 (setq *form-stack* (nconc mvforms *form-stack*)))))))))

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
variables corresponding to *BORROWED-VARIABLES*.  Return five values:
BODY, ARGUMENT-TYPES, RESULT-TYPES, SIDE-EFFECTS-P, and
FREE-VARIABLES."
  (let ((*form-stack* *form-stack*)
	(result-forms ())
	(result-types ())
	(any-side-effects nil))
    (loop (multiple-value-bind (form type side-effects-p)
	      (pop-form t :need-variable nil :when-empty nil) ; modifies the place *form-stack*
	    (setq any-side-effects
		  (or any-side-effects side-effects-p))
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
	(setq any-side-effects
	      (or any-side-effects (binding-side-effects-p binding)))
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
	      (0 `nil)
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
    (multiple-value-bind (form1 type1) (pop-form t :need-variable :if-side-effects)
      (multiple-value-bind (form2 type2) (pop-form t :need-variable :if-side-effects)
	(push (make-mvform :form form1 :types (list type1)) *form-stack*)
	(push (make-mvform :form form2 :types (list type2)) *form-stack*))))

(define-bst-special-form ":="
    (let ((var (pop-literal)))
      (multiple-value-bind (value-form value-type) (pop-form t :need-variable nil)
	;;(format t "var: ~S value: ~S~%" var value-form)
	(let* ((bst-function (get-bst-function var))
	       (setter-form-maker (bst-function-setter-form-maker bst-function))
	       (setter-form (funcall setter-form-maker value-form)))
	  (push (make-mvform :form setter-form :types () :side-effects-p t)
		*form-stack*)))))

(defun get-bst-function (name)
  (let ((function (gethash (string name) *bst-functions*)))
    (unless function
      (bst-compile-error "~A is an unknown function" name))
    function))

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
					     (pop-form req-type
						       :need-variable :if-side-effects)
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
			      *borrowed-variables*)))
      (push (make-mvform :form `(psetq ,@psetq-args)
			 :types (mapcar #'variable-type *borrowed-variables*)
			 :side-effects-p t)
	    *form-stack*))
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
	  (unless (= (length body-arg-types) (length body-res-types))
	    (bst-compile-error "BODY function ~S is not stack-balanced: ~S --> ~S"
			       body-literal body-arg-types body-res-types))
	  (let ((init-clauses (mapcar (lambda (var type)
					`(,var ,(pop-form type)))
				      free-body-vars body-arg-types))
		(values-body
		 (case (length body-res-types)
		   (0 ())
		   (1 (list (car free-body-vars)))
		   (2 (list `(values ,@free-body-vars))))))
	    (push (make-mvform :form `(do ,init-clauses
				          (,pred-form ,@values-body)
				       ,@body)
			       :types body-res-types
			       :side-effects-p (or pred-side-effects-p body-side-effects-p))
		  *form-stack*)))))) 


;;;

(defun compile-funcall (function-name)
  (let ((special-form (gethash (string function-name) *bst-special-forms*)))
    (if special-form
	(funcall special-form)
	(let* ((bst-function (get-bst-function function-name))
	       (arg-types (bst-function-argument-types bst-function))
	       (arg-forms (nreverse
			   (mapcar (lambda (type)
				     ;; `:need-variable nil' is sufficient
				     ;; but might want to say :if-side-effects
				     ;; for aesthetic reasons:
				     (pop-form type :need-variable nil
					       ;;:if-side-effects
					       ))
				   (reverse arg-types))))
	       (result-types (bst-function-result-types bst-function)))
	  (cond
	    ((bst-function-lisp-form-maker bst-function)
	     (push (make-mvform :form (apply (bst-function-lisp-form-maker bst-function)
					     arg-forms)
				:types result-types
				:side-effects-p (bst-function-side-effects-p bst-function))
		   *form-stack*))
	    (t				; normal function call
	     (push (make-mvform :form (cons (bst-function-lisp-name bst-function)
					    arg-forms)
				:types result-types
				:side-effects-p (bst-function-side-effects-p bst-function))
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
	(*form-stack* ())
	(*bst-gentemp-counter* 0))
    (compile-body function-definition)
    (package-as-procedure name)))

;;;

(defun compile-bst-file (bst-file lisp-file)
  (let ((*bib-macros* (make-hash-table))
	(*bst-compiling* t))
    (with-open-file (*lisp-stream* lisp-file :direction :output)
      (with-open-file (bst-stream bst-file)
	(format *lisp-stream*
		";;;; This is a -*- Common-Lisp -*- program, automatically translated~%~
;;;; from the BibTeX style file `~A'~%~
;;;; by the CL-BibTeX compiler ($Revision: 1.4 $).~%"
		bst-file)
	(get-bst-commands-and-process bst-stream)))))

#|
(compile-bst-file (kpathsea:find-file "amsalpha-xx.bst")
		  "/tmp/compiled-bst.lisp")

|#

