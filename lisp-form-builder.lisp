;;; A BibTeX re-implementation in Common Lisp - construct beautiful Lisp forms
;;; Copr. 2001, 2002 Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de>
;;; This is free software, licensed under GNU GPL (see file COPYING)

(defun build-if-form (val-form then-form else-form)
  "Build a Lisp form equivalent to `(IF ,VAL-FORM ,THEN-FORM ,ELSE-FORM)
but try to beautify the result by using COND, UNLESS, and WHEN, rather
than IF in certain cases."
  (let ((then-operator (and (consp then-form) (car then-form)))
	(else-operator (and (consp else-form) (car else-form))))
    (labels ((clauses (form)
	       (let ((operator (and (consp form) (car form))))
		 (case operator
		   (progn `((t ,@(cdr form))))
		   (if `((,(cadr form)
			  ,(caddr form))
			 (t
			  ,(cadddr form))))
		   (cond (cdr form))
		   (t `((t ,form))))))
	     (body (form)
	       (let ((operator (and (consp form) (car form))))
		 (if (eql operator 'progn)
		     (cdr form)
		     (list form)))))
      (cond
	((equal then-form '(values))   ; we have, in fact, an `unless'
	 `(unless ,val-form
	   ,@(body else-form)))
	((equal else-form '(values))	; we have, in fact, a `when'
	 `(when ,val-form
	   ,@(body then-form)))
	((member else-operator '(if cond)) ; beautify using `cond'
	 `(cond
	   (,val-form ,@(body then-form))
	   ,@(clauses else-form)))
	((member then-operator '(if cond)) ; beautify using reverse `cond'
	 `(cond
	   (,(build-not-form val-form)
	    ,@(body else-form))
	   ,@(clauses then-form)))
	((or (eql else-operator 'progn)
	     (eql then-operator 'progn)) ; beautify using `cond'
	 `(cond
	   (,val-form ,@(body then-form))
	   ,@(clauses else-form)))
	(t				; normal if
	 `(if ,val-form
	   ,then-form
	   ,else-form))))))

(defun build-associative-form (operators form1 form2)
  "Build the form `(,@OPERATORS FORM1 FORM2) but if FORM1 and FORM2
are of this form, use the associativity of the operation to build
`(,@OPERATORS FORMS...) instead."
  (labels ((operation-p (form)
	     (and (consp form)
		  (let ((index (mismatch operators form :test 'equal)))
		    (or (not index)
			(= index (length operators))))))
	   (args (form)
	     (subseq form (length operators)))
	   (arg-forms (form)
	     (if (operation-p form)
		 (args form)
		 (list form))))
    `(,@operators ,@(arg-forms form1) ,@(arg-forms form2)))) 

(defun build-not-form (form)
  "Build the form `(not ,@FORM) but simplify that if possible."
  (labels ((operator (f)
	     (and (consp f) (car f))))
    (let ((op (operator form)))
      (case op
	(not (cadr form))
	((> < >= <=)
	 (if (= (length form) 3)
	     (let ((negop (ecase op (> '<=) (< '>=) (<= '>) (>= '<))))
	       `(,negop ,@(cdr form)))
	     `(not ,form)))
	(string= `(string/= ,@(cdr form)))
	(string/= `(string= ,@(cdr form)))
	((and or)
	 ;; de-Morgan-ize if at least one negated subform is found
	 (if (find 'not (cdr form) :key #'operator)
	     (let ((negop (ecase op (and 'or) (or 'and))))
	       `(,negop ,@(mapcar #'build-not-form (cdr form))))
	     `(not ,form)))
	(t `(not ,form))))))

