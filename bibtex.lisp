;;; A BibTeX re-implementation in Common Lisp - the BST interpreter
;;; Copr. 2001, 2002 Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de>
;;; This is free software, licensed under GNU GPL (see file COPYING)

(use-package '(bibtex-runtime))

(defvar *bst-readtable*
  (let ((*readtable* (copy-readtable nil)))
    ;; comment character
    (set-syntax-from-char #\% #\;)
    ;; CL's package marker : is an ordinary constituent in BST...
    ;; Here's just a quick fix to make := work
    (set-macro-character #\:
			 #'(lambda (stream char)
			     (declare (ignore char))
			     (let ((sym (read stream nil nil t)))
			       (unless (symbolp sym)
				 (error "Bad syntax"))
			       (intern
				(string-upcase 
				 (concatenate 'string ":" (symbol-name sym)))))))
			       
    ;; function lists
    (set-macro-character #\{
			 #'(lambda (stream char)
			     (declare (ignore char))
			     (read-delimited-list #\} stream t)))
    (set-syntax-from-char #\} #\))
    ;; quote
    (set-macro-character #\'
			 #'(lambda (stream char)
			     (declare (ignore char))
			     (list 'quote (read stream nil nil t))))
    ;; numbers
    (set-macro-character #\#
			 #'(lambda (stream char)
			     (declare (ignore char))
			     (read stream nil nil t)))
    ;; double-quote; BST strings have no escape
    (set-macro-character #\"
			 #'(lambda (stream char)
			     (declare (ignore char))
			     (coerce
			      (loop as char = (read-char stream nil nil t)
				    until (char= char #\")
				    collect char)
			      'string)))
    *readtable*))


;;; BST functions

(defstruct bst-function
  name
  type
  lisp-name
  setter
  lisp-form-maker
  argument-types
  result-types
  side-effects-p
  setter-form-maker
  value
  body
  ignore-redefinition-p)

(defvar *builtin-bst-functions* (make-hash-table :size 30 :test 'equalp))

(defun register-bst-primitive (bst-name argument-types result-types lisp-function &key (ignore-redefinition nil) (side-effects-p nil))
  (setf (gethash (string bst-name) *builtin-bst-functions*)
	(make-bst-function :name (string bst-name)
			   :type 'built-in
			   :lisp-name lisp-function
			   :argument-types argument-types
			   :result-types result-types
			   :side-effects-p side-effects-p
			   :ignore-redefinition-p ignore-redefinition)))

(defmacro define-bst-primitive (bst-name arglist result-types
				&key interpreted compiled
				(side-effects-p nil)
				(ignore-redefinition-p nil))
  `(setf (gethash ,(string bst-name) *builtin-bst-functions*)
    (make-bst-function :name ,(string bst-name)
     :type 'built-in
     :argument-types ',(mapcar #'cadr arglist)
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
     :side-effects-p ,side-effects-p
     :ignore-redefinition-p ,ignore-redefinition-p)))
			       

(register-bst-primitive ">" '((integer) (integer)) '((boolean)) '>)
(register-bst-primitive "<" '((integer) (integer)) '((boolean)) '<)
(register-bst-primitive "=" '(t t) '((boolean)) 'equal)
(register-bst-primitive "+" '((integer) (integer)) '((integer)) '+)
(register-bst-primitive "-" '((integer) (integer)) '((integer)) '-)

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

(mismatch '(concatenate 'string) '(concatenate 'string 1 2) :test 'equal)
	  
(define-bst-primitive "*" ((a (string)) (b (string))) ((string))
  :interpreted (concatenate 'string a b)
  :compiled (build-associative-form `(concatenate 'string) a b))

(define-bst-primitive ":=" ((value t) (variable (symbol))) ()
  :interpreted
  (let ((function (get-bst-function-of-type variable '(int-global-var str-global-var
						       int-entry-var str-entry-var))))
    (case (bst-function-type function)
      ((int-global-var int-entry-var)
       (unless (integerp value)
	 (error "Assignment of non-integer value ~S to integer variable ~S"
		value variable)))
      ((str-global-var str-entry-var)
       (unless (stringp value)
	 (error "Assignment of non-string value ~S to string variable ~S"
		value variable))))
    (funcall (bst-function-setter function) value))
  :side-effects-p :assignment)

(register-bst-primitive "add.period$" '((string)) '((string)) 'add-period-unless-sentence-end)

(define-bst-primitive "call.type$" () ()
  :interpreted 
  (let* ((type (gethash "ENTRY-TYPE" *bib-entry*))
	 (function (or (get-bst-function-of-type type '(wiz-defined compiled-wiz-defined))
		       (get-bst-function-of-type 'default.type '(wiz-defined compiled-wiz-defined)))))
    (when function
      (bst-execute function)))
  :compiled `(let ((type/fun (assoc (gethash "ENTRY-TYPE" *bib-entry*)
				    *bib-entry-type-functions*)))
	      (if type/fun
		  (funcall (cdr type/fun))
		  (,(bst-name-to-lisp-name "default.type")))))

(define-bst-primitive "change.case$" ((string (string)) (spec (string))) ((string))
  :interpreted
  (cond
    ((string-equal spec "t") (bibtex-string-titledowncase string))
    ((string-equal spec "l") (bibtex-string-downcase string))
    ((string-equal spec "u") (bibtex-string-upcase string))
    (t (bib-warn "~S is an illegal case-conversion string" spec)
       string))
  :compiled
  (if (stringp spec)			; specifier known at compile time
      (cond 
	((string-equal spec "t")
	 `(bibtex-string-titledowncase ,string))
	((string-equal spec "l")
	 `(bibtex-string-downcase ,string))
	((string-equal spec "u")
	 `(bibtex-string-upcase ,string))
	(t (bib-warn "~S is an illegal case-conversion string" spec)
	   string))
      `(cond
	((string-equal ,spec "t") (bibtex-string-titledowncase ,string))
	((string-equal ,spec "l") (bibtex-string-downcase ,string))
	((string-equal ,spec "u") (bibtex-string-upcase ,string))
	(t (bib-warn "~S is an illegal case-conversion string" ,spec)
	 ,string))))

(define-bst-primitive "chr.to.int$" ((s (string))) ((integer))
  :interpreted (cond
		 ((= (length s) 1)
		  (char-code (char s 0)))
		 (t
		  (bib-warn "String ~S is not a one-character string")
		  0))
  :compiled (if (stringp s)
		(if (= (length s) 1)
		    `(char-code ,(char s 0))
		    0)
		`(let ((str ,s))
		  (if (= (length str) 1)
		      (char-code (char str 0))
		      0))))

(define-bst-primitive "cite$" () ((string))
  :interpreted (gethash "KEY" *bib-entry* "")
  :compiled `(gethash "KEY" *bib-entry* ""))

(define-bst-primitive "duplicate$" ((object t)) (t t)
  :interpreted (values object object))

(register-bst-primitive "empty$" '((string missing)) '((boolean)) 'empty-field-p)

(define-bst-primitive "format.name$" ((names (string)) (index (integer)) (format (string)))
    ((string))
  :interpreted (format-nth-bibtex-name nil format names index)
  :compiled `(format-nth-bibtex-name nil ,format ,names ,index)
  ;; FIXME: This changes the arg order!
  )

(define-bst-primitive "if$" ((pred (boolean)) (then (symbol body)) (else (symbol body))) ()
  :interpreted
  (bst-execute-stack-literal
   (if pred
       then
       else)))      

(define-bst-primitive "int.to.chr$" ((code (integer))) ((string))
  :interpreted
  (let ((char (code-char code)))
    (cond
      (char (string char))
      (t (bib-warn "~A isn't a valid character code" code)
	 "")))
  :compiled
  `(string (code-char ,code)))  

(define-bst-primitive "int.to.str$" ((n (integer))) ((string))
  :interpreted (format nil "~A" n)
  :compiled `(format nil "~A" ,n))

(define-bst-primitive "missing$" ((object (string missing))) ((boolean))
  :interpreted (null object)
  :compiled `(null ,object))
  
(define-bst-primitive "newline$" () ()
  :interpreted (terpri *bbl-output*)
  :compiled `(terpri *bbl-output*)
  :side-effects-p t)

(register-bst-primitive "num.names$" '((string)) '((integer)) 'num-bibtex-names)

(define-bst-primitive "pop$" ((object t)) ()
  :interpreted (values))

(define-bst-primitive "preamble$" () ((string))
  :interpreted *bib-preamble*
  :compiled `*bib-preamble*)

(register-bst-primitive "purify$" '((string)) '((string)) 'bibtex-string-purify)

(define-bst-primitive "quote$" () ((string))
  :interpreted "\""
  :compiled "\"")

(register-bst-primitive "skip$" '() '() 'values)

(define-bst-primitive "stack$" () ()
  :interpreted nil
  :compiled `(values))

(define-bst-primitive "substring$"
    ((s (string)) (start (integer)) (count (integer)))
    ((string))
  :interpreted (bibtex-substring s start count)
  :compiled (if (eql count 'most-positive-fixnum)
		;; We can't use subseq because it throws an error
		;; if start >= length.  At least get rid of the
		;; `global.max$' equivalent.
		`(bibtex-substring ,s ,start)
		`(bibtex-substring ,s ,start ,count)))  

(define-bst-primitive "swap$" ((a t) (b t)) (t t)
  :interpreted (values b a))

(register-bst-primitive "text.length$" '((string)) '((integer)) 'length) ; FIXME: count text chars only
(register-bst-primitive "text.prefix$" '((string) (integer)) '((string)) 'bibtex-string-prefix)

(define-bst-primitive "top$" ((object t)) ()
  :interpreted (format *error-output* "~A~%" object)
  :compiled `(format *error-output* "~A~%" ,object)
  :side-effects-p t)

(define-bst-primitive "type$" () ((string))
  :interpreted (string-downcase (gethash "ENTRY-TYPE" *bib-entry* ""))
  :compiled `(string-downcase (gethash "ENTRY-TYPE" *bib-entry* "")))

(register-bst-primitive "warning$" '((string)) 'nil 'bib-warn :side-effects-p t)

(define-bst-primitive "while$" ((predicate (symbol body)) (body (symbol body))) ()
  :interpreted
  (do ()
      ((not (bst-execute-stack-literal/pop predicate '(boolean))))
    (bst-execute-stack-literal body)))
	
(register-bst-primitive "width$" '((string)) '((integer)) 'length) ; fixme

(define-bst-primitive "write$" ((s (string))) ()
  :interpreted (princ s *bbl-output*)
  :compiled `(princ ,s *bbl-output*)
  :side-effects-p t)

;;; The following three functions are not defined in the original
;;; BibTeX but in all style files.  We define them here because we
;;; can't infer that their result is `boolean' rather than `integer'.

(define-bst-primitive "and" ((a (boolean)) (b (boolean))) ((boolean))
  :interpreted (and a b)
  :compiled (build-associative-form `(and) a b)
  :ignore-redefinition-p t)

(define-bst-primitive "or" ((a (boolean)) (b (boolean))) ((boolean))
  :interpreted (or a b)
  :compiled (build-associative-form `(or) a b)
  :ignore-redefinition-p t)

(define-bst-primitive "not" ((a (boolean))) ((boolean))
  :interpreted (not a)
  :compiled `(not ,a)
  :ignore-redefinition-p t)

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
			   :type func-type
			   :argument-types '()
			   :result-types (list type)
			   :side-effects-p nil)))

(register-bst-entry "sort.key$" 'str-entry-var '(string) "" *builtin-bst-functions*)
(register-bst-entry "crossref" 'field '(string missing) nil *builtin-bst-functions*)

(defun register-bst-global-var (variable lisp-name func-type type initial-value hash-table)
  (let ((variable (string variable)))
    (setf (gethash variable hash-table)
	  (make-bst-function :name variable
			     :setter #'(lambda (value)
					 (setf (bst-function-value
						(gethash variable hash-table))
					       value))
			     :lisp-form-maker #'(lambda () lisp-name)
			     :setter-form-maker #'(lambda (value-form)
						    `(setq ,lisp-name ,value-form))
			     :type func-type
			     :argument-types '()
			     :result-types (list type)
			     :side-effects-p nil
			     :value initial-value))))

(register-bst-global-var "entry.max$" 'most-positive-fixnum 'int-global-var '(integer)
			 most-positive-fixnum *builtin-bst-functions*)
(register-bst-global-var "global.max$" 'most-positive-fixnum 'int-global-var '(integer)
			 most-positive-fixnum *builtin-bst-functions*)

(defvar *bst-functions* nil)

;;;

(defvar *bst-stream* nil)

(defun bst-read (&key (eof-ok nil))
  (let ((result
	 (let ((*readtable* *bst-readtable*))
	   (read *bst-stream* nil '*EOF*))))
    (when (and (not eof-ok)
	       (eq result '*EOF*))
      (error "Unexpected end of file"))
    result))

(defvar *bst-compiling* nil
  "Non-nil if we are compiling a Common Lisp program from the BST
program, rather than interpreting the BST program.")

(defvar *lisp-stream* nil
  "A stream where we write the Common Lisp program equivalent to the
BST program.")

(defvar *main-lisp-body* ()
  "A list collecting the forms corresponding to EXECUTE, ITERATE,
READ, REVERSE, and SORT commands in reverse order.")

(defun lisp-write (arg)
  (let ((*print-case* :downcase))
    (pprint arg *lisp-stream*))
  (terpri *lisp-stream*))

#|(defmacro lisp-write/exec (form)
  (if *bst-compiling*
      `(lisp-write ,form)
      form))|#

;;;

(defun builtin-bst-functions ()
  (let ((table (make-hash-table :test 'equalp)))
    (loop for key being each hash-key of *builtin-bst-functions*
	  do (setf (gethash key table)
		   (gethash key *builtin-bst-functions*)))
    table))

(defvar *read-seen-p* nil "Non-nil if the BST READ command has been seen.")
(defvar *entry-seen-p* nil "Non-nil if the BST ENTRY command has been seen.")

(defvar *literal-stack* nil
  "The stack of BibTeX literals during interpretation of a BST
program.")

(defun get-bst-commands-and-process (stream)
  (let* ((*bst-stream* stream)
	 (*entry-seen-p* nil)
	 (*read-seen-p* nil)
	 (*literal-stack* nil))
    (loop
     (when (and *bst-compiling*
		(char= (peek-char t *bst-stream* nil #\x) #\%))
       ;; Copy top-level comments; replace N leading % signs with N+1 semicola
       (terpri *lisp-stream*)
       (loop while (char= (peek-char t *bst-stream* nil #\x) #\%)
	     do
	     (princ ";" *lisp-stream*)
	     (loop while (char= (peek-char nil *bst-stream* nil #\x) #\%)
		   do (read-char *bst-stream*) (princ ";" *lisp-stream*))
	     (princ (read-line *bst-stream* nil "") *lisp-stream*)
	     (terpri *lisp-stream*)))
     (let ((command (bst-read :eof-ok t)))
       ;;(format t "Processing ~A command~%" command)
       (case command
	 ((ENTRY) (bst-entry-command))
	 ((EXECUTE) (bst-execute-command))
	 ((FUNCTION) (bst-function-command))
	 ((INTEGERS) (bst-integers-command))
	 ((ITERATE) (bst-iterate-command))
	 ((MACRO) (bst-macro-command))
	 ((READ) (bst-read-command))
	 ((REVERSE) (bst-reverse-command))
	 ((SORT) (bst-sort-command))
	 ((STRINGS) (bst-strings-command))
	 ((*EOF*) (return))
	 (else
	  (error "~A is an illegal style-file command" command)))))))

(defun bst-name-to-lisp-name (bst-name)
  (setq bst-name (string-upcase (string bst-name)))
  (if (string-equal bst-name "T")
      (gentemp "TEMP")
      (intern bst-name)))

(defun check-for-already-defined-function (name)
  (unless (symbolp name)
    (error "~A is not a valid identifier" name))
  (let ((function (gethash (string name) *bst-functions*)))
    (when function
      (unless (bst-function-ignore-redefinition-p function)
	(error "~A is already a ~A function name"
	       name (bst-function-type function))))
    function))

(defun bst-entry-command ()
  (when *entry-seen-p*
    (error "Illegal, another entry command"))
  (setq *entry-seen-p* t)
  (let* ((fields (bst-read))
	 (int-entry-vars (bst-read))
	 (str-entry-vars (bst-read)))
    (dolist (field fields)
      (check-for-already-defined-function field)
      (register-bst-entry field 'field '(string missing) nil *bst-functions*))
    (dolist (entry int-entry-vars)
      (check-for-already-defined-function entry)
      (register-bst-entry entry 'int-entry-var '(integer) 0 *bst-functions*))
    (dolist (entry str-entry-vars)
      (check-for-already-defined-function entry)
      (register-bst-entry entry 'str-entry-var '(string) "" *bst-functions*))))

(defun singleton-list-p (arg)
  "Non-nil if ARG is a list consisting of one symbol."
  (and (consp arg)
       (symbolp (car arg))
       (null (cdr arg))))

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

(defun bst-execute-command ()
  (unless *read-seen-p*
    (error "Illegal, execute command before read command"))
  (let ((function-list (bst-read)))
    (unless (singleton-list-p function-list)
      (error "Illegal argument ~A to execute command"
	     function-list))
    (let* ((name (car function-list))
	   (function (get-bst-function-of-type name '(built-in wiz-defined compiled-wiz-defined))))
      (if *bst-compiling*
	  (push (bst-compile-thunkcall name) *main-lisp-body*)
	  (bst-execute function)))))

(defun bst-function-command ()
  (let* ((function-list (bst-read))
	 (function-definition (bst-read)))
    (unless (singleton-list-p function-list)
      (error "Illegal argument ~A to function command"
	     function-list))
    (let* ((bst-name (car function-list)))
      (unless (check-for-already-defined-function bst-name)
	(if *bst-compiling*
	    (compile-bst-function bst-name function-definition *lisp-stream*)
	    (setf (gethash (string bst-name) *bst-functions*)
		  (make-bst-function :name (string bst-name)
				     :type 'wiz-defined
				     :body function-definition)))))))

(defun bst-integers-command ()
  (let* ((name-list (bst-read)))
    (unless (listp name-list)
      (error "Illegal argument ~A to integers command"
	     name-list))
    (dolist (bst-name name-list)
      (check-for-already-defined-function bst-name)
      (let ((lisp-name (bst-name-to-lisp-name bst-name)))
	(register-bst-global-var bst-name lisp-name 'int-global-var '(integer) 0 *bst-functions*)
	(when *bst-compiling*
	  (lisp-write `(defvar ,lisp-name 0)))))))

(defun bst-iterate-command ()
  (unless *read-seen-p*
    (error "Illegal, iterate command before read command"))
  (let ((function-list (bst-read)))
    (unless (singleton-list-p function-list)
      (error "Illegal argument ~A to iterate command"
	     function-list))
    (let* ((name (car function-list))
	   (function (get-bst-function-of-type name '(built-in wiz-defined compiled-wiz-defined))))
      (if *bst-compiling*
	  (push `(dolist (*bib-entry* *bib-entries*)
		  ,(bst-compile-thunkcall name))
		*main-lisp-body*)
	  (dolist (*bib-entry* *bib-entries*)
	    (bst-execute function)))))))

(defun bst-macro-command ()
  (when *read-seen-p*
    (error "Illegal, macro command after read command"))
   (let* ((function-list (bst-read))
	  (function-definition (bst-read)))
     (unless (singleton-list-p function-list)
       (error "Illegal argument ~A to macro command"
	      function-list))
     (unless (and (consp function-definition)
		  (stringp (car function-definition))
		  (null (cdr function-definition)))
       (error "Illegal argument ~A to macro command"
	      function-definition))
     (let ((name (car function-list))
	   (definition (car function-definition)))
       (check-for-already-defined-function name)
       (setq name (string name))
       (make-bst-function :name name
			  :lisp-name (lambda () (gethash name *bib-macros*))
			  :lisp-form-maker (lambda ()
					     `(gethash name *bib-macros*))
			  :type 'macro
			  :argument-types '()
			  :result-types '((string))
			  :side-effects-p nil)
       (setf (gethash name *bib-macros*) definition))))

(defun bst-read-command ()
  (when *read-seen-p*
    (error "Illegal, another read command"))
  (unless *entry-seen-p*
    (error "Illegal, read command before entry command"))
  (setq *read-seen-p* t)
  (if *bst-compiling*
      (push `(read-all-bib-files-and-compute-bib-entries) *main-lisp-body*)
      (read-all-bib-files-and-compute-bib-entries)))   

(defun bst-reverse-command ()
  (unless *read-seen-p*
    (error "Illegal, reverse command before read command"))
  (let ((function-list (bst-read)))
    (unless (singleton-list-p function-list)
      (error "Illegal argument ~A to execute command"
	     function-list))
    (let* ((name (car function-list))
	   (function (get-bst-function-of-type name '(built-in wiz-defined compiled-wiz-defined))))
	
      (if *bst-compiling*
	  (push `(dolist (*bib-entry* (reverse *bib-entries*))
		  ,(bst-compile-thunkcall name))
		*main-lisp-body*)
	  (dolist (*bib-entry* (reverse *bib-entries*))
	    (bst-execute function))))))

(defun bst-sort-command ()
  (unless *read-seen-p*
    (error "Illegal, sort command before read command"))
  (if *bst-compiling*
      (push `(setq *bib-entries*
	      (stable-sort *bib-entries* 'string<=
	       :key (lambda (entry) (gethash "SORT.KEY$" entry ""))))
	    *main-lisp-body*)
      (setq *bib-entries*
	    (stable-sort *bib-entries* 'string<=
			 :key (lambda (entry) (gethash "SORT.KEY$" entry ""))))))

(defun bst-strings-command ()
  (let* ((name-list (bst-read)))
    (unless (listp name-list)
      (error "Illegal argument ~A to strings command"
	     name-list))
    (dolist (bst-name name-list)
      (check-for-already-defined-function bst-name)
      (let ((lisp-name (bst-name-to-lisp-name bst-name)))
	(register-bst-global-var bst-name lisp-name 'str-global-var '(string) "" *bst-functions*)
	(when *bst-compiling*
	  (lisp-write `(defvar ,lisp-name "")))))))


;;; The BST interpreter

(defvar *show-stack* nil)

(defun bst-execute-body (body)
  (dolist (form body)
    (when *show-stack*
      (format t "Stack: ~S Form: ~S~%" *literal-stack* form))
    (cond
      ((or (numberp form) (stringp form)) (push form *literal-stack*))
      ((symbolp form) (bst-execute (get-bst-function-of-type form)))
      ((and (consp form) (eql (car form) 'quote))
       (push (cadr form) *literal-stack*))
      ((consp form) (push form *literal-stack*))
      (t (error "Illegal form in BST function body: ~S" form)))))

(defun bst-execute-stack-literal (literal)
  (cond
    ((symbolp literal) (bst-execute (get-bst-function-of-type literal)))
    ((consp literal) (bst-execute-body literal))
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
	(symbol
	 (cond ((null literal)
		(unless (or (eql type t)
			    (member 'missing type))
		  (type-error)))
	       (t
		(unless (or (eql type t)
			    (member 'symbol type))
		  (type-error)))))
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
	  (symbol
	   (cond ((null value)
		  (unless (or (eql type t)
			      (member 'missing type))
		    (type-error)))
		 (t
		  (unless (or (eql type t)
			      (member 'symbol type))
		    (type-error)))))
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
    
;;; The BibTeX program

(defun bibtex (file-stem)
  (let ((*bib-macros* (make-hash-table))
	(*bib-database* (make-hash-table :test #'equalp))
	(*bib-preamble* "")
	(*bib-entries* ())
	(*bib-files* ())
	(*cite-all-entries* nil)
	(*cite-keys* ())
	(*history* +spotless-history+)
	(*err-count* 0)
	(*bib-style* nil)
	(*bst-functions* (builtin-bst-functions)))	
    (read-aux-file (concatenate 'string file-stem ".aux"))
    (let* ((bst-file (kpathsea:find-file (concatenate 'string *bib-style* ".bst")))
	   (bst-stream (and bst-file
			   (open bst-file :if-does-not-exist nil))))
      (unless bst-stream
	(bib-fatal "I couldn't open style file `~A'" *bib-style*))
      (with-open-file (*bbl-output* (concatenate 'string file-stem ".bbl")
				    :direction :output)
	(get-bst-commands-and-process bst-stream)))))

(defun cl-bibtex (file-stem function)
  (let ((*bib-macros* (make-hash-table))
	(*bib-database* (make-hash-table :test #'equalp))
	(*bib-preamble* "")
	(*bib-entries* ())
	(*bib-files* ())
	(*cite-all-entries* nil)
	(*cite-keys* ())
	(*history* +spotless-history+)
	(*err-count* 0)
	(*bib-style* nil)
	(*bst-functions* (builtin-bst-functions)))	
    (read-aux-file (concatenate 'string file-stem ".aux"))
    (with-open-file (*bbl-output* (concatenate 'string file-stem ".bbl")
				  :direction :output)
      (funcall function))))

;;;;

#|
(defun f ()
  (let ((s (open "/usr/share/texmf/bibtex/bst/base/abbrv.bst"))
	(*bibtex-split-initials* t)
	(*bst-compiling* nil)
	(*bib-macros* (make-hash-table))
	(*bib-database* (make-hash-table :test #'equalp))
	(*bib-files* '("/home/mkoeppe/cvs/iba-papers/iba-bib.bib"))
	(*cite-all-entries* t)
	(*cite-keys* nil)
	(*bib-entries* nil))
    (get-bst-commands-and-process s)))

(let ((*readtable* *bst-readtable*))
  (read s))

(let ((*readtable* *bst-readtable*))
  (read-from-string "a:b"))

(defvar s (open "/tmp/x"))

(let ((*readtable* *bst-readtable*))
  (read s))

(with-input-from-string (s "\"a\\ebc\"")
  (read s))
|#

;;; Local Variables:
;;; eval: (put 'define-bst-primitive 'lisp-indent-function 3)
;;; End:
