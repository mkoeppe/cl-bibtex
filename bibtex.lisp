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
  lisp-form
  argument-types
  result-types
  side-effects-p
  value
  body)

(defvar *builtin-bst-functions* (make-hash-table :size 30 :test 'equalp))

(defun register-bst-primitive (bst-name argument-types result-types lisp-function)
  (setf (gethash (string bst-name) *builtin-bst-functions*)
	(make-bst-function :name (string bst-name)
			   :type 'built-in
			   :lisp-name lisp-function
			   :argument-types argument-types
			   :result-types result-types
			   :side-effects-p nil)))

(defmacro define-bst-primitive (bst-name arglist result-types &rest body)
  (let ((declarations '()))
    (do ()
	((not (and (not (null body))
		   (consp (car body))
		   (eql 'declare (caar body))))
	 (setq declarations (nreverse declarations)))
      (push (car body) declarations)
      (pop body))
    `(register-bst-primitive ,(string bst-name) ',(mapcar #'cadr arglist) ',result-types
			     #'(lambda ,(mapcar #'car arglist)
				 ,@declarations
				 ,@(if (symbolp bst-name)
				       `((block ,bst-name 
					   ,@body))
				       body)))))

(register-bst-primitive ">" '((integer) (integer)) '((boolean)) '>)
(register-bst-primitive "<" '((integer) (integer)) '((boolean)) '<)
(register-bst-primitive "=" '(t t) '((boolean)) 'equal)
(register-bst-primitive "+" '((integer) (integer)) '((integer)) '+)
(register-bst-primitive "-" '((integer) (integer)) '((integer)) '-)

(define-bst-primitive "*" ((a (string)) (b (string))) ((string))
  (concatenate 'string a b))

(define-bst-primitive ":=" ((value t) (variable (symbol))) ()
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
    (funcall (bst-function-setter function) value)))

(register-bst-primitive "add.period$" '((string)) '((string)) 'add-period-unless-sentence-end)
(register-bst-primitive "call.type$" '() '() 'call-type)

(defun call-type ()
  (let* ((type (gethash "entry-type" *bib-entry*))
	 (function (or (get-bst-function-of-type type '(wiz-defined))
		       (get-bst-function-of-type 'default.type '(wiz-defined)))))
    (when function
      (bst-execute function))))

(define-bst-primitive "change.case$" ((string (string)) (spec (string))) ((string))
  (cond
    ((string-equal spec "t") (bibtex-string-titledowncase string))
    ((string-equal spec "l") (bibtex-string-downcase string))
    ((string-equal spec "u") (bibtex-string-upcase string))
    (t (bib-warn "~S is an illegal case-conversion string" spec)
       string)))  

(define-bst-primitive "chr.to.int$" ((s (string))) ((integer))
  (cond
    ((= (length s) 1)
     (char-code (char s 0)))
    (t
     (bib-warn "String ~S is not a one-character string")
     0)))

(define-bst-primitive "cite$" () ((string))
  (gethash "key" *bib-entry* ""))

(define-bst-primitive "duplicate$" ((object t)) (t t)
  (values object object))

(register-bst-primitive "empty$" '((string missing)) '((boolean)) 'empty-field-p)

(define-bst-primitive format.name$ ((names (string)) (index (integer)) (format (string)))
    ((string))
  (unless (> index 0)
    (bib-warn "Bad index: ~A" index)
    (return-from format.name$ ""))
  (let ((bibtex-names (parse-bibtex-name-list names)))
    (when (> index (length bibtex-names))
      (if (zerop (length bibtex-names))
	  (bib-warn "There is no name in ~S" names)
	  (bib-warn "There aren't ~A names in ~S" index names))
      (return-from format.name$ ""))
    (format-bibtex-name nil format (elt bibtex-names (- index 1)))))

(define-bst-primitive "if$" ((pred (boolean)) (then (symbol body)) (else (symbol body))) ()
  (bst-execute-stack-literal
   (if pred
       then
       else)))      

(define-bst-primitive int.to.chr$ ((code (integer))) ((string))
  (let ((char (code-char code)))
    (unless char
      (bib-warn "~A isn't a valid character code" code)
      (return-from int.to.chr$ ""))
    (string char)))

(define-bst-primitive "int.to.str$" ((n (integer))) ((string))
  (format nil "~A" n))

(define-bst-primitive "missing$" ((object (string missing))) ((boolean))
  (null object))
  
(define-bst-primitive "newline$" () ()
  (terpri *bbl-output*))

(define-bst-primitive "num.names$" ((names (string))) ((integer))
  (length (parse-bibtex-name-list names)))

(define-bst-primitive "pop$" ((object t)) ()
  (declare (ignore object))
  nil)

(define-bst-primitive "preamble$" () ((string))
  *bib-preamble*)

(register-bst-primitive "purify$" '((string)) '((string)) 'bibtex-string-purify)

(define-bst-primitive "quote$" () ((string))
  "\"")

(register-bst-primitive "skip$" '() '() 'values)

(define-bst-primitive "substring$" ((s (string)) (start (integer)) (count (integer)))
    ((string))
  (cond
    ((or (> start (length s))
	 (< start (- (length s)))
	 (<= count 0))
     "")
    ((>= start 0)			; take count chars from start
     (subseq s (max 0 (- start 1)) (min (length s) (+ (- start 1) count))))
    (t
     (subseq s (max 0 (- (length s) (- start) (- count 1)))
	     (+ (length s) start 1)))))

#|(funcall (bst-function-lisp-name (gethash 'substring$ *builtin-bst-functions*))
	 8 -1 "ABCEFDGHJ")|#

(define-bst-primitive "swap$" ((a t) (b t)) (t t)
  (values b a))

(register-bst-primitive "text.length$" '((string)) '((integer)) 'length) ; FIXME: count text chars only
(register-bst-primitive "text.prefix$" '((string) (integer)) '((string)) 'bibtex-string-prefix)

(define-bst-primitive "type$" () ((string))
  (string-downcase (gethash "entry-type" *bib-entry* "")))

(register-bst-primitive "warning$" '((string)) 'nil 'bib-warn)

(define-bst-primitive "while$" ((predicate (symbol body)) (body (symbol body))) ()
  (do ()
      ((not (bst-execute-stack-literal/pop predicate '(boolean))))
    (bst-execute-stack-literal body)))
	
(register-bst-primitive "width$" '((string)) '((integer)) 'length) ; fixme

(define-bst-primitive "write$" ((s (string))) ()
  (princ s *bbl-output*))

(defun register-bst-entry (entry func-type type default-value hash-table)
  (setq entry (string entry))
  (setf (gethash entry hash-table)
	(make-bst-function :name entry
			   :lisp-name #'(lambda ()
					  (gethash entry *bib-entry* default-value))
			   :setter #'(lambda (value) (setf (gethash entry *bib-entry*)
							   value))
			   :type func-type
			   :argument-types '()
			   :result-types (list type)
			   :side-effects-p nil)))

(register-bst-entry "sort.key$" 'str-entry-var '(string) "" *builtin-bst-functions*)
(register-bst-entry "crossref" 'field '(string missing) nil *builtin-bst-functions*)

(defun register-bst-global-var (variable func-type type initial-value hash-table)
  (setq variable (string variable))
  (setf (gethash variable hash-table)
	(make-bst-function :name variable
			   :setter #'(lambda (value)
				       (setf (bst-function-value
					      (gethash variable hash-table))
					     value))
			   :type func-type
			   :argument-types '()
			   :result-types (list type)
			   :side-effects-p nil
			   :value initial-value)))

(register-bst-global-var "entry.max$" 'int-global-var '(integer)
			 most-positive-fixnum *builtin-bst-functions*)
(register-bst-global-var "global.max$" 'int-global-var '(integer)
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

(defun lisp-write (arg)
  (pprint arg *lisp-stream*))

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
	 (*bst-functions* (builtin-bst-functions))
	 (*entry-seen-p* nil)
	 (*read-seen-p* nil)
	 (*literal-stack* nil))
    (loop
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
  bst-name)

(defun check-for-already-defined-function (name)
  (unless (symbolp name)
    (error "~A is not a valid identifier" name))
  (let ((function (gethash (string name) *bst-functions*)))
    (when function
	(error "~A is already a ~A function name"
	       name (bst-function-type function)))))

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
	   (function (get-bst-function-of-type name '(built-in wiz-defined))))
      (if *bst-compiling*
	  (warn "Execute command not implemented")
	  (bst-execute function)))))

(defun bst-function-command ()
  (let* ((function-list (bst-read))
	 (function-definition (bst-read)))
    (unless (singleton-list-p function-list)
      (error "Illegal argument ~A to function command"
	     function-list))
    (let* ((bst-name (car function-list)))
      (check-for-already-defined-function bst-name)
      (let ((lisp-name (bst-name-to-lisp-name bst-name))
	    (argument-types nil)
	    (result-types nil)
	    (side-effects-p nil))
	(setf (gethash (string bst-name) *bst-functions*)
	      (make-bst-function :name (string bst-name)
				 :lisp-name lisp-name
				 :type 'wiz-defined
				 :argument-types argument-types
				 :result-types result-types
				 :side-effects-p side-effects-p
				 :body function-definition))))))

(defun bst-integers-command ()
  (let* ((name-list (bst-read)))
    (unless (listp name-list)
      (error "Illegal argument ~A to integers command"
	     name-list))
    (dolist (bst-name name-list)
      (check-for-already-defined-function bst-name)
      (register-bst-global-var bst-name 'int-global-var '(integer) 0 *bst-functions*)
      (if *bst-compiling*
	  (let ((lisp-name (bst-name-to-lisp-name bst-name)))
	    (lisp-write `(defvar ,lisp-name 0)))))))

(defun bst-iterate-command ()
  (unless *read-seen-p*
    (error "Illegal, iterate command before read command"))
  (let ((function-list (bst-read)))
    (unless (singleton-list-p function-list)
      (error "Illegal argument ~A to iterate command"
	     function-list))
    (let* ((name (car function-list))
	   (function (get-bst-function-of-type name '(built-in wiz-defined))))
      (if *bst-compiling*
	  (warn "Iterate command not implemented")
	  (loop for index from 0 below (length *bib-entries*)
		do (let ((*bib-entry* (elt *bib-entries* index)))
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
  (dolist (file *bib-files*)
    (let ((expanded-file (kpathsea:find-file (concatenate 'string file ".bib"))))
      (unless expanded-file
	(format *error-output* "I couldn't find database file `~A'" file))
      (with-open-file (s expanded-file :if-does-not-exist nil)
	(unless s
	  (format *error-output* "I couldn't open database file `~A'" expanded-file))
	(read-bib-database s))))
  (setq *bib-entries*
	(cited-bib-entries (if *cite-all-entries* t *cite-keys*)
			   :min-crossrefs 2)))   

(defun bst-reverse-command ()
  (unless *read-seen-p*
    (error "Illegal, reverse command before read command"))
  (let ((function-list (bst-read)))
    (unless (singleton-list-p function-list)
      (error "Illegal argument ~A to execute command"
	     function-list))
    (let* ((name (car function-list))
	   (function (get-bst-function-of-type name '(built-in wiz-defined))))
      (if *bst-compiling*
	  (warn "Reverse command not implemented")
	  (loop for index from (- (length *bib-entries*) 1) downto 0
		do (let ((*bib-entry* (elt *bib-entries* index)))
		     (bst-execute function)))))))

(defun bst-sort-command ()
  (unless *read-seen-p*
    (error "Illegal, sort command before read command"))
  (stable-sort *bib-entries* 'string<=
	       :key (lambda (entry) (gethash "sort.key$" entry ""))))

(defun bst-strings-command ()
  (let* ((name-list (bst-read)))
    (unless (listp name-list)
      (error "Illegal argument ~A to strings command"
	     name-list))
    (dolist (bst-name name-list)
      (check-for-already-defined-function bst-name)
      (register-bst-global-var bst-name 'str-global-var '(string) "" *bst-functions*)
      (when *bst-compiling*
	(let ((lisp-name (bst-name-to-lisp-name bst-name)))
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
	(*bib-style* nil))
    (read-aux-file (concatenate 'string file-stem ".aux"))
    (let* ((bst-file (kpathsea:find-file (concatenate 'string *bib-style* ".bst")))
	   (bst-stream (and bst-file
			   (open bst-file :if-does-not-exist nil))))
      (unless bst-stream
	(bib-fatal "I couldn't open style file `~A'" *bib-style*))
      (with-open-file (*bbl-output* (concatenate 'string file-stem ".bbl")
				    :direction :output)
	(get-bst-commands-and-process bst-stream)))))

;;;;

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

#|
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
