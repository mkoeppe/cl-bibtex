;;; A BibTeX re-implementation in Common Lisp - The built-in BST functions
;;; Copr. 2001, 2002 Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de>
;;; This is free software, licensed under GNU GPL (see file COPYING)

(in-package bibtex-compiler)

(register-bst-primitive ">" '((integer) (integer)) '((boolean)) '>)
(register-bst-primitive "<" '((integer) (integer)) '((boolean)) '<)
(register-bst-primitive "=" '(t t) '((boolean)) 'equal)

(define-bst-primitive "+" ((a (integer)) (b (integer))) ((integer))
  :interpreted (+ a b)
  :compiled (build-associative-form `(+) a b))

(register-bst-primitive "-" '((integer) (integer)) '((integer)) '-)

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
    (funcall (bst-function-setter function) value)))

(register-bst-primitive "add.period$" '((string)) '((string)) 'add-period-unless-sentence-end)

(define-bst-primitive "call.type$" () ()
  :interpreted 
  (let* ((type (gethash "ENTRY-TYPE" *bib-entry*))
	 (function (or (get-bst-function-of-type type '(wiz-defined compiled-wiz-defined))
		       (get-bst-function-of-type 'default.type '(wiz-defined compiled-wiz-defined)))))
    (when function
      (bst-execute function)))
  :compiled
  (let ((type/fun-sym (bst-intern "TYPE/FUN")))
    `(let ((,type/fun-sym
	    (assoc (gethash "ENTRY-TYPE" *bib-entry*)
		   *bib-entry-type-functions* :test 'string-equal)))
      (if ,type/fun-sym
	  (funcall (cdr ,type/fun-sym))
	  (,(bst-name-to-lisp-name "default.type" :function))))))

(define-bst-primitive "change.case$" ((string (string))
				      (spec (string) :need-variable t)) ((string))
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

(define-bst-primitive "chr.to.int$" ((s (string) :need-variable t)) ((integer))
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
		`(if (= (length ,s) 1)
		     (char-code (char ,s 0))
		     0)))

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
  :interpreted (bbl-terpri)
  :compiled `(bbl-terpri)
  :side-effects-p t)

(register-bst-primitive "num.names$" '((string)) '((integer)) 'num-bibtex-names)

(define-bst-primitive "pop$" ((object t)) ()
  :interpreted (declare (ignore object)))

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
		(if (eql start 1)
		    ;; Program tried to cut the string down to a
		    ;; length of `entry.max$'.  Since CL-BibTeX does
		    ;; not have such a limitations, we can just use
		    ;; the string.
		    s
		    ;; We can't use subseq because it throws an error
		    ;; if start >= length.  At least get rid of the
		    ;; `global.max$' equivalent.
		    `(bibtex-substring ,s ,start))
		;; General form
		`(bibtex-substring ,s ,start ,count)))  

(define-bst-primitive "swap$" ((a t) (b t)) (t t)
  :interpreted (values b a))

(register-bst-primitive "text.length$" '((string)) '((integer)) 'length)
;; FIXME: TEXT.LENGTH$ counts all characters except for braces; if a
;; backslash occurs at bracelevel = 1, consume all characters until
;; bracelevel = 0, and count everything as one character.

(register-bst-primitive "text.prefix$" '((string) (integer)) '((string)) 'bibtex-string-prefix)

(define-bst-primitive "top$" ((object t)) ()
  :interpreted (format *error-output* "~A~%" object)
  :compiled `(format *error-output* "~A~%" ,object)
  :side-effects-p t)

(define-bst-primitive "type$" () ((string))
  :interpreted (gethash "ENTRY-TYPE" *bib-entry* "")
  :compiled `(gethash "ENTRY-TYPE" *bib-entry* ""))

(define-bst-primitive "warning$" ((warning (string))) ()
  :interpreted (bib-warn warning)
  :compiled (if (and (consp warning)
		     (not (mismatch warning '(concatenate (quote string))
				    :end1 2 :test 'equal)))
		`(bib-warn* ,@(cddr warning))
		`(bib-warn* ,warning))
  :side-effects-p t)

#|(mismatch '(1 2 3) '(1 2) :end1 2)|#

(define-bst-primitive "while$" ((predicate (symbol body)) (body (symbol body))) ()
  :interpreted
  (do ()
      ((not (bst-execute-stack-literal/pop predicate '(boolean))))
    (bst-execute-stack-literal body)))
	
(register-bst-primitive "width$" '((string)) '((integer)) 'bibtex-string-width)

(define-bst-primitive "write$" ((s (string))) ()
  :interpreted (bbl-print s)
  :compiled `(bbl-print ,s)
  :side-effects-p t)

;;; The following three functions are not defined in the original
;;; BibTeX but in all style files.  We define them here because we
;;; can't infer that their result is `boolean' rather than `integer'.

;;; FIXME: BST's and, or functions don't shortcut.  So introduce
;;; bindings when the subforms have side-effects...

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
  :compiled (build-not-form a)
  :ignore-redefinition-p t)

;;; These two occur in amsxtra.bst.

(define-bst-primitive "false" () ((boolean))
  :interpreted (values nil)
  :compiled (values nil)
  :ignore-redefinition-p t)

(define-bst-primitive "true" () ((boolean))
  :interpreted t
  :compiled t
  :ignore-redefinition-p t)

(register-bst-entry "SORT.KEY$" 'str-entry-var '(string) "" *builtin-bst-functions*)
(register-bst-entry "CROSSREF" 'field '(string missing) nil *builtin-bst-functions*)

(register-bst-global-var "ENTRY.MAX$" 'most-positive-fixnum 'int-global-var '(integer)
			 most-positive-fixnum *builtin-bst-functions*
			 :constant-p t)
(register-bst-global-var "GLOBAL.MAX$" 'most-positive-fixnum 'int-global-var '(integer)
			 most-positive-fixnum *builtin-bst-functions*
			 :constant-p t)

