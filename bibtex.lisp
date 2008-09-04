;; A BibTeX re-implementation in Common Lisp - the BibTeX program
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

(defvar +version+ "1.0+cvs")

;;; The compiler front-end

(defvar *bibtex-pprint-dispatch*
  (let ((pprint-dispatch (copy-pprint-dispatch)))
    #-clisp				; CLISP says "Lisp stack overflow. RESET"
    (set-pprint-dispatch '(cons (member DEFINE-BIBTEX-STYLE))
			 (lambda (*standard-output* obj)
			   (pprint-logical-block (*standard-output* obj :prefix "(" :suffix ")")
			     (write (pprint-pop))
			     (write-char #\Space)
			     (pprint-newline :miser)
			     (pprint-indent :current 0)
			     (write (pprint-pop))
			     (pprint-indent :block 1)
			     (pprint-newline :mandatory)
			     (write (pprint-pop))
			     (loop (pprint-exit-if-list-exhausted)
				   (write-char #\Space)
				   (pprint-newline :linear)
				   (write (pprint-pop)))))
			 0
			 pprint-dispatch)
    pprint-dispatch))

(defun make-entry-type-function-alist ()
  (loop for fun being each hash-value in *bst-functions*
        when (and (member (bst-function-type fun)
                          '(wizard-defined compiled-wiz-defined))
                  (null (bst-function-argument-types fun))
                  (null (bst-function-result-types fun))
		  (side-effects-side-effects-p
		   (bst-function-side-effects fun)))
	collect (cons (bst-function-name fun)
		      (bst-function-lisp-name fun))))

(defun make-macro-set-forms ()
  (loop for macro being each hash-key in *bib-macros*
	and value being each hash-value in *bib-macros*
	nconcing `((gethash ,macro *bib-macros*) ,value) into setf-args
	finally (return (if (null setf-args)
			    '()
			    `((setf ,@setf-args))))))		   

(defun compile-bst-file (bst-file lisp-file &key (make-variables-lexical t)
				  (make-variables-constant t))
  "Compile the BibTeX style file BST-FILE to a Common Lisp BibTeX
style file LISP-FILE.  If :MAKE-VARIABLES-LEXICAL or
:MAKE-VARIABLES-CONSTANT is true (the default), make a second compiler
pass, where some variables are turned into lexical variables or
constant variables."
  (with-open-file (bst-stream bst-file)
    (let* ((package-name (concatenate 'string "BIBTEX-STYLE-"
				      (string-upcase 
				       (pathname-name bst-file))))
	   (use-list '("COMMON-LISP" "BIBTEX-RUNTIME" "BIBTEX-COMPILER"))
	   (temp-package-name (gentemp "BIBTEX-STYLE-"))
	   temp-package)
      (unwind-protect
	   (progn
	     (setq temp-package (make-package temp-package-name :use use-list))
	     (let* ((*bst-package* temp-package)
		    (*bib-entries-symbol* (bst-intern "BIB-ENTRIES"))
		    (*bib-macros* (make-hash-table :test #'equalp))
		    (*bst-compiling* t)
		    (*main-lisp-body* '())
		    (*bst-definition-sequence* '())
		    (*bst-function-call-sequence* '())
		    (*bst-functions* (builtin-bst-functions)))
	       (get-bst-commands-and-process bst-stream)
	       (let* ((constants (and make-variables-constant
				      (make-some-variables-constant)))
		      (*lexicals* (and make-variables-lexical
				       (make-some-variables-lexical))))
		 (when constants
		   (format *error-output* "~&Making variables constant: ~{ ~S~}~%"
			   constants))
		 (when *lexicals*
		   (format *error-output* "~&Making variables lexical: ~{ ~S~}~%"
			   *lexicals*))
		 (when (or *lexicals* constants)
		   ;; Recompile with lexical and constant variables
		   (dolist (bst-function (reverse *bst-definition-sequence*))
		     (when (and (bst-function-p bst-function)
				(eq (bst-function-type bst-function)
				    'compiled-wiz-defined))
		       (compile-bst-function bst-function))))
		 (with-open-file (lisp-stream lisp-file :direction :output)
		   (flet ((lisp-write (arg)
			    (let ((*print-case* :downcase)
				  (*print-pprint-dispatch* *bibtex-pprint-dispatch*)
				  (*package* *bst-package*))
			      (pprint arg lisp-stream))
			    (terpri lisp-stream)))
		     (format lisp-stream
			     ";;;; This is a -*- Common-Lisp -*- program, automatically translated~%~
                              ~&;;;; from the BibTeX style file `~A'~%~
                              ~&;;;; by the CL-BibTeX compiler (version ~A).~%"
			     (namestring bst-file) +version+)
		     (lisp-write
		      `(defpackage ,package-name
			 (:use ,@use-list)
			 (:shadow
			  ,@(sort (mapcar #'copy-symbol
					  (package-shadowing-symbols
					   *bst-package*))
				  #'string-lessp :key #'symbol-name))))
		     (lisp-write `(in-package ,package-name))
		     (dolist (item (reverse *bst-definition-sequence*))
		       (etypecase item
			 (string	; a comment
			  (princ item lisp-stream))
			 (bst-function ; a variable or wizard-defined function
			  (case (bst-function-type item)
			    ((int-global-var str-global-var)
			     (cond
			      ((bst-function-lexical-p item)
			       nil)
			      ((bst-function-constant-p item)
			       (lisp-write `(defconstant ,(bst-function-lisp-name item)
					      ,(bst-function-assigned-value-form item))))
			      (t
			       (lisp-write `(defvar ,(bst-function-lisp-name item)
					      ,(bst-function-value item))))))
			    (compiled-wiz-defined
			     (print-bst-function-info item lisp-stream)
			     (lisp-write (bst-function-defun-form item)))))))
		     (lisp-write `(define-bibtex-style ,(pathname-name bst-file) 
				   (let ((*bib-entry-type-functions*
					  ',(make-entry-type-function-alist))
					 ,*bib-entries-symbol*)
				     ,@(make-macro-set-forms)
				     ,@(reverse *main-lisp-body*)))))))))
	(delete-package temp-package)))))

;;; The BibTeX program

(defvar *bibtex-styles* '()
  "An alist mapping BibTeX styles (strings) to thunk designators that
implement the BibTeX style.  Use REGISTER-BIBTEX-STYLE to put items
here.")

(defvar *allow-load-lisp-bibtex-style* t
  "Non-nil if a Lisp BibTeX style is allowed to be located via
KPSEARCH and loaded into the Lisp image.  (This might be seen as a
security risk, because Lisp programs are much more powerful than BST
scripts.)")

(defvar *registered-bibtex-style* nil)

(defun register-bibtex-style (name thunk)
  "Register a BibTeX style, implemented as THUNK (a function
designator), under NAME."
  (setq *registered-bibtex-style* (cons name thunk))
  (push *registered-bibtex-style* *bibtex-styles*))

(defmacro define-bibtex-style (name &body body)
  (let ((function-name
	 (gentemp (concatenate 'string "BIBTEX-STYLE-" (string name)))))
    `(progn (defun ,function-name () ,@body)
      (register-bibtex-style ',name ',function-name))))      

(defun interpreted-bibtex-style (bst-file)
  "Return a thunk that implements the BibTeX style of the BST-FILE
by running the BST interpreter."
  (lambda ()
    (with-open-file (bst-stream bst-file :if-does-not-exist nil)
      (unless bst-stream
	(bib-fatal "I couldn't open style file `~A'" bst-file))
      (let ((*literal-stack* nil))
	(get-bst-commands-and-process bst-stream)))))

(defun lisp-bibtex-style (lbst-file)
  "Return a thunk that implements the Lisp BibTeX style of LBST-FILE."
  (let ((*registered-bibtex-style* nil))
    (unless (load lbst-file)
      (error "Loading Lisp BibTeX style file `~A' failed."
	     lbst-file))
    (unless *registered-bibtex-style*
      (error "Lisp BibTeX style `~A' failed to register itself." lbst-file))
    (cdr *registered-bibtex-style*)))

(defun find-bibtex-style (style)
  "Find the named BibTeX STYLE.
* First try the styles registered using REGISTER-BIBTEX-STYLE.
* Then, if *ALLOW-LOAD-LISP-BIBTEX-STYLE* is true, try to find and
  load a Lisp BibTeX style file named \"STYLE.lbst\".
* Finally try to find a BibTeX style file named \"STYLE.bst\".
Return a thunk that implements the BibTeX style.  Signal an error
if no style of the requested name has been found."
  (let (it)
    (cond
      ((setq it (assoc style *bibtex-styles* :test #'string-equal))
       (cdr it))
      ((and *allow-load-lisp-bibtex-style*
	    (setq it (kpathsea:find-file
		      (make-pathname :type "lbst" ;;:case :common
				     :defaults style))))
       (lisp-bibtex-style it))
      ((setq it (kpathsea:find-file
		 (make-pathname :type "bst" ;;:case :common
				:defaults style)))
       (format *error-output* "~&The style file: ~A~%" it)
       (interpreted-bibtex-style it))
      (t (error "Could not find a BibTeX style named `~A'." style)))))

(defun bibtex (file-stem &key style)
  "The BibTeX program.  Read citation commands, a list of
bibliographic databases and the name of the bibliography style from
TeX commands in the file `FILE-STEM.aux'.  Find the named bibliography
style via `find-bibtex-style'; it can be overridden programmatically
using the :STYLE argument (a string or a function designator).  Print the
formatted bibliography to the file `FILE-STEM.bbl'.
Return two values, the values of *history* and *err-count*. "
  (let ((*bib-macros* (make-hash-table :test #'equalp))
	(*bib-database* (make-hash-table :test #'equalp))
	(*bib-preamble* "")
	(*bib-entries* ())
	(*bib-files* ())
	(*bibtex-split-initials-already-warned-hashtable* (make-hash-table :test #'equal))
	(*cite-all-entries* nil)
	(*cite-keys* ())
	(*history* +spotless-history+)
	(*err-count* 0)
	(*bib-style* nil)
	(*bst-functions* (builtin-bst-functions)))	
    (read-aux-file (make-pathname :type "aux" ;;:case :common
				  :defaults file-stem))
    (let ((style-function
	   (cond
	     ((not style) (find-bibtex-style *bib-style*))
	     ((functionp style) style)
	     ((symbolp style) (fdefinition style))
	     ((stringp style) (find-bibtex-style style))
	     (t (error "Bad :STYLE argument: ~S" style)))))
      (with-open-file (bbl-output (make-pathname :type "bbl" ;;:case :common
						 :defaults file-stem)
				  :direction :output
				  :if-exists :supersede)
	(with-bbl-output (bbl-output)
	  (funcall style-function))))
    (values *history* *err-count*)))

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

(bibtex "ibm-theory")

(progn
  (setf (ext:default-directory) "/home/mkoeppe/w/iba-papers/")
  (let ((*lexicals* '("NUMNAMES" "NAMESLEFT" "NAMEPTR" "S" "T" "LEN" "MULTIRESULT")))
    (compile-bst-file (kpathsea:find-file "amsalpha.bst")
		      "/tmp/compiled-bst.lisp"))
  (load "/tmp/compiled-bst.lisp" :if-source-newer :compile)
  (cl-bibtex "ibm-theory" 'amsalpha))

(let ((*lexicals* '("NUMNAMES" "NAMESLEFT" "NAMEPTR" "S" "T" "LEN" "MULTIRESULT")))
    (compile-bst-file "/home/mkoeppe/w/diss/diss.bst"
		      "/tmp/compiled-bst.lisp"))

(compile-bst-file "test.bst"
		  "/tmp/compiled-bst.lisp")

|#

