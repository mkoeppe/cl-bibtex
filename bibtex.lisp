;;; A BibTeX re-implementation in Common Lisp - the BibTeX program
;;; Copr. 2001, 2002 Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de>
;;; This is free software, licensed under GNU GPL (see file COPYING)

(in-package bibtex-compiler)

;;; The compiler front-end

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
  
(defun compile-bst-file (bst-file lisp-file)
  (let ((*bib-macros* (make-hash-table))
	(*bst-compiling* t)
	(*main-lisp-body* ())
	(*bst-functions* (builtin-bst-functions)))
    (with-open-file (*lisp-stream* lisp-file :direction :output)
      (with-open-file (bst-stream bst-file)
	(format *lisp-stream*
		";;;; This is a -*- Common-Lisp -*- program, automatically translated~%;;;; from the BibTeX style file `~A'~%;;;; by the CL-BibTeX compiler ($Revision: 1.13 $).~%"
		bst-file)
	(get-bst-commands-and-process bst-stream)
	(lisp-write `(defun ,(intern (string-upcase (pathname-name bst-file))) ()
		      (let ((*bib-entry-type-functions*
			     ',(make-entry-type-function-alist))
			    bib-entries)
			,@(reverse *main-lisp-body*))))))))

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
	(let ((*literal-stack* nil))
	  (get-bst-commands-and-process bst-stream))))))

(defun cl-bibtex (file-stem function)
  (let ((*bib-macros* (make-hash-table))
	(*bib-database* (make-hash-table :test #'equalp))
	(*bib-preamble* "")
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

