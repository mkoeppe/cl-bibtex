;; Copyright 2001, 2002, 2007 Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de>
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of version 2.1 of the GNU Lesser 
;; General Public License as published by the Free Software 
;; Foundation or any later version, as clarified by the preamble 
;; found in COPYING-preamble.txt. This preamble is in the style
;; of the Franz Inc. preamble at http://opensource.franz.com/preamble.html
;; with names and copyright holders altered accordingly.


(in-package :bibtex-program)

;;; The emulation of the bibtex program

(defun do-emulate-bibtex (command-line-args)
  (let ((*min-crossrefs* 2)
	(*bibtex-split-initials* t)
	(file-stem nil))
    (dolist (arg command-line-args)
      (cond
       ((eql (mismatch "-min-crossrefs=" arg)
	     (length "-min-crossrefs="))
	(setq *min-crossrefs*
	      (parse-integer arg :start (length "-min-crossrefs="))))
       ((string= arg "-no-split-initials")
	(setq *bibtex-split-initials* nil))
       ((string= arg "-split-initials")
	(setq *bibtex-split-initials* t))
       ((string= arg "-terse")
	nil)
       ((and (not (string= arg ""))
	     (char= (char arg 0) #\-))
	(error "Unknown command-line switch: `~A'" arg))
       (t
	(if file-stem
	    (error "Need exactly one file argument; `~A' is extraneous" arg))
	(setq file-stem arg))))
    (unless file-stem
      (error "Need exactly one file argument"))
    (format *error-output*
	    "This is CL-BibTeX, Version ~A~%" bibtex-compiler::+version+)
    (bibtex-compiler:bibtex file-stem)))

(defun quit (&optional code)
  #+abcl (ext:quit code)
  #+allegro (excl:exit code)
  #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
  #+cmu (ext:quit code)
  #+cormanlisp (win32:exitprocess code)
  #+gcl (lisp:bye code)
  #+lispworks (lw:quit :status code)
  #+lucid (lcl:quit code)
  #+sbcl (sb-ext:quit :unix-status
                      (typecase code ((signed-byte 32) code) (null 0) (t 1)))
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'quit code)))

(defun emulate-bibtex (argv)
  ;;(princ "bar") (terpri)
  (let (#+(or cmu sbcl) (*gc-verbose* nil))
    (multiple-value-bind (history err-count)
	(handler-case (do-emulate-bibtex argv)
	  (error (condition)
	    (format *error-output* "~&bibtex: ~A~%"
		    condition)
	    (format *error-output* "~&Try `bibtex --help' for more information.~%")
	    (quit 4)))
      (unless (zerop history)
	(format *error-output* 
		"~&(There ~[were~;was~:;were~] ~:*~D ~[~;warning~;error~;fatal~] message~:*~:P)~%"
		err-count history))
      (quit history))))

