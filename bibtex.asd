;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; A BibTeX re-implementation in Common Lisp
;; Author: Matthias Koeppe
;; this bibtex asd file by Hans Halvorson <hhalvors@princeton.edu>
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of version 2.1 of the GNU Lesser 
;; General Public License as published by the Free Software 
;; Foundation or any later version, as clarified by the preamble 
;; found in COPYING-preamble.txt. This preamble is in the style
;; of the Franz Inc. preamble at http://opensource.franz.com/preamble.html
;; with names and copyright holders altered accordingly.

(defpackage :bibtex-system (:use :cl :asdf))
(in-package :bibtex-system)

(defsystem bibtex
  :version "1.1"
  :description "Compatible re-implementation of the BibTeX program, with a BST-to-CL compiler."
  :author "Matthias Koeppe <mkoeppe@math.ucdavis.edu>"
  :licence "LGPL 2.1+"
  :depends-on (#-(or cmu abcl sbcl clisp ecl) :port
		 :split-sequence)
  :components ((:file "cmp")
	       (:file "packages" :depends-on ("cmp"))
	       (:file "kpathsea" :depends-on ("packages"))
	       (:file "bibtex-runtime" :depends-on ("packages"))
	       (:file "lisp-form-builder" :depends-on ("packages"))
	       (:file "bst-functions" :depends-on ("packages" "lisp-form-builder"
							      "bibtex-runtime"))
	       (:file "interpreter" :depends-on ("bst-functions"))
	       (:file "bibtex-compiler" :depends-on ("lisp-form-builder" "bst-functions" 
									 "bibtex-runtime"))
	       (:file "bst-reader" :depends-on ("interpreter" "bst-functions"
						"bibtex-compiler" "bibtex-runtime"))
	       (:file "bst-builtins" :depends-on ("bst-functions" "interpreter"
						  "bibtex-runtime"))
	       (:file "bibtex" :depends-on ("bibtex-compiler" "bst-reader"))
	       (:file "bibtex-program" :depends-on ("bibtex"))))
