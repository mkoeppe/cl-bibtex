;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; A BibTeX re-implementation in Common Lisp
;;; Author: Matthias Koeppe
;;; this bibtex asd file by Hans Halvorson <hhalvors@princeton.edu>
;;; This is free software, licensed under GPL v2 (see file COPYING)

(defpackage :bibtex-system (:use :cl :asdf))
(in-package :bibtex-system)

(defsystem bibtex
  :version "0.4"  
  :components ((:file "packages")
	       (:file "kpathsea" :depends-on ("packages"))
	       (:file "bibtex-runtime" :depends-on ("packages"))
	       (:file "lisp-form-builder" :depends-on ("packages"))
	       (:file "bst-functions" :depends-on ("packages" "lisp-form-builder"))
	       (:file "interpreter" :depends-on ("bst-functions"))
	       (:file "bibtex-compiler" :depends-on ("lisp-form-builder" "bst-functions"))
	       (:file "bst-reader" :depends-on ("interpreter" "bst-functions"
						"bibtex-compiler" "bibtex-runtime"))
	       (:file "bst-builtins" :depends-on ("bst-functions" "interpreter"
						  "bibtex-runtime"))
	       (:file "bibtex" :depends-on ("bibtex-compiler" "bst-reader"))))
